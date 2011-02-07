%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc  Queuing utility for Twitter Searchs
%%% @end
%%%-------------------------------------------------------------------
-module(twitter_searcher).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-define(SEARCH_URL, "http://search.twitter.com/search.json").
-define(MAX_RETRIES, 5).

-behaviour(gen_server).

-record(state, {queue = queue:new()       :: queue(),
                retry = 0                 :: non_neg_integer(),
                refresh_urls = dict:new() :: dict()}).
-opaque state() :: #state{}.

-export([start_link/0, search/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% =================================================================================================
%% External functions
%% =================================================================================================
%% @spec start_link() -> {ok, pid()}
%% @hidden
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec search(Keyword::binary()) -> [term()]
%% @doc  Searches twitter for a keyword
-spec search(binary()) -> [term()].
search(Keyword) ->
  gen_server:call(?MODULE, Keyword, infinity).

%% =================================================================================================
%% Server functions
%% =================================================================================================
%% @hidden
-spec init([]) -> {ok, state()}.
init([]) ->
  {ok, _TimerRef} = timer:send_after(inaka:get_env(twitter_search_frequency), handle_req),
  {ok, #state{queue         = queue:new(),
              refresh_urls  = dict:new()}}.

%% @hidden
-spec handle_call(string(), reference(), state()) -> {noreply, state()}.
handle_call(Request, From, State = #state{queue = Q}) ->
  case queue:is_empty(Q) of
    true ->
      {ok, _TimerRef} = timer:send_after(inaka:get_env(twitter_search_frequency), handle_req);
    false ->
      ok
  end,
  {noreply, State#state{queue = queue:in({From, Request}, Q)}}.

%% @hidden
-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast(_, State) -> {noreply, State}.

%% @hidden
-spec handle_info(handle_req, state()) -> {noreply, state(), hibernate}.
handle_info(handle_req, State = #state{queue = Q,
                                       refresh_urls = Urls,
                                       retry = Retry}) ->
  case queue:out(Q) of
    {empty, Q} ->
      {noreply, State#state{retry = 0}, hibernate};
    {{value, {From = {To, _Tag}, Trend}}, NewQ} ->
      Url = ?SEARCH_URL ++
              case dict:find({To, Trend}, Urls) of
                error ->
                  [$? | mochiweb_util:urlencode(
                     [{q,            Trend},
                      {lang,         inaka:get_env(twitter_lang)},
                      {rpp,          inaka:get_env(twitter_rpp)},
                      {result_type,  inaka:get_env(twitter_result_type)}
                     ])];
                {ok, RUrl} ->
                  [RUrl, $&, mochiweb_util:urlencode(
                     [{lang,         inaka:get_env(twitter_lang)},
                      {rpp,          inaka:get_env(twitter_rpp)},
                      {result_type,  inaka:get_env(twitter_result_type)}
                     ])]
              end,
      try ibrowse:send_req(Url, [], get, [], []) of
        {ok, "200", _Headers, Body} ->
          RefreshUrl = 
            try couchbeam_mochijson2:decode(Body) of
              {Json} ->
                Msgs = proplists:get_value(<<"results">>, Json),
                gen_server:reply(From, Msgs),
                proplists:get_value(<<"refresh_url">>, Json, Url)
            catch
              _:Error ->
                gen_server:reply(From, []),
                Url
            end,
          {ok, _TimerRef} = timer:send_after(inaka:get_env(twitter_search_frequency), handle_req),
          {noreply, State#state{queue = NewQ,
                                retry = 0,
                                refresh_urls = dict:store({To, Trend}, RefreshUrl, Urls)}, hibernate};
        {ok, "420", Headers, Body} ->
          case Retry of
            ?MAX_RETRIES ->
              {ok, _TimerRef} = timer:send_after(inaka:get_env(twitter_search_frequency), handle_req),
              gen_server:reply(From, []),
              {noreply, State#state{queue = NewQ, retry = 0,
                                    refresh_urls = dict:erase({To, Trend}, Urls)}, hibernate};
            Retry ->
              NextTimeout =
                case proplists:get_value("Retry-After", Headers, notset) of
                  notset -> inaka:get_env(twitter_search_frequency);
                  Value -> (list_to_integer(Value) + 1) * 1000
                end,
              {ok, _TimerRef} = timer:send_after(NextTimeout, handle_req),
              {noreply, State#state{retry = Retry + 1,
                                    refresh_urls = dict:erase({To, Trend}, Urls)}, hibernate}
          end;
        {ok, Code, Headers, Body} ->
          {ok, _TimerRef} = timer:send_after(inaka:get_env(twitter_search_frequency), handle_req),
          gen_server:reply(From, []),
          {noreply, State#state{queue = NewQ, retry = 0,
                                refresh_urls = dict:erase({To, Trend}, Urls)}, hibernate};
        Error ->
          {ok, _TimerRef} = timer:send_after(inaka:get_env(twitter_search_frequency), handle_req),
          gen_server:reply(From, []),
          {noreply, State#state{queue = NewQ, retry = 0,
                                refresh_urls = dict:erase({To, Trend}, Urls)}, hibernate}
      catch
        _:timeout -> %% An ibrowse internal process timed out
          gen_server:reply(From, []),
          {noreply, State#state{queue = NewQ, retry = 0,
                                refresh_urls = dict:erase({To, Trend}, Urls)}, hibernate};
        _:{timeout, _} -> %% An ibrowse internal process timed out
          gen_server:reply(From, []),
          {noreply, State#state{queue = NewQ, retry = 0,
                                refresh_urls = dict:erase({To, Trend}, Urls)}, hibernate};
        _:{conn_failed, {error, Error}} ->
          gen_server:reply(From, []),
          {noreply, State#state{queue = NewQ, retry = 0,
                                refresh_urls = dict:erase({To, Trend}, Urls)}, hibernate};
        _:Error ->
          gen_server:reply(From, []),
          {noreply, State#state{queue = NewQ, retry = 0,
                                refresh_urls = dict:erase({To, Trend}, Urls)}, hibernate}
      end
  end;
handle_info(_, State) -> {noreply, State, hibernate}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_, _) -> ok.

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.