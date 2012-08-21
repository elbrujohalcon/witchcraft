-module(g).
-author('elbrujohalcon@inaka.net').

-behaviour(gen_server).

-record(state, {i :: pos_integer(), n :: pos_integer()}).

%% api
-export([start/1, count/1, count_many/2, avg_time/2]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec start(pos_integer()) -> [{ok, pid()}].
start(N) -> [gen_server:start({local, list_to_atom(?MODULE_STRING ++ integer_to_list(I))}, ?MODULE, {I, N}, []) || I <- lists:seq(1, N)].

-spec count(ask | test) -> {pos_integer(), pos_integer()}.
count(ask) ->
  P = list_to_atom(?MODULE_STRING "1"),
  timer:tc(fun() -> gen_server:call(P, count) end);
count(test) ->
  timer:tc(fun() -> count(?MODULE_STRING, 1) end).

count(Sup, N) ->
    case erlang:whereis(list_to_atom(Sup ++ integer_to_list(N))) of
        undefined -> N - 1;
        _Pid -> count(Sup, N + 1)
    end.

-spec count_many(pos_integer(), ask | test) -> ok.
count_many(N, Type) ->
  [spawn(fun() -> timer:sleep(N), io:format("~p (~p): ~p~n", [Type, I, count(Type)]) end) || I <- lists:seq(1, N)].

-spec avg_time(pos_integer(), ask | test) -> float().
avg_time(N, Type) ->
  Self = self(),
  [spawn(fun() -> timer:sleep(N), Self ! count(Type) end) || _ <- lists:seq(1, N)],
  Rs = [receive {T, _} -> T end || _ <- lists:seq(1, N)],
  lists:sum(Rs) / length(Rs).

%%%===================================================================
%%% init, terminate, code_change, info callbacks
%%%===================================================================
-spec init({pos_integer(), pos_integer()}) -> {ok, #state{}}.
init({I, N}) -> {ok, #state{i = I, n = N}}.

-spec terminate(atom(), #state{}) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(string(), #state{}, any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Cast, State) -> {noreply, State}.

-type from() :: {pid(), reference()}.
-spec handle_call(term(), from(), #state{}) -> {reply, ok, #state{}}.
handle_call(count, _From, State) -> {reply, State#state.n, State}.

-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) -> {noreply, State}.