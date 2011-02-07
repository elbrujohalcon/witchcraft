-module(user_default).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-export([whoami/0, gquery/1, hquery/1]).

-spec whoami() -> string().
whoami() -> os:cmd("whoami").

-spec gquery(string()) -> [binary()].
gquery(Keyword) -> gquery(Keyword, 0).

-spec gquery(string(), integer()) -> [binary()].
gquery(Keyword, Page) ->
  Url = "https://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=" ++ Keyword ++
          "&num=100&rsz=8&safe=off&start=" ++ integer_to_list(Page),
  io:format("G ~s~n", [Url]),
  {ok, "200", _Headers, Data} =
    ibrowse:send_req(Url, [], get, [], []),
  {struct, MainResultProps} = mochijson2:decode(Data),
  {struct, ResponseProps} = proplists:get_value(<<"responseData">>, MainResultProps),
  PageResults =
    lists:map(
      fun({struct, ResultProps}) ->
              proplists:get_value(<<"url">>, ResultProps)
      end, proplists:get_value(<<"results">>, ResponseProps)),
  {struct, CursorProps} = proplists:get_value(<<"cursor">>, ResponseProps),
  NextPages =
    lists:filter(fun({struct, PageProps}) ->
                         proplists:get_value(<<"label">>, PageProps, 0) > Page
                 end, proplists:get_value(<<"pages">>, CursorProps, [])),
  case NextPages of
    [{struct, NextPageProps} | _Rest] ->
      case proplists:get_value(<<"label">>, NextPageProps) of
        X when X < 5 ->
          PageResults ++ gquery(Keyword, X);
        _ ->
          PageResults
      end;
    _ ->
      PageResults
  end.

-spec hquery(string()) -> [binary()].
hquery(Keyword) ->
  Url = "http://is4.heroku.com/run2?url=http://www.google.com/search?q=" ++ Keyword ++
          "%26num=100%26safe=off",
  io:format("H ~s~n", [Url]),
  {ok, "200", _Headers, Data} =
    ibrowse:send_req(Url, [], get, [], []),
  lists:map(fun list_to_binary/1, string:tokens(Data, "\n")).