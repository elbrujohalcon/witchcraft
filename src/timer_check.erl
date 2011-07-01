-module(timer_check).

-export([start/0, notify/1, notify/2, stop/0, loop/0, fail/0]).

-spec start() -> true.
start() ->
  P = spawn(?MODULE, loop, []),
  erlang:register(?MODULE, P).

-spec loop() -> ok.
loop() ->
  receive
    stop -> ok;
    {T, X} -> io:format("~p: ~p~n", [calendar:local_time(), X]),
              timer:send_after(T, {T, X});
    X -> io:format("~p: ~p~n", [calendar:local_time(), X])
  end,
  loop().

-spec notify(X) -> X.
notify(X) ->
  ?MODULE ! X.

-spec notify(pos_integer(), X) -> {pos_integer(), X}.
notify(T, X) ->
  ?MODULE ! {T, X}.

-spec fail() -> no_return().
fail() -> timer:sleep(1000).

-spec stop() -> true.
stop() ->
  exit(erlang:whereis(?MODULE), kill).