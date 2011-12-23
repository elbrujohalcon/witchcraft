-module(parent).

-export([run/0, module/0, a_value/0]).

run() ->
  io:format("Module: ~p~nA Value: ~p~n", [module(), a_value()]).

module() -> ?MODULE.

a_value() -> parent_value.