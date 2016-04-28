-module(xe).

-export([test/0, test_two/1]).

test() ->
  ?MODULE_STRING.

test_two(_) ->
 ?MODULE_STRING.
