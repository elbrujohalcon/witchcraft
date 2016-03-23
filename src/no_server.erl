-module(no_server).

-export([test/0, test/1, try_test/1, trap_exit_test/1]).
-export([init/1]).

-type mode() :: run | {stop, term()} | ignore | fail.

-spec test() -> ok.
test() ->
  lists:foreach(
    fun run_test/1, [fun test/1, fun try_test/1, fun trap_exit_test/1]).

run_test(Fun) ->
  spawn(
    fun() ->
      io:format("Testing ~p @ ~p…~n", [Fun, self()]),
      {started, _} = Fun(run),
      {not_started, normal} = Fun({stop, normal}),
      ignored = Fun(ignore),
      {not_started, hammertime} = Fun({stop, hammertime}),
      {not_started, {bad_return_value, {no_way, jose}}} = Fun(fail),
      io:format("~p works!~n", [Fun])
    end).

-spec test(mode()) -> {started, integer()} | {not_started, term()} | ignored.
test(Mode) ->
  case gen_server:start_link(no_server, Mode, [{debug, [log, trace]}]) of
    {ok, Pid} -> {started, sys:get_state(Pid)};
    ignore -> ignored;
    {error, Reason} -> {not_started, Reason}
  end.

-spec try_test(mode()) ->
  {started, pid()} | {not_started, term()} | {not_started, atom(), term()}.
try_test(Mode) ->
  try test(Mode)
  catch
    Kind:Error -> {not_started, Kind, Error}
  end.

-spec trap_exit_test(mode()) ->
  {started, pid()} | {not_started, term()} | {not_started, atom(), term()}.
trap_exit_test(Mode) ->
  Before = process_flag(trap_exit, true),
  try test(Mode)
  catch
    Kind:Error -> {not_started, Kind, Error}
  after
    process_flag(trap_exit, Before)
  end.

-spec init(run) -> {ok, integer()}
        ; ({stop, X}) -> {stop, X}
        ; (fail) -> no_return()
        ; (ignore) -> ignore.
init(run) -> {ok, rand:uniform(10)};
init({stop, X}) -> {stop, X};
init(fail) -> throw({no_way, jose});
init(ignore) -> ignore.
