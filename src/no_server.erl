-module(no_server).

-export([test/1, try_test/1, trap_exit_test/1]).
-export([init/1]).

-type mode() :: run | fail.

-spec test(mode()) -> {started, pid()} | {not_started, term()}.
test(Mode) ->
  case gen_server:start_link(no_server, Mode, [{debug, [log, trace]}]) of
    {ok, Pid} -> {started, Pid};
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

-spec init(run) -> {ok, state}
        ; (fail) -> {stop, hammertime}.
init(run) -> {ok, state};
init(fail) -> {stop, hammertime}.
