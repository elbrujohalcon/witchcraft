-module(wombat_parser).
-export([main/1, parse_logs/2]).

-type event() :: {os:timestamp(), event_type(), status()}.
-type event_type() ::
        experiment | set_as_global | alarm_rasied | change_global_config.
-type status() :: started | completed | {failed, any()}.
-type row() :: {event_type(), completed | {failed, any()}, timer:time()}.

-spec main([string()]) -> _.
main([Input, Output]) -> parse_logs(Input, Output);
main(_) -> io:format("Usage: ~s input output~n", [escript:script_name()]).

-spec parse_logs(file:filename(), file:filename()) -> _.
parse_logs(Input, Output) ->
  {ok, Logs} = file:consult(Input),
  Rows = do_parse_logs(Logs, []),
  ToWrite = [io_lib:format("~p.~n", [Row]) || Row <- Rows],
  file:write_file(Output, ToWrite).

-spec do_parse_logs([event()], [row()]) -> [row()].
do_parse_logs([], Acc) ->
  lists:reverse(Acc);
do_parse_logs([{Time, EventType, started} | Logs], Acc) ->
  do_parse_logs(Logs, [{EventType, started, Time} | Acc]);
do_parse_logs([{Time, EventType, Result} | Logs], Acc) ->
  {EventType, started, StartTime} = lists:keyfind(EventType, 1, Acc),
  Diff = timer:now_diff(Time, StartTime),
  NewAcc = lists:keystore(EventType, 1, Acc, {EventType, Result, Diff}),
  do_parse_logs(Logs, NewAcc).
