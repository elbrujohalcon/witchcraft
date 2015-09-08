-module(race).

-export([test/0]).

%% Exercise

test() ->
	[] = race(100, []),
	[bolt, bruce, brujo] = race(100, [{bolt, 12.4}, {bruce, 8.3}, {brujo, 6}]).

run(Runner, Meters, SpeedInMS) ->
	run(Runner, Meters, 0, SpeedInMS).
run(Runner, Meters, Distance, _SpeedInMS) when Distance >= Meters ->
	io:format("~p arrived!~n", [Runner]),
	done;
run(Runner, Meters, Distance, SpeedInMS) ->
	io:format("~p at ~p meters~n", [Runner, Distance]),
	timer:sleep(1000),
	run(Runner, Meters, Distance + SpeedInMS, SpeedInMS).

%% Solution

race(Meters, Runners) ->
	Self = self(),
	lists:foreach(
		fun({Runner, SpeedInMS}) ->
			spawn_run(Runner, Meters, SpeedInMS, Self)
		end, Runners),
	finish_line(Runners, []).

spawn_run(Runner, Meters, SpeedInMS, Self) ->
	spawn_link(
		fun() ->
			run(Runner, Meters, SpeedInMS),
			Self ! Runner
		end).

finish_line([], Acc) -> lists:reverse(Acc);
finish_line(Runners, Acc) ->
	io:format("Wating for ~p~n", [Runners]),
	receive
		Runner ->
			finish_line(
				lists:keydelete(Runner, 1, Runners), [Runner|Acc])
	end.