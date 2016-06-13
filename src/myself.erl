%% COMMON ELVISCI COMPLAIN ABOUT THIS VERY VERY EXTREMELY INSANELY LONG LONG LINE THAT I JUST WROTE FOR YOU, OK?
-module(myself).

-export([my_code/0, say/1, say_fun/0, add_say/2, my_bin/0]).

my_code() ->
	io:format("~s~n", [erl_prettypr:format(erl_syntax:form_list(do_my_code()))]).

my_bin() ->
	{?MODULE, Beam, _} = code:get_object_code(?MODULE), Beam.

do_my_code() ->
	{ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(my_bin(),[abstract_code]),
	AC.

say(1) -> 1;
say(2) -> 2.

x() -> <<1,3,4,5>>.

say_fun() ->
	{value, Say, _Rest} = lists:keytake(say, 3, do_my_code()),
	Say.

add_say(Key, Value) ->
	NewCode =
		lists:map(
			fun({function, L, say, 1, Clauses}) ->
				{function, L, say, 1,
					[{clause, L, [{integer, L, Key}], [], [{integer, L, Value}]} |Clauses]};
			   (Form) -> Form
			end, do_my_code()),
	{ok, ?MODULE, Binary} = compile:forms(NewCode),
	FinalCode =
		lists:map(
			fun({function, L, my_bin, 0, _Clauses}) ->
				{function, L, my_bin, 0,
					[{clause, L, [], [],
						[{bin, L,
							[{bin_element, L,
								{string, L, binary_to_list(Binary)}, default, default}]
						 }]}]};
			   (Form) -> Form
			end, NewCode),
	io:format("New Code:~n~s~n----------~n", [erl_prettypr:format(erl_syntax:form_list(FinalCode))]),
	{ok, ?MODULE, FinalBinary} = compile:forms(FinalCode, [debug_info]),
	code:load_binary(?MODULE, "no-file", FinalBinary).
