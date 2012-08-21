-module(parent).

-export([run/0, module/0, a_value/0, print/1]).

run() ->
  io:format("Module: ~p~nA Value: ~p~n", [module(), a_value()]).

module() -> ?MODULE.

a_value() -> parent_value.

print(ChildTuple) ->
	[Name | Fields] = tuple_to_list(ChildTuple),
	FieldNames = Name:attribute_names(),
	io:format("INSERT INTO ~s ~p VALUES ~p~n", [Name, FieldNames, Fields]).