-module(parfer).

-export([parf/1]).

parf(File) ->
	{ok, Bin} = file:read_file(File),
	{ok, Tokens, _Lines} = erl_scan:string(binary_to_list(Bin)),
	Forms = forms(Tokens),
	[begin
		{ok, P} = erl_parse:parse_form(Form),
		P
	 end || Form <- Forms].


forms(Tokens) -> forms(Tokens,[[]]).
forms([], [[]|Rev]) -> lists:reverse(Rev);
forms([T = {dot, _} | Tokens], [Last | Rest]) -> forms(Tokens, [[], Last ++ [T] | Rest]);
forms([T|Tokens], [Last | Rest]) -> forms(Tokens, [Last ++ [T] | Rest]).