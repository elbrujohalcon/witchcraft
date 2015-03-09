-module(tested).
-author('elbrujohalcon@gmail.com').

-export([main/0]).

-spec main() -> eof.
main() -> process_lines(hd(get_line_numbers())).

process_lines(0) -> eof;
process_lines(N) ->
  process_line(),
  process_lines(N - 1).

process_line() -> io:format("~p~n", [can_handle(get_line_numbers())]).

can_handle([C, K, W]) when C * W =< K -> yes;
can_handle(_) -> no.

get_line_numbers() -> get_line_numbers(io:get_line("")).
get_line_numbers(eof) -> get_line_numbers({error, premature_eof});
get_line_numbers({error, ErrorDescription}) ->
  io:format(standard_error, "Couldn't read: ~p~n", [ErrorDescription]),
  erlang:halt(1);
get_line_numbers(Line) ->
  [list_to_integer(T) || T <- string:tokens(Line, [$\s, $\n])].
