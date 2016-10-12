-module(git).

-export(['$handle_undefined_function'/2]).

-spec '$handle_undefined_function'(atom(), [any()]) -> any().
'$handle_undefined_function'(Func, Args) ->
  Git = git(),
  GitCommand = atom_to_list(Func),
  GitArgs = [io_lib:format("~p ", [Arg]) || Arg <- Args],
  Command = [Git, $\s, GitCommand, $\s, GitArgs],
  Output = os:cmd(Command),
  io:format("~s~n", [Output]).

git() ->
  case os:find_executable("git") of
    false -> throw(enogit);
    Path -> Path
  end.
