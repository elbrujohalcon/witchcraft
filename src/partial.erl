-module(partial).

-export([app/2]).

app(F, Args) ->
  {arity, InitialArity} = erlang:fun_info(F, arity),
  case length(Args) of
    L when L < InitialArity ->
      MissingArgs = [{var, 1, N} || N <- lists:seq(1, InitialArity - L)],
      ArgList = [case is_function(A) of
                   false -> erl_parse:abstract(A);
                   true  -> {var, 1, erlang:fun_to_list(A)}
                 end || A <- Args] ++ MissingArgs,
      Parsed = [{'fun', 1,
                 {clauses, [{clause, 1, MissingArgs, [],
                             [{call, 1, {var, 1, 'F'}, ArgList}]}]}}],
      {value, R, _} = erl_eval:exprs(Parsed, [{'F', F}] ++
                                              [{erlang:fun_to_list(A), A} ||
                                               A <- Args, is_function(A)]),
      R
  end.
