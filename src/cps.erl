-module (cps).

-compile(export_all).

-spec run() -> [pos_integer()].
run() ->
  {CPs, LastSieve} =
    lists:foldl(
      fun(N, {Acc, Sieve}) ->
        io:format("Testing ~p's rotations...", [N]),
        case all_primes(N, Sieve) of
          {true, NewSieve} -> io:format("in!~n"), {[N|Acc], NewSieve};
          {false, NewSieve} -> io:format("out!~n"), {Acc, NewSieve}
        end
      end, {[], initial_sieve()}, cands()),
  io:format("Last Sieve: ~p~n", [LastSieve]),
  tl(lists:sort([2 | [number(CP) || CP <- CPs]])).

-spec cands() -> [[1|3|5|7|9]].
cands() ->
  Candidates =
    [lists:dropwhile(fun(X) -> X == 0 end, [X0, X1, X2, X3, X4, X5])
      || X0 <- [0,1,3,5,7,9],
         X1 <- case X0 of
                  0 -> [0,1,3,5,7,9];
                  X0 -> lists:seq(X0, 9, 2)
               end,
         X2 <- case X1 of
                  0 -> [0,1,3,5,7,9];
                  X1 -> lists:seq(case X0 of 0 -> 1; X0 -> X0 end, 9, 2)
               end,
         X3 <- case X2 of
                  0 -> [0,1,3,5,7,9];
                  X2 -> lists:seq(case X0 of 0 -> 1; X0 -> X0 end, 9, 2)
               end,
         X4 <- case X3 of
                  0 -> [0,1,3,5,7,9];
                  X3 -> lists:seq(case X0 of 0 -> 1; X0 -> X0 end, 9, 2)
               end,
         X5 <- case X4 of
                  0 -> [0,1,3,5,7,9];
                  X4 -> lists:seq(case X0 of 0 -> 1; X0 -> X0 end, 9, 2)
               end],
  tl(lists:sort(Candidates)).

all() ->
  tl(lists:sort(
    [lists:dropwhile(fun(X) -> X == 0 end, [X0, X1, X2, X3, X4, X5])
      || X0 <- [0,1,3,5,7,9],
         X1 <- case X0 of
                  0 -> [0,1,3,5,7,9];
                  X0 -> [1,3,5,7,9]
               end,
         X2 <- case X1 of
                  0 -> [0,1,3,5,7,9];
                  X1 -> [1,3,5,7,9]
               end,
         X3 <- case X2 of
                  0 -> [0,1,3,5,7,9];
                  X2 -> [1,3,5,7,9]
               end,
         X4 <- case X3 of
                  0 -> [0,1,3,5,7,9];
                  X3 -> [1,3,5,7,9]
               end,
         X5 <- case X4 of
                  0 -> [0,1,3,5,7,9];
                  X4 -> [1,3,5,7,9]
               end])).

rot(Candidates) ->
  lists:sort(
    lists:append(
      [ case rots(O) of
          [O|_] = All -> All;
          _ -> []
        end
       || O <- Candidates])).

rots(N) ->
  L = length(N),
  sets:to_list(
    sets:from_list(
      lists:map(
        fun(I) ->
          lists:sublist(N,I,L) ++ lists:sublist(N,1,I-1)
        end, lists:seq(1, L)))).


tst() ->
  {TAll, All} = timer:tc(?MODULE, all, []),
  io:format("All in ~p secs. ", [TAll/1000000]),
  {TCan, Can} = timer:tc(?MODULE, cands, []),
  io:format("Cands in ~p secs. ", [TCan/1000000]),
  {TRot, Rot} = timer:tc(?MODULE, rot, [Can]),
  io:format("Rot in ~p secs. ", [TRot/1000000]),
  {TLen, {LAll, LRot}} = timer:tc(fun() -> {length(All), length(Rot)} end),
  io:format("Len in ~p secs. ", [TLen/1000000]),
  {TDiff, Diff} = timer:tc(fun() -> remove(All, Rot) end),
  io:format("Diff in ~p secs. ", [TDiff/1000000]),
  {LAll, length(Can), LRot, LRot - LAll, Diff}.

remove(Xs, Ys) -> remove(Xs, Ys, []).

remove(_Xs, [], Rs) -> Rs;
remove([], _Ys, Rs) -> Rs;
remove([X|Xs], [X|Ys], Rs) -> remove(Xs, Ys, Rs);
remove([X|Xs], [Y|Ys], Rs) when Y > X -> remove(Xs, [Y|Ys], [X|Rs]);
remove([X|Xs], [Y|Ys], Rs) when Y < X -> remove([X|Xs], Ys, Rs).

initial_sieve() -> {10, [7,5,3,2]}.

fill_sieve(N, {Limit, _} = S) when Limit >= N -> S;
fill_sieve(N, {Limit, Primes}) ->
  NewLimit = Limit + 1,
  NewPrimes =
    case lists:any(fun(P) -> NewLimit rem P == 0 end, Primes) of
      true -> Primes;
      false -> [NewLimit|Primes]
    end,
  fill_sieve(N, {NewLimit, NewPrimes}).

all_primes(N, Sieve) ->
  do_all_primes(rots(N), Sieve).

do_all_primes([], Sieve) -> {true, Sieve};
do_all_primes([R|Rots], Sieve) ->
  case is_prime(R, Sieve) of
    {true, NewSieve} -> do_all_primes(Rots, NewSieve);
    {false, NewSieve} -> {false, NewSieve}
  end.

is_prime(N, Sieve) ->
  NN = number(N),
  S = trunc(math:sqrt(NN)),
  NewSieve = {_, Primes} = fill_sieve(S, Sieve),
  {lists:all(fun(P) -> P > S orelse NN rem P /= 0 end, Primes), NewSieve}.

number(N) ->
  {NN, _} =
    lists:foldr(
      fun(D, {Acc, X}) ->
        {Acc + D * X, X * 10}
      end, {0, 1}, N),
  NN.
