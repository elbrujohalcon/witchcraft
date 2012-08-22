-module(g).
-author('elbrujohalcon@inaka.net').

-behaviour(gen_server).

-record(state, {i :: pos_integer(), n :: pos_integer()}).

%% api
-export([start/1, count/1, start_many/2, count_many/2, avg_time/2, avg_times/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec start(pos_integer()) -> [{ok, pid()}].
start(N) ->
  try ets:delete(wpool) catch _Class:_Error -> noop end,
  ets:new(wpool, [public, named_table, ordered_set, {read_concurrency, true}]),
  ets:insert(wpool, {'g-count', N}),
  [gen_server:start({local, list_to_atom(?MODULE_STRING ++ integer_to_list(I))}, ?MODULE, {I, N}, []) || I <- lists:seq(1, N)].

-spec count(ask | test | ets) -> {pos_integer(), pos_integer()}.
count(ask) ->
  timer:tc(fun() -> gen_server:call(g1, count) end);
count(ets) ->
  timer:tc(fun() -> [{'g-count', N}|_] = ets:lookup(wpool, 'g-count'), N end);
count(test) ->
  timer:tc(fun() -> count(?MODULE_STRING, 1) end).

count(Sup, N) ->
  case erlang:whereis(list_to_atom(Sup ++ integer_to_list(N))) of
      undefined -> N - 1;
      _Pid -> count(Sup, N + 1)
  end.

-spec count_many(pos_integer(), ask | test | ets) -> [pid()].
count_many(N, Type) ->
  [spawn(fun() -> timer:sleep(N), io:format("~p (~p): ~p~n", [Type, I, count(Type)]) end) || I <- lists:seq(1, N)].

-spec launch_many(pos_integer(), ask | test | ets) -> [pid()].
launch_many(N, Type) ->
  [spawn(fun() -> receive start -> io:format("~p (~p): ~p~n", [Type, I, count(Type)]) end end) || I <- lists:seq(1, N)].

-spec start_many(pos_integer(), ask | test | ets) -> ok.
start_many(N, Type) ->
  [Pid ! start || Pid <- launch_many(N, Type)].

-spec avg_time(pos_integer(), ask | test | ets) -> float().
avg_time(N, Type) ->
  Self = self(),
  [Pid ! start || Pid <- [spawn(fun() -> receive start -> Self ! count(Type) end end) || _ <- lists:seq(1,N)]],
  Rs = [receive {T, _} -> T end || _ <- lists:seq(1,N)],
  lists:sum(Rs) / length(Rs).

-spec avg_times(pos_integer()) -> {ETS::float(), Test::float(), Ask::float()}.
avg_times(N) -> {avg_time(N, ets), avg_time(N, test), avg_time(N, ask)}.

%%%===================================================================
%%% init, terminate, code_change, info callbacks
%%%===================================================================
-spec init({pos_integer(), pos_integer()}) -> {ok, #state{}}.
init({I, N}) -> {ok, #state{i = I, n = N}}.

-spec terminate(atom(), #state{}) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(string(), #state{}, any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Cast, State) -> {noreply, State}.

-type from() :: {pid(), reference()}.
-spec handle_call(term(), from(), #state{}) -> {reply, ok, #state{}}.
handle_call(count, _From, State) -> {reply, State#state.n, State}.

-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) -> {noreply, State}.