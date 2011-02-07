-module(event_test).

-behaviour(gen_event).

-export([run/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name :: atom()}).

-define(PAUSE, 1000).

%% ====================================================================
%% External functions
%% ====================================================================
-spec run() -> ok.
run() ->
  io:format("We start a publisher and three subscribers...~n"),
  {ok, Publisher} = gen_event:start_link({local, ?MODULE}),
  io:format("Publisher running on ~p~n", [Publisher]),
  Sub1 = spawn(fun() -> add_subscriber(sub1) end),
  io:format("sub1 subscriber running on ~p~n", [Sub1]),
  Sub2 = spawn(fun() -> add_subscriber(sub2) end),
  io:format("sub2 subscriber running on ~p~n", [Sub2]),
  Sub3 = spawn(fun() -> add_subscriber(sub3) end),
  io:format("sub3 subscriber running on ~p~n", [Sub3]),
  timer:sleep(?PAUSE),
  io:format("Now you should see three event receptions...~n", []),
  ok = gen_event:notify(?MODULE, {local_time, calendar:local_time()}),
  timer:sleep(?PAUSE),
  io:format("Now we shut down sub3, so according to the docs ?MODULE:terminate/2 will be called for sub3"
           " with {stop,normal} as argument...~n"),
  Sub3 ! stop,
  timer:sleep(?PAUSE),
  io:format("What happened? Each other subscriber received an info message?~n"
           "\tThat's because they're linked to the publisher since they were added using gen_event:add_sup_handler/3~n"
           "\tThat's not on the erlang docs...~n"),
  timer:sleep(?PAUSE),
  io:format("Now we'll unsubscribe sub2 without actually stopping it, that should result in sub2 process receiving a gen_event_EXIT message...~n"),
  Sub2 ! unsubscribe,
  timer:sleep(?PAUSE),
  io:format("We would expect the publisher to be unlinked from sub2, so we stop sub2 hoping that nothing happens...~n"),
  Sub2 ! stop,
  timer:sleep(?PAUSE),
  io:format("But... something actually happened! sub1 received an info message telling him that sub2 exited~n"
           "\tThat means that even when sub2's handler was deleted, sub2 process is still linked to the publisher~n"),
  
  timer:sleep(?PAUSE),
  io:format("Cleaning up...~n"),
  gen_event:stop(?MODULE),
  Sub1 ! stop,
  ok.

%% ====================================================================
%% Server functions
%% ====================================================================
init(Name) -> {ok, #state{name = Name}}.

handle_event(Event, State) ->
  io:format("~p: Event received: ~p~n", [State#state.name, Event]),
  {ok, State}.

handle_call(Request, State) ->
  io:format("~p: Call received: ~p~n", [State#state.name, Request]),
  {ok, ok, State}.

handle_info(Info, State) ->
  io:format("~p: Info received: ~p~n", [State#state.name, Info]),
  {ok, State}.

terminate(Reason, State) ->
  io:format("~p: Terminate: ~p~n", [State#state.name, Reason]),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
add_subscriber(Name) ->
  ok = gen_event:add_sup_handler(?MODULE, {?MODULE, Name}, Name),
  loop(Name).

loop(Name) ->
  receive
    unsubscribe ->
      gen_event:delete_handler(?MODULE, {?MODULE, Name}, {unsubscribed, Name}),
      unlink(erlang:whereis(?MODULE)),
      loop(Name);
    stop -> ok;
    {gen_event_EXIT, {event_test,Name}, Reason} ->
      io:format("~p: Handler removed: ~p~n", [Name, Reason]),
      loop(Name);
    Other ->
      io:format("~p: Unexpected message: ~p~n", [Name, Other]),
      loop(Name)
  end.