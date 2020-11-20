-module(ufo).

-export([run/0, start/0, stop/0, actor_loop/1]).

start() ->
  {ok, E} =
    et_viewer:start(
      [ {title, "Aliens!"}
      , {detail_level, max}
      , {trace_global, true}
      , {trace_pattern, {et, max}}
      , {hide_unknown, true}
      ]),
  register(ufo, E),
  spawn_actors(),
  timer:sleep(1000),
  register('UNKNOWN', self()).

run() ->
  send_msg("UNKNOWN", hd(actors()), "any aliens yet?").

stop() ->
  et_viewer:stop(whereis(ufo)),
  kill_actors().

spawn_actors() ->
  lists:foreach(fun spawn_actor/1, actors()).

spawn_actor(Name) ->
  Pid = spawn(ufo, actor_loop, [Name]),
  register(list_to_atom(Name), Pid).

kill_actors() ->
  unregister('UNKNOWN'),
  lists:foreach(fun kill_actor/1, actors()).

kill_actor(Name) ->
  exit(whereis(list_to_atom(Name)), kill).

actor_loop(Name) ->
  internal_actor_loop(Name).

internal_actor_loop(Name) ->
  receive
    {Msg, From} ->
      NextActor = next_actor(Name),
      timer:sleep(rand:uniform(10) * 250),
      send_msg(Name, NextActor, Msg),
      timer:sleep(rand:uniform(10) * 250),
      reply(Name, From, random_answer()),
      internal_actor_loop(Name);
    _Msg ->
      internal_actor_loop(Name)
  end.

random_answer() ->
  Answers =
    [ "let me see"
    , "checking..."
    , "give me a minute, please"
    ],
  lists:nth(rand:uniform(length(Answers)), Answers).

next_actor(Name) ->
  Actors = actors() -- [Name],
  lists:nth(rand:uniform(length(Actors)), Actors).

actors() -> ["Brujo", "Aki", "James", "Dave", "Juan"].

send_msg(From, To, Msg) ->
  et:phone_home(rand:uniform(50), From, To, Msg, #{}),
  list_to_atom(To) ! {Msg, From}.

reply(From, To, Msg) ->
  et:phone_home(50 + rand:uniform(50), From, To, Msg, #{}),
  list_to_atom(To) ! Msg.
