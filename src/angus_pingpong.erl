%%% File    : pingpong.erl
%%% Author  :  <simon@erlang-consulting.com>, <martin@erlang-consulting.com>
%%% Description : Sends a message N times between two processes
%%% Created : Dec 2005 by  Simon Aurell and Martin Carlson

-module(angus_pingpong).

%% Interface
-export([start/0, stop/0, send/1]).

%% Internal Exports
-export([init_a/0, init_b/0]).

start() ->
    register(a, spawn(pingpong, init_a, [])),
    ok.

stop() ->
    exit(whereis(a), abnormal).

send(N) ->
    a ! {msg, message, N},
    ok.

init_a() ->
    register(b, spawn_link(pingpong, init_b, [])),
    loop_a().

init_b() ->
    loop_b().

loop_a() ->
    receive
        {msg, _Msg, 0} ->
            loop_a();
        {msg, Msg, N} ->
            io:format("ping...~n"),
            timer:sleep(500),
            b ! {msg, Msg, N -1},
            loop_a();
        Any when Any == Any -> loop_a() %flush
    after
        15000 ->
            io:format("Ping got bored, exiting.~n"),
            exit(timeout)
    end.

loop_b() ->
    receive
        {msg, _Msg, 0} ->
            loop_b();
        {msg, Msg, N} ->
            io:format("pong!~n"),
            timer:sleep(500),
            a ! {msg, Msg, N -1},
            loop_b();
        Any when Any == Any -> loop_b() %flush
    after
        15000 ->
            io:format("Pong got bored, exiting.~n"),
            exit(timeout)
    end.
