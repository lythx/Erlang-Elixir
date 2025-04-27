%%%-------------------------------------------------------------------
%%% @author Szymon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. kwi 2025 17:00
%%%-------------------------------------------------------------------
-module(pingpong).
-author("Szymon").

%% API
-export([start/0, stop/0, play/1, pong_loop/1]).

start() ->
  register(ping, spawn(fun() -> ping_loop(0) end)),
  register(pong, spawn(fun() -> pong_loop(0) end)).

stop() ->
  ping ! stop,
  pong ! stop,
  ok.

play(N) ->
  ping ! {play, N},
  ok.

ping_loop(Sum) ->
  receive
    stop ->
      io:format("Ping stopped~n"),
      ok;
    {play, 0} ->
      io:format("Ping 0~n"),
      timer:sleep(1000),
      ping_loop(Sum);
    {play, N} ->
      io:format("Ping ~B, Suma: ~B~n", [N, Sum + N]),
      pong ! {play, (N - 1)},
      timer:sleep(1000),
      ping_loop(Sum + N)
  after
    20000 ->
      ok
  end.

pong_loop(Sum) ->
    receive
      stop ->
        io:format("Pong stopped~n"),
        ok;
      {play, 0} ->
        io:format("Pong 0~n"),
        timer:sleep(1000),
        pong_loop(Sum);
      {play, N} ->
        io:format("Ping ~B, Suma: ~B~n", [N, Sum + N]),
        ping ! {play, N - 1},
        timer:sleep(1000),
        pong_loop(Sum + N)
    after
      20000 ->
        ok
    end.
