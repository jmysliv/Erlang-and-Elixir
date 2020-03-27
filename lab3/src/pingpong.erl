%%%-------------------------------------------------------------------
%%% @author jjmre
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. mar 2020 12:51
%%%-------------------------------------------------------------------
-module(pingpong).
-author("jjmre").

%% API
-export([start/0, stop/0, play/1, ping_function/1, pong_function/1]).

start() ->
  register(ping, spawn(pingpong, ping_function, [0])),
  register(pong, spawn(pingpong, pong_function, [0])).

stop() ->
  ping ! stop,
  pong ! stop.

play(N) ->
  ping ! {N}.


ping_function(Sum)->
  receive
    {0} -> ping_function(Sum);
    {N} ->
      io:format("Ping odbija pileczke, pozostalo ~p, dotychczasowa suma ~p~s", [N, Sum, "\n"]),
      timer:sleep(2000),
      pong ! {N-1},
      ping_function(Sum + N);
    stop -> stop
    after 20000 -> expired
  end.

pong_function(Sum)->
  receive
    {0} -> pong_function(Sum);
    {N} ->
      io:format("Pong odbija pileczke, pozostalo ~p, dotychczasowa suma ~p~s", [N, Sum, "\n"]),
      timer:sleep(2000),
      ping ! {N-1},
      pong_function(Sum + N);
    stop -> stop
  after 20000 -> expired
  end.
