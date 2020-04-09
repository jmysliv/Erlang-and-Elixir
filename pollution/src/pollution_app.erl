%%%-------------------------------------------------------------------
%%% @author jjmre
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. kwi 2020 22:26
%%%-------------------------------------------------------------------
-module(pollution_app).
-author("jjmre").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  pollution_supervisor:start_link().

stop(_State) ->
  ok.

%%%===================================================================
