%%%-------------------------------------------------------------------
%%% @author jjmre
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. mar 2020 16:12
%%%-------------------------------------------------------------------
-module(lab1).
-author("jjmre").

%% API
-export([power/2]).

power( _, 0) -> 1;
power(N, 1) -> N;
power(N, X) -> N * power(N, X-1).


