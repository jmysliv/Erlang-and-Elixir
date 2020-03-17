%%%-------------------------------------------------------------------
%%% @author jjmre
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. mar 2020 11:39
%%%-------------------------------------------------------------------
-module(qsort).
-author("jjmre").

%% API
-export([ lessThan/2, grtEqThan/2, qs/1, randomElems/3,  compareSpeeds/3]).

lessThan(List, Arg) -> [X || X <- List, X<Arg].

grtEqThan(List, Arg) -> [X || X <- List, X>=Arg].

qs([]) -> [];
qs([Pivot|Tail]) -> qs( lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot) ).

randomElems(N,Min,Max)-> [Min + rand:uniform(Max-Min) || X <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) ->
  {D1, _} = timer:tc(Fun1, List),
  {D2, _} = timer:tc(Fun2, List),
  io:format("czas wykonania Fun1: ~p,Czas wykonania Fun2:  ~p", [D1, D2]).