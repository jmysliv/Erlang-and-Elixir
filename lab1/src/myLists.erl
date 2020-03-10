%%%-------------------------------------------------------------------
%%% @author jjmre
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. mar 2020 09:48
%%%-------------------------------------------------------------------
-module(myLists).
-author("jjmre").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1, sumFloatsTail/2]).

contains([], _) -> false;
contains([H | _], H) -> true;
contains([_ | T], X) -> contains(T, X).

duplicateElements([]) -> [];
duplicateElements([H | T]) -> [H, H] ++ duplicateElements(T).

sumFloats([]) -> 0;
sumFloats([H | T]) ->
  if
    is_float(H) -> H + sumFloats(T);
    true -> sumFloats(T)
  end.

sumFloatsTail([], Sum) -> Sum;
sumFloatsTail([H | T], Sum) ->
  if
    is_float(H) -> sumFloatsTail(T, Sum + H);
    true -> sumFloatsTail(T, Sum)
  end.

