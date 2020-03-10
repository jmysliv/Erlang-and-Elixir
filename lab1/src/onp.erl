%%%-------------------------------------------------------------------
%%% @author jjmre
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. mar 2020 10:17
%%%-------------------------------------------------------------------
-module(onp).
-author("jjmre").

%% API
-export([onp/1]).

parse_to_float(S) ->
  case string:to_float(S) of
    {error,no_float} -> list_to_integer(S) * 1.0;
    {X, _}-> X
  end.

iterate([], [H | []]) -> H;
iterate(["+" | T], [L1, L2 | Rest]) ->
  X = L1 + L2,
  iterate(T, [X | Rest]);
iterate(["-" | T], [L1, L2 | Rest]) ->
  X = L2 - L1,
  iterate(T, [X | Rest]);
iterate(["*" | T], [L1, L2 | Rest]) ->
  X = L1 * L2,
  iterate(T, [X | Rest]);
iterate(["/" | T], [L1, L2 | Rest]) ->
  X = L2 / L1,
  iterate(T, [X | Rest]);
iterate(["sqrt" | T], [L1 | Rest]) ->
  X = math:sqrt(L1),
  iterate(T, [X | Rest]);
iterate(["pow" | T], [L1, L2 | Rest]) ->
  X = math:pow(L1, L2),
  iterate(T, [X | Rest]);
iterate(["sin" | T], [L1 | Rest]) ->
  X = math:sin(L1),
  iterate(T, [X | Rest]);
iterate(["cos" | T], [L1 | Rest]) ->
  X = math:cos(L1),
  iterate(T, [X | Rest]);
iterate([S | T], Rest) ->
  X = parse_to_float(S),
  iterate(T, [X | Rest]).




onp(S) ->
  T = string:tokens(S, " "),
  iterate(T, []).

