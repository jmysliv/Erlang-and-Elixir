%%%-------------------------------------------------------------------
%%% @author jjmre
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. mar 2020 12:56
%%%-------------------------------------------------------------------
-module(fun_lab2).
-author("jjmre").

%% API
-export([my_map/2, my_filter/2, sum_digits/1, find_numbers/0]).

my_map(_, []) -> [];
my_map(Fun, [Head | Tail]) -> [Fun(Head) | my_map(Fun, Tail)].

my_filter(_, []) -> [];
my_filter(Fun, List) -> [X || X <-List, Fun(X)].

sum_digits(X) ->
  Char_list = integer_to_list(X),
  List_of_digits = [list_to_integer([Char]) || Char <- Char_list],
  Sum = fun(A, B) -> A + B end,
  lists:foldl(Sum, 0, List_of_digits).

find_numbers() ->
  List = qsort:randomElems(10, 1, 1000000),
  Divided_by_3 = fun(X) when X rem 3 == 0 -> true; (_) -> false end,
  Fun = fun(X) ->
    Sum = fun_lab2:sum_digits(X),
    Divided_by_3(Sum) end,
  lists:filter(Fun, List).