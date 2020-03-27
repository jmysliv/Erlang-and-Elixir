%%%-------------------------------------------------------------------
%%% @author jjmre
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. mar 2020 13:35
%%%-------------------------------------------------------------------
-module(parcellockerfinder).
-author("jjmre").

%% API
-export([set_up/0, findMyParcelLocation2/2, findParcelForEachPerson3/2]).

measureSpeed(Fun, Arguments) ->
  {D, _} = timer:tc(Fun, Arguments),
  D.

calculateDistance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1-Y2, 2)).

findNearest({X1, Y1, _}, []) -> {X1, Y1};
findNearest({}, [{X2, Y2, Distance} | Tail]) -> findNearest({X2, Y2, Distance}, Tail);
findNearest({X1, Y1, Min}, [{X2, Y2, Distance} | Tail]) ->
  case Distance < Min of
    true -> findNearest({X2, Y2, Distance}, Tail);
    _ -> findNearest({X1, Y1, Min}, Tail)
end.


findMyParcelLocation(PersonLocation, LockerLocations) ->
  Distances = [{X2, Y2, calculateDistance(PersonLocation, {X2, Y2})} || {X2, Y2} <- LockerLocations],
  findNearest({}, Distances).

findParcelForEachPerson(PersonLoacations, LockerLocations) ->
  [{PersonLocation, findMyParcelLocation(PersonLocation, LockerLocations)} || PersonLocation <- PersonLoacations].

findMyParcelLocation2(PersonLocation, LockerLocations) ->
  Distances = [{X2, Y2, calculateDistance(PersonLocation, {X2, Y2})} || {X2, Y2} <- LockerLocations],
  {X, Y} = findNearest({}, Distances),
  receive
    {Pid, get} ->
      Pid ! {PersonLocation, {X, Y}}
    after 10000 -> ok
  end.

createProc([], _, PidList) -> PidList;
createProc([PersonLocation | Tail], LockerLocations, PidList) ->
  Pid = spawn(parcellockerfinder, findMyParcelLocation2, [PersonLocation, LockerLocations]),
  createProc(Tail, LockerLocations, [Pid | PidList]).

collect([], Pairs) -> Pairs;
collect([Pid | Tail], Pairs) ->
  Pid ! {self(), get},
  receive
    Pair -> collect(Tail, [Pair | Pairs])
  end.

findParcelForEachPerson2(PersonLocations, LockerLocations) ->
  PidList = createProc(PersonLocations, LockerLocations, []),
  collect(PidList, []).

findParcelForEachPerson3(PersonLocations, LockerLocations) ->
  SubList = [{PersonLocation, findMyParcelLocation(PersonLocation, LockerLocations)} || PersonLocation <- PersonLocations],
  receive
    {Pid, get} ->
      Pid ! SubList
    after 10000 -> ok
  end.

splitListOn4([], A, B, C, D) -> {A, B, C, D};
splitListOn4([H1, H2, H3, H4 | Tail], A, B, C, D) ->
  splitListOn4(Tail, [H1 | A], [H2 | B], [H3 | C], [H4 | D]);
splitListOn4(Rest, A, B, C, D) -> {[Rest | A], B, C, D}.

collect4([], Pairs) -> Pairs;
collect4([Pid | Tail], Pairs) ->
  Pid ! {self(), get},
  receive
    SubList -> collect(Tail, Pairs ++ SubList)
  end.

createAndManage(PersonsLocations, LockerLocations) ->
  {A, B, C, D} = splitListOn4(PersonsLocations, [], [], [], []),
  Pid1 = spawn(parcellockerfinder, findParcelForEachPerson3, [A, LockerLocations]),
  Pid2 = spawn(parcellockerfinder, findParcelForEachPerson3, [B, LockerLocations]),
  Pid3 = spawn(parcellockerfinder, findParcelForEachPerson3, [C, LockerLocations]),
  Pid4 = spawn(parcellockerfinder, findParcelForEachPerson3, [D, LockerLocations]),
  collect4([Pid1, Pid2, Pid3, Pid4], []).


set_up() ->
  PersonsLocations = [{rand:uniform(10000), rand:uniform(10000)} || X <- lists:seq(1, 10000)],
  ParcelsLocations = [{rand:uniform(10000), rand:uniform(10000)} || X <- lists:seq(1, 1000)],
  Fun = fun(A, B) -> findParcelForEachPerson(A, B) end,
  T1 = measureSpeed(Fun, [PersonsLocations, ParcelsLocations]),
  Fun2 = fun(A, B) -> findParcelForEachPerson2(A, B) end,
  T2 = measureSpeed(Fun2, [PersonsLocations, ParcelsLocations]),
  Fun3 = fun(A, B) ->  createAndManage(A, B) end,
  T3 = measureSpeed(Fun3, [PersonsLocations, ParcelsLocations]),
  io:format("Czas zwyklego ~p, Czas rownoleglego: ~p, Czas mniej rownoleglego: ~p", [T1, T2, T3]).
