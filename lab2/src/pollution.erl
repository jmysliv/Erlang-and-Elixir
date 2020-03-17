%%%-------------------------------------------------------------------
%%% @author jjmre
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. mar 2020 13:42
%%%-------------------------------------------------------------------
-module(pollution).
-author("jjmre").
-record(monitor, {stations, measurements}).

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getStationWithBiggestValue/3, getStationsThatExceedsLimit/4]).

createMonitor() -> #monitor{stations = #{}, measurements = #{}}. %%Funkcja tworzy monitor, czyli rekord z dwoma elementami, które są mapami

checkNewStation(Name, {X, Y}, Monitor) -> %%Sprawdza czy mozna dodać nową stacje, czy istnieje juz stacje o tej samej nazwie, lub o tych samych wspołrzednych, zwraca true jeśli można dodać nową stacje
  Fu = fun(K, _) when (K == Name) -> true; (_, {V1, V2}) ->
    case ((V1==X) and (V2==Y)) of
      true -> true;
      _ -> false
    end end,
  List = maps:filter(Fu, Monitor#monitor.stations),
  case map_size(List) of
      0 -> true;
      _ -> false
  end.

%% dodaje nową stacje, po sprawdzeniu czy moze ja dodac
addStation(Name, {X, Y}, Monitor)  when is_float(X) and is_float(Y) and is_record(Monitor, monitor) and is_list(Name)
  and (X > -90) and (X < 90) and (Y > -180) and (Y < 180) ->
  case  checkNewStation(Name, {X, Y}, Monitor) of
      true -> #monitor{stations = (Monitor#monitor.stations)#{Name => {X, Y}}, measurements = (Monitor#monitor.measurements)};
      _ -> Monitor
  end.

%%Sprawdza czy istnieje stacja o podanej nazwie, zwraca true jeśli istnieje
checkStationExists(Name, Monitor) ->
  Fu = fun(K, _) when (K == Name) -> true; (_, _) -> false end,
  List = maps:filter(Fu, Monitor#monitor.stations),
  case map_size(List) of
    0 -> false;
    _ -> true
  end.

%%Dodaje, lub uaktualnia pomiar jeśli stacja istnieje
addValue(Name, Date, Type, Value, Monitor) when is_record(Monitor, monitor) and is_list(Type) and (is_float(Value) or is_integer(Value))->
  case checkStationExists(Name, Monitor)  of
      true -> #monitor{stations = (Monitor#monitor.stations), measurements = (Monitor#monitor.measurements)#{{Name, Date, Type} => Value}};
      _ -> Monitor
  end.

%%usuwa pomiar
removeValue(Name, Date, Type, Monitor) when is_record(Monitor, monitor) ->
  #monitor{stations = (Monitor#monitor.stations), measurements = maps:remove({Name, Date, Type}, (Monitor#monitor.measurements))}.

getOneValue(Name, Date, Type, Monitor)  when is_record(Monitor, monitor) -> %%zwraca wartośc pomiaru
  maps:get({Name, Date, Type},(Monitor#monitor.measurements)).

getStationMean(Name, Type, Monitor) when is_record(Monitor, monitor) -> %%oblicza średnia pomiaru dla danego typu i stacji
  Fu = fun({K_name, _, K_type}, _) ->
    case ((K_name == Name) and (K_type == Type)) of
        true -> true;
        _ -> false
    end end,
  Map = maps:filter(Fu, (Monitor#monitor.measurements)),
  Fu2 = fun(_,V,Acc) -> Acc + V end,
  Sum = maps:fold(Fu2,0,Map),
  Sum/map_size(Map).

getDailyMean(Type, {Day, _}, Monitor) when is_record(Monitor, monitor) -> %%oblicza średnia pomiaru dla danego typu i dnia
  Fu = fun({ _, {K_day, _}, K_type}, _) ->
    case ((K_day == Day) and (K_type == Type)) of
      true -> true;
      _ -> false
    end end,
  Map = maps:filter(Fu, (Monitor#monitor.measurements)),
  Fu2 = fun(_,V,Acc) -> Acc + V end,
  Sum = maps:fold(Fu2,0,Map),
  case map_size(Map) of
      0 -> 0;
      _ -> Sum/map_size(Map)
  end.

getStationWithBiggestValue(Type, {Day, _}, Monitor) -> %%zwraca stacje która miała najwieksze odczyty z dane typu w danym dniu
  Fu = fun({ _, {K_day, _}, K_type}, _) ->
    case ((K_day == Day) and (K_type == Type)) of
      true -> true;
      _ -> false
    end end,
  Map = maps:filter(Fu, (Monitor#monitor.measurements)),
  Fu2 = fun(_,V,Acc) ->
    case V > Acc of
      true -> V;
      _ -> Acc
    end end,
  Biggest = maps:fold(Fu2,0,Map),
  Fu3 = fun(_, V) when V == Biggest -> true; (_, _) -> false end,
  S = maps:filter(Fu3, Map),
  [Head |_] = maps:keys(S),
  {Name, _, _} = Head,
  Name.

getStationsThatExceedsLimit(Type, {Day, _}, Limit, Monitor) -> %%zwraca liste stacji które przekroczyły limit
  Fu = fun({ _, {K_day, _}, K_type}, V) ->
    case ((K_day == Day) and (K_type == Type) and (V > Limit)) of
      true -> true;
      _ -> false
    end end,
  Map = maps:filter(Fu, (Monitor#monitor.measurements)),
  Fu2 = fun({Name, _, _}, _) -> Name end,
  Stations = maps:values(maps:map(Fu2, Map)),
  Stations.