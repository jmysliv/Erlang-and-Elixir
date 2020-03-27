%%%-------------------------------------------------------------------
%%% @author jjmre
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. mar 2020 17:45
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("jjmre").
%% API
-export([start/0, stop/0, init/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getStationWithBiggestValue/2, getStationsThatExceedsLimit/3]).

start() ->
  register(pollutionServer, spawn(pollution_server, init, [])).

stop() ->
  pollutionServer ! {request, self(), stop},
  receive
    {response, Info} -> Info
  end.

init() ->
  loop(pollution:createMonitor()).

loop(Monitor) ->
  receive
    {request, Pid, addStation, {Name, Cords}} ->
      Pid ! {response, stationAdded},
      loop(pollution:addStation(Name, Cords, Monitor));
    {request, Pid, addValue, {Name, Date, Type, Value}} ->
      Pid ! {response, valueAdded},
      loop(pollution:addValue(Name, Date, Type, Value, Monitor));
    {request, Pid, removeValue, {Name, Date, Type}} ->
      Pid ! {response, valueRemoved},
      loop(pollution:removeValue(Name, Date, Type, Monitor));
    {request, Pid, getOneValue, {Name, Date, Type}} ->
      Pid ! {response, pollution:getOneValue(Name, Date, Type, Monitor)},
      loop(Monitor);
    {request, Pid, getStationMean, {Name, Type}} ->
      Pid ! {response, pollution:getStationMean(Name, Type, Monitor)},
      loop(Monitor);
    {request, Pid, getDailyMean, {Date, Type}} ->
      Pid ! {response, pollution:getDailyMean(Type, Date, Monitor)},
      loop(Monitor);
    {request, Pid, getStationWithBiggestValue, {Date, Type}} ->
      Pid ! {response, pollution:getStationWithBiggestValue(Type, Date, Monitor)},
      loop(Monitor);
    {request, Pid, getStationsThatExceedsLimit, {Date, Type, Limit}} ->
      Pid ! {response, pollution:getStationsThatExceedsLimit(Type, Date, Limit, Monitor)},
      loop(Monitor);
    {request, Pid, stop} ->
      Pid ! {response, serverStopped};
    _ -> loop(Monitor)
  end.

addStation(Name, Cords) ->
  pollutionServer ! {request, self(), addStation, {Name, Cords}},
  receive
    {response, Info} -> Info
    after 5000 -> serverDoesntRespond
  end.

addValue(Name, Date, Type, Value) ->
  pollutionServer ! {request, self(), addValue, {Name, Date, Type, Value}},
  receive
    {response, Info} -> Info
  after 5000 -> serverDoesntRespond
  end.

removeValue(Name, Date, Type) ->
  pollutionServer ! {request, self(), removeValue, {Name, Date, Type}},
  receive
    {response, Info} -> Info
  after 5000 -> serverDoesntRespond
  end.

getOneValue(Name, Date, Type) ->
  pollutionServer ! {request, self(), getOneValue, {Name, Date, Type}},
  receive
    {response, Info} -> Info
  after 5000 -> serverDoesntRespond
  end.

getStationMean(Name, Type) ->
  pollutionServer ! {request, self(), getStationMean, {Name, Type}},
  receive
    {response, Info} -> Info
  after 5000 -> serverDoesntRespond
  end.

getDailyMean(Date, Type) ->
  pollutionServer ! {request, self(), getDailyMean, {Date, Type}},
  receive
    {response, Info} -> Info
  after 5000 -> serverDoesntRespond
  end.

getStationWithBiggestValue(Date, Type) ->
  pollutionServer ! {request, self(), getStationWithBiggestValue, {Date, Type}},
  receive
    {response, Info} -> Info
  after 5000 -> serverDoesntRespond
  end.

getStationsThatExceedsLimit(Date, Type, Limit) ->
  pollutionServer ! {request, self(), getStationsThatExceedsLimit, {Date, Type, Limit}},
  receive
    {response, Info} -> Info
  after 5000 -> serverDoesntRespond
  end.

