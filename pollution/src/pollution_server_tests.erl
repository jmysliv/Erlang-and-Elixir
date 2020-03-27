%%%-------------------------------------------------------------------
%%% @author jjmre
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. mar 2020 20:02
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
-author("jjmre").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").

setUp() ->
  pollution_server:start(),
  pollution_server:addStation("AGH", {10.0,10.0}),
  pollution_server:addValue("AGH", {{2020,4,15},{23,3,51}}, "PM10", 80),
  pollution_server:addValue("AGH", {{2020,4,15},{23,3,51}}, "PM2.5", 100),
  pollution_server:addValue("AGH", {{2020,4,15},{23,5,1}}, "PM2.5", 200),
  pollution_server:addStation("Krakow", {45.0,67.0}),
  pollution_server:addValue("Krakow", {{2020,4,15},{23,5,37}}, "CO", 110),
  pollution_server:addValue("Krakow", {{2020,4,15},{23,5,37}}, "PM10", 110),
  pollution_server:addValue("Krakow", {{2020,4,15},{23,5,37}}, "PM2.5", 120),
  pollution_server:addValue("Krakow", {{2020,4,15},{23,7,57}}, "CO", 160),
  pollution_server:addValue("Krakow", {{2020,4,15},{23,8,23}}, "SO2", 200),
  pollution_server:addValue("AGH", {{2020,4,15},{23,5,40}}, "PM10", 300),
  pollution_server:removeValue("AGH", {{2020,4,15},{23,5,40}}, "PM10").


addStation_test() ->
  pollution_server:start(),
  ?assert(pollution_server:addStation("AGH", {10.0, 20.0})
    == stationAdded),
  pollution_server:stop().

addValue_test() ->
  pollution_server:start(),
  pollution_server:addStation("AGH", {10.0, 20.0}),
  ?assert(pollution_server:addValue("AGH", {{2020,4,15},{23,5,37}}, "PM10", 110)
    == valueAdded),
  pollution_server:stop().

removeValue_test() ->
  pollution_server:start(),
  pollution_server:addStation("AGH", {10.0, 20.0}),
  pollution_server:addValue("AGH", {{2020,4,15},{23,5,37}}, "PM10", 110),
  ?assert(pollution_server:removeValue("AGH", {{2020,4,15},{23,5,37}}, "PM10")
    == valueRemoved),
  pollution_server:stop().

getOneValue_test() ->
  setUp(),
  ?assert(pollution_server:getOneValue("AGH", {{2020,4,15},{23,3,51}}, "PM10") == 80),
  pollution_server:stop().

getStationMean_test() ->
  setUp(),
  ?assert(pollution_server:getStationMean("AGH", "PM10") == 80.0),
  ?assert(pollution_server:getStationMean("Krakow", "CO") == 135.0),
  pollution_server:stop().

getDailyMean_test() ->
  setUp(),
  ?assert(pollution_server:getDailyMean({{2020,4,15},{23,3,51}}, "PM10") == 95.0),
  pollution_server:stop().

getStationWithBiggestValue_test() ->
  setUp(),
  ?assert(pollution_server:getStationWithBiggestValue({{2020,4,15},{16,37,26}}, "PM2.5") == "AGH"),
  pollution_server:stop().

getStationsThatExceedsLimit_test() ->
  setUp(),
  ?assert(pollution_server:getStationsThatExceedsLimit({{2020,4,15},{16,37,26}},"PM10",  100) == ["Krakow"]),
  pollution_server:stop().

