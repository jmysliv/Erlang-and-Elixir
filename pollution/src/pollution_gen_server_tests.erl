%%%-------------------------------------------------------------------
%%% @author jjmre
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. kwi 2020 22:04
%%%-------------------------------------------------------------------
-module(pollution_gen_server_tests).
-author("jjmre").


%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").

setUp() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("AGH", {10.0,10.0}),
  pollution_gen_server:addValue("AGH", {{2020,4,15},{23,3,51}}, "PM10", 80),
  pollution_gen_server:addValue("AGH", {{2020,4,15},{23,3,51}}, "PM2.5", 100),
  pollution_gen_server:addValue("AGH", {{2020,4,15},{23,5,1}}, "PM2.5", 200),
  pollution_gen_server:addStation("Krakow", {45.0,67.0}),
  pollution_gen_server:addValue("Krakow", {{2020,4,15},{23,5,37}}, "CO", 110),
  pollution_gen_server:addValue("Krakow", {{2020,4,15},{23,5,37}}, "PM10", 110),
  pollution_gen_server:addValue("Krakow", {{2020,4,15},{23,5,37}}, "PM2.5", 120),
  pollution_gen_server:addValue("Krakow", {{2020,4,15},{23,7,57}}, "CO", 160),
  pollution_gen_server:addValue("Krakow", {{2020,4,15},{23,8,23}}, "SO2", 200),
  pollution_gen_server:addValue("AGH", {{2020,4,15},{23,5,40}}, "PM10", 300),
  pollution_gen_server:removeValue("AGH", {{2020,4,15},{23,5,40}}, "PM10").


addStation_test() ->
  pollution_gen_server:start(),
  ?assert(pollution_gen_server:addStation("AGH", {10.0, 20.0})
    == ok),
  pollution_gen_server:stop().

addValue_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("AGH", {10.0, 20.0}),
  ?assert(pollution_gen_server:addValue("AGH", {{2020,4,15},{23,5,37}}, "PM10", 110)
    == ok),
  pollution_gen_server:stop().

removeValue_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("AGH", {10.0, 20.0}),
  pollution_gen_server:addValue("AGH", {{2020,4,15},{23,5,37}}, "PM10", 110),
  ?assert(pollution_gen_server:removeValue("AGH", {{2020,4,15},{23,5,37}}, "PM10")
    == ok),
  pollution_gen_server:stop().

getOneValue_test() ->
  setUp(),
  ?assert(pollution_gen_server:getOneValue("AGH", {{2020,4,15},{23,3,51}}, "PM10") == 80),
  pollution_gen_server:stop().

getStationMean_test() ->
  setUp(),
  ?assert(pollution_gen_server:getStationMean("AGH", "PM10") == 80.0),
  ?assert(pollution_gen_server:getStationMean("Krakow", "CO") == 135.0),
  pollution_gen_server:stop().

getDailyMean_test() ->
  setUp(),
  ?assert(pollution_gen_server:getDailyMean({{2020,4,15},{23,3,51}}, "PM10") == 95.0),
  pollution_gen_server:stop().

getStationWithBiggestValue_test() ->
  setUp(),
  ?assert(pollution_gen_server:getStationWithBiggestValue({{2020,4,15},{16,37,26}}, "PM2.5") == "AGH"),
  pollution_gen_server:stop().

getStationsThatExceedsLimit_test() ->
  setUp(),
  ?assert(pollution_gen_server:getStationsThatExceedsLimit({{2020,4,15},{16,37,26}},"PM10",  100) == ["Krakow"]),
  pollution_gen_server:stop().


