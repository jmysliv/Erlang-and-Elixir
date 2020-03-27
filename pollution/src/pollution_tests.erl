%%%-------------------------------------------------------------------
%%% @author jjmre
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. mar 2020 19:26
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("jjmre").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").
-record(monitor, {stations, measurements}).

agh() -> {monitor,#{"Krakow" => {45.0,67.0},
  "AGH" => {10.0,10.0}},
  #{{"AGH",{{2020,4,15},{23,3,51}},"PM10"} => 80,
    {"AGH",{{2020,4,15},{23,3,51}},"PM2.5"} => 100,
    {"AGH",{{2020,4,15},{23,5,1}},"PM2.5"} => 200,
    {"Krakow",{{2020,4,15},{23,5,37}},"CO"} => 110,
    {"Krakow",{{2020,4,15},{23,5,37}},"PM10"} => 110,
    {"Krakow",{{2020,4,15},{23,5,37}},"PM2.5"} => 120,
    {"Krakow",{{2020,4,15},{23,7,57}},"CO"} => 160,
    {"Krakow",{{2020,4,15},{23,8,23}},"SO2"} => 200}}.

createMonitor_test() ->
  ?assert(pollution:createMonitor() == #monitor{stations = #{}, measurements = #{}}).

addStation_test() ->
  ?assert(pollution:addStation("AGH", {10.0, 20.0}, pollution:createMonitor())
    == #monitor{stations = #{"AGH" => {10.0, 20.0}}, measurements = #{}}).

addValue_test() ->
  ?assert(pollution:addValue("AGH", {{2020,4,15},{23,5,37}}, "PM10", 110, #monitor{stations = #{"AGH" => {10.0, 20.0}}, measurements = #{}})
    == #monitor{stations = #{"AGH" => {10.0, 20.0}}, measurements = #{{"AGH",{{2020,4,15},{23,5,37}},"PM10"} => 110}}).

removeValue_test() ->
  ?assert(pollution:removeValue("AGH", {{2020,4,15},{23,5,37}}, "PM10", #monitor{stations = #{"AGH" => {10.0, 20.0}}, measurements = #{{"AGH",{{2020,4,15},{23,5,37}},"PM10"} => 110}})
    == #monitor{stations = #{"AGH" => {10.0, 20.0}}, measurements = #{}}).

getOneValue_test() ->
  ?assert(pollution:getOneValue("AGH", {{2020,4,15},{23,3,51}}, "PM10", agh()) == 80).

getStationMean_test() ->
  ?assert(pollution:getStationMean("AGH", "PM10", agh()) == 80.0),
  ?assert(pollution:getStationMean("AGH", "PM2.5", agh()) == 150.0).

getDailyMean_test() ->
  ?assert(pollution:getDailyMean("PM10", {{2020,4,15},{16,37,26}}, agh()) == 95.0).

getStationWithBiggestValue_test() ->
  ?assert(pollution:getStationWithBiggestValue("PM2.5", {{2020,4,15},{16,37,26}}, agh()) == "AGH").

getStationsThatExceedsLimit_test() ->
  ?assert(pollution:getStationsThatExceedsLimit("PM10", {{2020,4,15},{16,37,26}}, 100, agh()) == ["Krakow"]).