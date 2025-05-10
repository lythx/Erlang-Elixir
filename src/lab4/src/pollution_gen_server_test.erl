%%%-------------------------------------------------------------------
%%% @author Szymon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. kwi 2025 17:38
%%%-------------------------------------------------------------------
-module(pollution_gen_server_test).

-author("Szymon").

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  {foreach,
    fun setup/0,
    fun cleanup/1,
    [
      fun get_one_value/0,
      fun get_one_value_fail/0,
      fun get_station_mean/0,
      fun get_station_min/0,
      fun get_station_mean_fail/0,
      fun get_daily_mean/0,
      fun get_daily_mean_fail/0,
      fun get_daily_min_and_max/0,
      fun get_daily_min_and_max_fail/0,
      fun value_collector_statem/0
    ]}.

setup() -> ok.

cleanup(_) -> pollution_gen_server:reset_state().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_gen_server:add_value("Stacja 1", Time, "PM1", 36.3),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3),

  ?assertMatch(46.3, pollution_gen_server:get_one_value("Stacja 1", Time, "PM10")),
  ?assertMatch(36.3, pollution_gen_server:get_one_value("Stacja 1", Time, "PM1")),
  ?assertMatch(46.3, pollution_gen_server:get_one_value({1,1}, Time, "PM10")),
  ?assertMatch(26.3, pollution_gen_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10")).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_fail() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_gen_server:add_value("Stacja 1", Time, "PM1", 36.3),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3),

  ?assertMatch({error, _}, pollution_gen_server:get_one_value("Stacja 1", Time, "PM25")),
  ?assertMatch({error, _}, pollution_gen_server:get_one_value({1,1}, Time, "PM25")),
  ?assertMatch({error, _}, pollution_gen_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10")),
  ?assertMatch({error, _}, pollution_gen_server:get_one_value("Stacja 2", Time, "PM1")),
  ?assertMatch({error, _}, pollution_gen_server:get_one_value({1,2}, Time, "PM10")).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_mean() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,11}}, "PM10", 20),
  ?assertMatch(15.0, pollution_gen_server:get_station_mean("Stacja 1", "PM10")),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10),
  ?assertMatch(40/3, pollution_gen_server:get_station_mean("Stacja 1", "PM10")),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,13}}, "PM10", 20),
  ?assertMatch(15.0, pollution_gen_server:get_station_mean({1,1}, "PM10")).


get_station_min() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,11}}, "PM10", 20),
  ?assertMatch(10, pollution_gen_server:get_station_min("Stacja 1", "PM10")),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10),
  ?assertMatch(10, pollution_gen_server:get_station_min("Stacja 1", "PM10")),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,13}}, "PM10", -5),
  ?assertMatch(-5, pollution_gen_server:get_station_min({1,1}, "PM10")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_mean_fail() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  ?assertMatch({error, _}, pollution_gen_server:get_station_mean("Stacja 1", "PM10")),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  ?assertMatch({error, _}, pollution_gen_server:get_station_mean("Stacja 1", "PM25")),
  ?assertMatch({error, _}, pollution_gen_server:get_station_mean("Stacja 2", "PM25")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  pollution_gen_server:add_station("Stacja 2", {2,2}),
  pollution_gen_server:add_station("Stacja 3", {3,3}),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch(15.0, pollution_gen_server:get_daily_mean("PM10",{2023,3,27})),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 20),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 100),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 220),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 2000),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000),


  ?assertMatch(15.0, pollution_gen_server:get_daily_mean("PM10",{2023,3,27})),

  pollution_gen_server:add_value("Stacja 3", {{2023,3,27},{11,16,18}}, "PM10", 1234),



  ?assertMatch(258.8, pollution_gen_server:get_daily_mean("PM10",{2023,3,27})).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_fail() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  pollution_gen_server:add_station("Stacja 2", {2,2}),
  ?assertMatch({error, _}, pollution_gen_server:get_daily_mean("PM10",{2023,3,27})),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch({error, _}, pollution_gen_server:get_daily_mean("PM25",{2023,3,27})),
  ?assertMatch({error, _}, pollution_gen_server:get_daily_mean("PM10",{2023,3,29})).

get_daily_min_and_max() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  pollution_gen_server:add_station("Stacja 2", {2,2}),
  pollution_gen_server:add_station("Stacja 3", {3,3}),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch({10, 20}, pollution_gen_server:get_daily_min_and_max("PM10",{2023,3,27})),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 20),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 100),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 220),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 2000),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000),
  ?assertMatch({10, 20}, pollution_gen_server:get_daily_min_and_max("PM10",{2023,3,27})),

  pollution_gen_server:add_value("Stacja 3", {{2023,3,27},{11,16,18}}, "PM10", 1234),



  ?assertMatch({10, 1234}, pollution_gen_server:get_daily_min_and_max("PM10",{2023,3,27})).

get_daily_min_and_max_fail() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  pollution_gen_server:add_station("Stacja 2", {2,2}),
  ?assertMatch({error, _}, pollution_gen_server:get_daily_min_and_max("PM10",{2023,3,27})),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch({error, _}, pollution_gen_server:get_daily_min_and_max("PM25",{2023,3,27})),
  ?assertMatch({error, _}, pollution_gen_server:get_daily_min_and_max("PM10",{2023,3,29})).

value_collector_statem() ->
  Date = {{2023,3,27},{11,16,10}},
  pollution_gen_server:add_station("Stacja 1", {1, 1}),
  pollution_value_collector_gen_statem:set_station("Stacja 1"),
  pollution_value_collector_gen_statem:add_value(Date, "PM10", 6),
  pollution_value_collector_gen_statem:add_value(Date, "PM2.5", 10),
  pollution_value_collector_gen_statem:store_data(),
  ?assertMatch(6, pollution_gen_server:get_one_value("Stacja 1", Date, "PM10")),
  ?assertMatch(10, pollution_gen_server:get_one_value("Stacja 1", Date, "PM2.5")).