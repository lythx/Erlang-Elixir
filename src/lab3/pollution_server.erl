%%%-------------------------------------------------------------------
%%% @author Szymon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. kwi 2025 13:24
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Szymon").

%% API
-export([add_station/2, add_value/4, remove_value/3,
  get_one_value/3, get_station_mean/2, get_daily_mean/2, get_daily_min_and_max/2,
  get_station_min/2, start/0, stop/0]).


start() ->
  register(pollution_server_pid, spawn(fun() -> init() end)),
  pollution_server_pid.

stop() ->
  pollution_server_pid ! stop,
  unregister(pollution_server_pid),
  ok.

add_station(Name, Coords) ->
  call_server_update_function(add_station, [Name, Coords]).

add_value(Station, Time, Type, Value) ->
  call_server_update_function(add_value, [Station, Time, Type, Value]).

remove_value(Station, Time, Type) ->
  call_server_update_function(remove_value, [Station, Time, Type]).

get_one_value(Station, Time, Type) ->
  call_server_get_function(get_one_value, [Station, Time, Type]).

get_station_min(Station, Type) ->
  call_server_get_function(get_station_min, [Station, Type]).

get_station_mean(Station, Type) ->
  call_server_get_function(get_station_mean, [Station, Type]).

get_daily_mean(Type, Date) ->
  call_server_get_function(get_daily_mean, [Type, Date]).

get_daily_min_and_max(Type, Date) ->
  call_server_get_function(get_daily_min_and_max, [Type, Date]).


init() ->
   ServerLoop = fun Loop(Monitor) ->
    receive
      {update, Pid, Module, Function, Args} ->
        NewMonitorOrError = apply(Module, Function, Args ++ [Monitor]),
        Pid ! NewMonitorOrError,
        case NewMonitorOrError of
          {error, _} ->
            Loop(Monitor);
          NewMonitor ->
            Loop(NewMonitor)
        end;
      {get, Pid, Module, Function, Args} ->
        Pid ! apply(Module, Function, Args ++ [Monitor]),
        Loop(Monitor);
      stop ->
        ok
    after
      60000 ->
        ok
    end
    end,
  ServerLoop([]).

call_server_update_function(Function, Args) ->
  pollution_server_pid ! {update, self(), pollution, Function, Args},
  receive
    Val ->
      Val
  after
    5000 ->
      ok
  end.

call_server_get_function(Function, Args) ->
  pollution_server_pid ! {get, self(), pollution, Function, Args},
  receive
    Val ->
      Val
  after
    5000 ->
      ok
  end.
