%%%-------------------------------------------------------------------
%%% @author Szymon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2025 14:19
%%%-------------------------------------------------------------------
-module(pollution).
-author("Szymon").

%% API
-export([create_monitor/0, add_station/3, add_value/5,
  remove_value/4, get_one_value/4, get_station_min/3, get_daily_mean/3]).

create_monitor() -> [].
add_station(Name, Coords, Monitor) ->
  case get_station(Name, Monitor) of
    {error, _} ->
      case get_station(Coords, Monitor) of
        {error, _} -> [Monitor | {Name, Coords, []}];
        _ -> {error, "Station with given coordinates already exists"}
      end;
    _ -> {error, "Station with given name already exists"}
  end.

add_value(Station, Time, Type, Value, Monitor) ->
  case get_station(Station, Monitor) of
    {error, Message} -> {error, Message};
    {Name, Coords, Readings} ->
      case get_one_value(Name, Time, Type, Monitor) of
        {error, _} ->
          NewReadings = case proplists:get_value(Type, Readings) of
            undefined ->
              [Readings | {Type, [{Time, Value}]}];
            Values ->
              NewValues = [Values | {Time, Value}],
              lists:map(
                fun({T, V}) ->
                  case T of
                    Type -> [V | NewValues]
                  end
                end, Readings),
          lists:map(
            fun({N, _, _}) ->
              case N of
                Name -> {Name, Coords, NewReadings}
              end
            end, Monitor)
                        end
      end
  end.

get_one_value(Station, Time, Type, Monitor) ->
  case get_station(Station, Monitor) of
    {error, _} -> {error, _};
    {_, _, Readings} ->
      case proplists:get_value(Type, Readings) of
        undefined -> {error, "Value of given type doesn't exist"};
        Values ->
          case proplists:get_value(Time, Values) of
            {error, "Value with given time doesn't exist"};
            Value -> Value
          end
      end.

get_station(_, []) ->
  {error, "Station not found"};
get_station({X, Y}, [{Name, {X, Y}, Readings} | _]) ->
  {Name, {X, Y}, Readings};
get_station(Name, [{Name, Coords, Readings} | _]) ->
  {Name, Coords, Readings};
get_station(Station, [_ | T]) ->
  get_station(Station, T).
