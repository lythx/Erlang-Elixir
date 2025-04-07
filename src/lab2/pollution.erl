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
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4,
  get_one_value/4, get_station_mean/3, get_daily_mean/3, get_daily_min_and_max/3]).

create_monitor() -> [].

add_station(Name, Coords, Monitor) ->
  case get_station(Name, Monitor) of
    {error, _} ->
      case get_station(Coords, Monitor) of
        {error, _} ->
          Monitor ++ [{Name, Coords, []}];
        _ ->
          {error, "Station with given coordinates already exists"}
      end;
    _ ->
      {error, "Station with given name already exists"}
  end.

add_value(Station, Time, Type, Value, Monitor) ->
  case get_station(Station, Monitor) of
    {error, Message} ->
      {error, Message};
    {Name, Coords, Readings} ->
      case get_one_value(Name, Time, Type, Monitor) of
        {error, _} ->
          NewReadings =
            case proplists:get_value(Type, Readings) of
              undefined ->
                Readings ++ [{Type, [{Time, Value}]}];
              Values ->
                NewValues = proplists:delete(Time, Values) ++ [{Time, Value}],
                proplists:delete(Type, Readings) ++ [{Type, NewValues}]
            end,
          remove_station(Name, Monitor) ++ [{Name, Coords, NewReadings}];
        _ ->
          {error, "Value already exists"}
      end
  end.

remove_value(Station, Time, Type, Monitor) ->
  case get_station(Station, Monitor) of
    {error, Message} ->
      {error, Message};
    {Name, Coords, Readings} ->
      case get_one_value(Name, Time, Type, Monitor) of
        {error, _} ->
          {error, "Value does not exist"};
        _ ->
          Values = proplists:get_value(Type, Readings),
          NewValues = proplists:delete(Time, Values),
          NewReadings = proplists:delete(Type, Readings) ++ [{Type, NewValues}],
          remove_station(Name, Monitor) ++ [{Name, Coords, NewReadings}]
      end
  end.

get_one_value(Station, Time, Type, Monitor) ->
  case get_station(Station, Monitor) of
    {error, Message} ->
      {error, Message};
    {_, _, Readings} ->
      case proplists:get_value(Type, Readings) of
        undefined ->
          {error, "Value of given type does not exist"};
        Values ->
          case proplists:get_value(Time, Values) of
            undefined -> {error, "Value at given time does not exist"};
            Value -> Value
          end
      end
  end.

get_station_mean(Station, Type, Monitor) ->
  case get_station(Station, Monitor) of
    {error, Message} ->
      {error, Message};
    {_, _, Readings} ->
      case proplists:get_value(Type, Readings) of
        undefined ->
          {error, "Value of given type does not exist"};
        Values ->
          case length(Values) of
            0 ->
              {error, "Zero values for given type"};
            Count ->
              Sum = lists:foldl(fun({_, V}, Acc) -> Acc + V end, 0, Values),
              Sum / Count
          end
      end
  end.

get_daily_mean(Type, Date, Monitor) ->
  {Sum, Count} = lists:foldl(
    fun({_, _, Readings}, {AccS, AccC}) ->
      {S, C} = get_daily_sum_and_count_for_readings(Readings, Type, Date),
      {AccS + S, AccC + C}
    end,
    {0, 0},
    Monitor),
  case Count of
    0 ->
      {error, "No values of given type for given day"};
    _ ->
      Sum / Count
  end.

get_daily_min_and_max(Type, Date, Monitor) ->
  {DailyMin, DailyMax} = lists:foldl(
    fun({_, _, Readings}, {AccMin, AccMax}) ->
      case get_daily_min_and_max_for_readings(Readings, Type, Date) of
        {error, _} -> {AccMin, AccMax};
        {Min, Max} ->
          case AccMin of
            undefined -> {Min, Max};
            _ -> {lists:min([Min, AccMin]), lists:max([Max, AccMax])}
          end
      end
    end,
    {undefined, undefined},
    Monitor),
  case {DailyMin, DailyMax} of
    {undefined, undefined} -> {error, "No values of given type for given day"};
    _ -> {DailyMin, DailyMax}
  end.

get_station(_, []) ->
  {error, "Station not found"};
get_station({X, Y}, [{Name, {X, Y}, Readings} | _]) ->
  {Name, {X, Y}, Readings};
get_station(Name, [{Name, Coords, Readings} | _]) ->
  {Name, Coords, Readings};
get_station(Station, [_ | T]) ->
  get_station(Station, T).

remove_station(Name, Monitor) ->
  lists:filter(fun({N, _, _}) -> N =/= Name end, Monitor).

get_daily_sum_and_count_for_readings(Readings, Type, Date) ->
  case proplists:get_value(Type, Readings) of
    undefined ->
      {0, 0};
    Values ->
      DayValues = lists:filter(fun({{D, _}, _}) -> D == Date end, Values),
      Sum = lists:foldl(fun({_, V}, Acc) -> Acc + V end, 0, DayValues),
      {Sum, length(DayValues)}
  end.

get_daily_min_and_max_for_readings(Readings, Type, Date) ->
  case proplists:get_value(Type, Readings) of
    undefined ->
      {error, "Value of given type does not exist"};
    Values ->
      DayValues = lists:filter(fun({{D, _}, _}) -> D == Date end, Values),
      MappedValues = lists:map(fun({_, V}) -> V end, DayValues),
      case MappedValues of
        [] -> {error, "No values of given type on fiven day"};
        _ -> {lists:min(MappedValues), lists:max(MappedValues)}
      end
  end.
