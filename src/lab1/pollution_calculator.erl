%%%-------------------------------------------------------------------
%%% @author Szymon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. mar 2025 15:45
%%%-------------------------------------------------------------------
-module(pollution_calculator).
-author("Szymon").

%% API
-export([get_example_data/0, number_of_readings/2, calculate_max/2, calculate_mean/2]).

number_of_readings(Readings, Date) -> length([R || R <- Readings, is_matching_date(R, Date)]).

calculate_max(Readings, Type) ->
  case calc_max(Readings, Type) of
    null ->
      io:format("typ pomiaru nie istnieje\n");
    Value -> Value
  end.

calculate_mean(Readings, Type) ->
  case calc_sum_and_count(Readings, Type, 0, 0) of
    {0, 0} ->
      io:format("typ pomiaru nie istnieje\n");
    {Sum, Count} -> Sum / Count
  end.

get_example_data() -> [
  {"Podgorze", {{2025, 3, 22}, {15, 52, 42}}, [{"PM10", 0.1}, {"PM2.5", 0.17}, {"PM1", 0.44}, {"temperature", 16.4}, {"pressure", 992.7}, {"humidity", 30.2}]},
  {"Nowa Huta", {{2025, 3, 22}, {16, 15, 23}}, [{"PM10", 0.12}, {"PM2.5", 0.19}, {"temperature", 17.1}, {"pressure", 993.1}]},
  {"Podgorze", {{2025, 3, 23}, {9, 30, 0}}, [{"PM10", 0.08}, {"PM2.5", 0.15}, {"PM1", 0.38}, {"temperature", 14.8}, {"humidity", 35.5}]},
  {"Bronowice", {{2025, 3, 23}, {12, 45, 12}}, [{"PM2.5", 0.21}, {"temperature", 15.5}, {"pressure", 991.9}]},
  {"Podgorze", {{2025, 3, 23}, {18, 0, 5}}, [{"PM10", 0.15}, {"PM1", 0.48}, {"temperature", 16.9}, {"pressure", 992.5}, {"humidity", 28.7}]},
  {"Nowa Huta", {{2025, 3, 23}, {8, 20, 30}}, [{"PM10", 0.11}, {"PM2.5", 0.18}, {"temperature", 15.2}]},
  {"Podgorze", {{2025, 3, 24}, {14, 55, 45}}, [{"PM2.5", 0.16}, {"PM1", 0.41}, {"temperature", 16.0}, {"pressure", 993.3}, {"humidity", 32.0}]},
  {"Nowa Huta", {{2025, 3, 24}, {20, 10, 0}}, [{"PM10", 0.09}, {"temperature", 14.5}, {"pressure", 992.1}]}
].

is_matching_date({_, {Date, _}, _}, Date) -> true;
is_matching_date(_, _) -> false.

calc_max([], _) -> null;
calc_max([{_, _, Data} | T], Type) ->
  MaxTail = calc_max(T, Type),
  case get_type_value(Data, Type) of
    null ->
      MaxTail;
    Value ->
      case ((MaxTail == null) or (MaxTail < Value)) of
        true -> Value;
        false -> MaxTail
      end
  end.

calc_sum_and_count([], _, Sum, Count) -> {Sum, Count};
calc_sum_and_count([{_, _, Data} | T], Type, Sum, Count) ->
  case get_type_value(Data, Type) of
    null ->
      calc_sum_and_count(T, Type, Sum, Count);
    Value ->
      calc_sum_and_count(T, Type, Sum + Value, Count + 1)
  end.

get_type_value([], _) -> null;
get_type_value([{Type, Value} | _], Type) -> Value;
get_type_value([_ | T], Type) -> get_type_value(T, Type).
