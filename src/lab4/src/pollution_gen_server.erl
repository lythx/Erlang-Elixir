%%%-------------------------------------------------------------------
%%% @author Szymon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0, stop/0, crash/0, reset_state/0, add_station/2, add_value/4, remove_value/3,
get_one_value/3, get_station_mean/2, get_daily_mean/2, get_daily_min_and_max/2,
get_station_min/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {monitor = []}).
-record(call_info, {type = pollution_function, function, args = []}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

stop() ->
  gen_server:cast(?MODULE, stop).

crash() ->
  gen_server:cast(?MODULE, crash).

reset_state()->
  gen_server:cast(?MODULE, reset_state).

%%% pollution server interface

% casts

add_station(Name, Coords) ->
  gen_server:cast(?MODULE, #call_info{function = add_station, args=[Name, Coords] }).

add_value(Station, Time, Type, Value) ->
  gen_server:cast(?MODULE, #call_info{function = add_value, args=[Station, Time, Type, Value] }).

remove_value(Station, Time, Type) ->
  gen_server:cast(?MODULE, #call_info{function = remove_value, args=[Station, Time, Type] }).

% calls

get_one_value(Station, Time, Type) ->
  gen_server:call(?MODULE, #call_info{function = get_one_value, args = [Station, Time, Type] }).

get_station_min(Station, Type) ->
  gen_server:call(?MODULE, #call_info{function = get_station_min, args = [Station, Type] }).

get_station_mean(Station, Type) ->
  gen_server:call(?MODULE, #call_info{function = get_station_mean, args = [Station, Type] }).

get_daily_mean(Type, Date) ->
  gen_server:call(?MODULE, #call_info{function = get_daily_mean, args = [Type, Date] }).

get_daily_min_and_max(Type, Date) ->
  gen_server:call(?MODULE, #call_info{function = get_daily_min_and_max, args = [Type, Date] }).

%%% pollution server handlers

handle_cast(stop, Value) ->
  {stop, normal, Value};

handle_cast(crash, State = #state{}) ->
  no:exist(),
  {noreply, State};

handle_cast(reset_state, _) ->
  {noreply, #state{}};

handle_cast(#call_info{type = pollution_function, function = Function, args = Args},
    State = #state{}) ->
  NewMonitorOrError = apply(pollution, Function, Args ++ [State#state.monitor]),
  case NewMonitorOrError of
    {error, _} ->
      {noreply, State};
    NewMonitor ->
      NewState = #state{monitor = NewMonitor},
      {noreply, NewState}
  end.

handle_call(#call_info{type = pollution_function, function = Function, args = Args},
    _From, State = #state{}) ->
  Response = apply(pollution, Function, Args ++ [State#state.monitor]),
  {reply, Response, State}.

handle_info(_Info, State = #state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
