%%%-------------------------------------------------------------------
%%% @author Szymon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2025 20:33
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("Szymon").

-behaviour(gen_statem).

%% API
-export([start_link/0, stop/0, set_station/1, add_value/3, store_data/0]).

%% gen_statem callbacks
-export([init/1, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).

-record(data, {station = undefined, values = []}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_statem:call(?SERVER, stop).

set_station(Station) ->
  gen_statem:cast(?SERVER, {set_station, Station}).

add_value(Time, Type, Value) ->
  gen_statem:cast(?SERVER, {add_value, Time, Type, Value}).

store_data() ->
  gen_statem:call(?SERVER, store_data).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
  {ok, station_unset, #data{}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  handle_event_function.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%format_status(_Opt, [_PDict, _StateName, _State]) ->
%%  Status = some_term,
%%  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%state_name(_EventType, _EventContent, State = #state{}) ->
%%  NextStateName = next_state,
%%  {next_state, NextStateName, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(cast, {set_station, Station}, station_unset, _Data) ->
  {next_state, station_set, #data{station = Station}};

handle_event(cast, {add_value, Time, Type, Value},
    station_set, #data{station = Station, values = Values}) ->
  NewData = #data{station = Station, values = Values ++ [{Time, Type, Value}]},
  {keep_state, NewData};

handle_event({call, From}, store_data, station_set, #data{station = Station, values = Values}) ->
  lists:foreach(fun ({Time, Type, Value}) ->
    pollution_gen_server:add_value(Station, Time, Type, Value) end, Values),
  {next_state, station_unset, #data{}, [{reply, From, ok}]}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #data{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #data{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
