%%%-------------------------------------------------------------------
%% @doc lab4 public API
%% @end
%%%-------------------------------------------------------------------

-module(lab4_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lab4_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
