%%%-------------------------------------------------------------------
%%% @author Szymon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. mar 2025 17:37
%%%-------------------------------------------------------------------
-module(power).
-author("Szymon").

%% API
-export([power/2]).

power(X, 1) -> X;
power(X, P) -> X * power(X, P - 1).

