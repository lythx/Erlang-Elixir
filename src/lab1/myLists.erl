%%%-------------------------------------------------------------------
%%% @author Szymon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. mar 2025 18:06
%%%-------------------------------------------------------------------
-module(myLists).
-author("Szymon").

%% API
-export([contains/2, duplicate_elements/1, sumFloats/1]).

contains([], _) -> false;
contains([H|T], X) -> (H == X) or contains(T, X).

duplicate_elements([]) -> [];
duplicate_elements([H|T]) -> [H, H | duplicate_elements(T)].

%%sumFloats([]) -> 0.0;
%%sumFloats([H | T]) -> H + sumFloats(T).

sumFloats(L) -> sumFloatsAcc(L, 0.0).

sumFloatsAcc([], Acc) -> Acc;
sumFloatsAcc([H | T], Acc) -> sumFloatsAcc(T, H + Acc).
