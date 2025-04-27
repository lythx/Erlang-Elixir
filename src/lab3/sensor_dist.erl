%%%-------------------------------------------------------------------
%%% @author Szymon
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. kwi 2025 17:44
%%%-------------------------------------------------------------------
-module(sensor_dist).
-author("Szymon").

%% API
-export([find_closest/2, find_closest_parallel/2, get_rand_locations/1]).

find_closest(PeopleLocations, SensorsLocations) ->
  lists:min([find_for_person(P, SensorsLocations) || P <- PeopleLocations]).

find_for_person(PersonLocation, SensorsLocations) ->
  Distances = [{distance(PersonLocation, S), SensorsLocations} || S <- SensorsLocations],
  {MinDist, MinSensor} = lists:min(Distances),
  {MinDist, PersonLocation, MinSensor}.

distance({X1, Y1}, {X2, Y2}) -> (X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2).

find_closest_parallel(PeopleLocations, SensorsLocations) ->
  ParentPid = self(),
  Pids = [spawn(
    fun() ->
      find_for_person(P, SensorsLocations, ParentPid)
    end
  ) || P <- PeopleLocations],
  Results = [
    receive
      {Pid, Val} ->
        Val
    after
      5000 -> error
    end || Pid <- Pids],
  Results.

find_for_person(PersonLocation, SensorsLocations, ParentPID) ->
  ParentPID ! {self(), find_for_person(PersonLocation, SensorsLocations)}.

get_rand_locations(Number) ->
  [{rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(1, Number)].

%%timer:tc dla sekwencyjnego: 1687142
%%timer:tc dla rownoleglego: 2639462