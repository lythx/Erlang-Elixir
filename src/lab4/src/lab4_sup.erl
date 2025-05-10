%%%-------------------------------------------------------------------
%% @doc lab4 top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lab4_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 3,
        period => 5
    },
    ChildSpecs = [
        #{
            id => pollution_srv,
            start => {pollution_gen_server, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [pollution_gen_server]
        },
        #{
            id => value_collector_statem,
            start => {pollution_value_collector_gen_statem, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [pollution_value_collector_gen_statem]
        }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
