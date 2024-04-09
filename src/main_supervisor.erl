-module(main_supervisor).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 1,
          period => 5},

    SimulationSupervisorChildSpecs =
        {simulation_supervisor,
         {simulation_supervisor, start_link, []},
         permanent,
         brutal_kill,
         supervisor,
         [simulation_supervisor]},

    Children = [SimulationSupervisorChildSpecs],
    {ok, {SupFlags, Children}}.

%% internal functions
