-module(simulation_supervisor).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([spawn_boids/1, kill_all_boids/0, update_all_boids/0, get_all_boids_states/0,
         get_all_boids_positions/0]).

-include("debug_log.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

spawn_boid() ->
    supervisor:start_child(?MODULE, []).

spawn_boids(N) when is_integer(N), N > 1 ->
    ?LOG("|- Spawning ~w boids...~n", [N]),
    lists:map(fun(_) -> spawn_boid() end, lists:seq(1, N));
spawn_boids(1) ->
    ?LOG("|- Spawning 1 boid...~n", []),
    spawn_boid().

kill_boid(Id) ->
    supervisor:terminate_child(?MODULE, Id).

kill_all_boids() ->
    ?LOG("|- Killing all boids...~n", []),
    Children = supervisor:which_children(?MODULE),
    lists:map(fun({_, Pid, _, _}) ->
                 ?LOG("|-- Killing boid ~w~n", [Pid]),
                 kill_boid(Pid)
              end,
              Children).

update_all_boids() ->
    ?LOG("|- Updating all boids...~n", []),
    Children = supervisor:which_children(?MODULE),
    AllBoidsStates = get_all_boids_states(),
    lists:map(fun({_, Pid, _, _}) ->
                 ?LOG("|-- Updating boid ~w~n", [Pid]),
                 gen_server:cast(Pid, {update, AllBoidsStates})
              end,
              Children).

get_all_boids_states() ->
    Children = supervisor:which_children(?MODULE),
    lists:map(fun({_, Pid, _, _}) -> gen_server:call(Pid, {get_state}) end, Children).

get_all_boids_positions() ->
    Children = supervisor:which_children(?MODULE),
    lists:map(fun({_, Pid, _, _}) -> gen_server:call(Pid, {get_position}) end, Children).

init([]) ->
    ?LOG("simulation_supervisor has started (~w)~n", [self()]),
    SupFlags =
        #{strategy => simple_one_for_one,
          intensity => 5,
          period => 5},

    ChildSpecs = {boid, {boid, start_link, []}, permanent, 1000, worker, [boid]},

    {ok, {SupFlags, [ChildSpecs]}}.

%% internal functions
