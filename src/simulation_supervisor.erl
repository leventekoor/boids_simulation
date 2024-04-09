-module(simulation_supervisor).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([spawn_boids/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

spawn_boid() ->
    supervisor:start_child(?MODULE, []).

spawn_boids(N) when is_integer(N), N > 1 ->
    [spawn_boid()] ++ spawn_boids(N - 1);
spawn_boids(1) ->
    [spawn_boid()].

init([]) ->
    io:format("simulation_supervisor has started (~w)~n", [self()]),
    SupFlags =
        #{strategy => simple_one_for_one,
          intensity => 5,
          period => 5},

    ChildSpecs = {boid, {boid, start_link, []}, permanent, 1000, worker, [boid]},

    {ok, {SupFlags, [ChildSpecs]}}.

%% internal functions
