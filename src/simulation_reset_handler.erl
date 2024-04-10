-module(simulation_reset_handler).

-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    io:format("|- Resetting simulation...~n"),
    simulation_supervisor:kill_all_boids(),
    {ok, StartingBoidsCount} = application:get_env(boids_simulation, boids_count),
    simulation_supervisor:spawn_boids(StartingBoidsCount),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           <<"Simulation reset!\n">>,
                           Req0),
    {ok, Req, State}.
