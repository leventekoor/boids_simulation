-module(boids_simulation_handler).

-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    _ = simulation_supervisor:spawn_boids(10),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           <<"Spawned 10 boids for you master!\n">>,
                           Req0),
    {ok, Req, State}.
