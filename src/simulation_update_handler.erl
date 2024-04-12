-module(simulation_update_handler).

-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    simulation_supervisor:update_all_boids(),
    JsonData = generate_json_data(simulation_supervisor:get_all_boids_positions()),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"application/json">>,
                             <<"access-control-allow-origin">> => <<"*">>},
                           JsonData,
                           Req0),
    {ok, Req, State}.

generate_json_data(Positions) ->
    JsonData = lists:map(fun({X, Y}) -> #{<<"x">> => X, <<"y">> => Y} end, Positions),
    jsx:encode(JsonData).
