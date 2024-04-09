-module(boids_simulation_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(ANY_HOST, '_').
-define(NO_OPTIONS, []).

start(_StartType, _StartArgs) ->
    Paths =
        [{"/reset", simulation_reset_handler, ?NO_OPTIONS},
         {"/spawn", simulation_spawn_handler, ?NO_OPTIONS}],

    Dispatch = cowboy_router:compile([{?ANY_HOST, Paths}]),
    {ok, _} =
        cowboy:start_clear(my_http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),

    main_supervisor:start_link().

stop(_State) ->
    ok.

%% internal functions
