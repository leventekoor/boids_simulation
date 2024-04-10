-module(boid).

-behaviour(gen_server).

-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("../include/simulation_common.hrl").

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    io:format("boid has started (~w)~n", [self()]),
    {ok,
     #boid_state{position = random_position(),
                 velocity = random_velocity(),
                 acceleration = #acceleration{x = 0, y = 0}}}.

handle_call({get_state}, _From, State) ->
    {reply, State, State};
handle_call({update, BoidsStates}, _From, State) ->
    {reply, ok, update_boid(State, BoidsStates)};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
random_within_limit(Limit) ->
    rand:uniform(Limit) * rand:uniform().

random_position() ->
    #position{x = random_within_limit(?SIMULATION_WIDTH),
              y = random_within_limit(?SIMULATION_HEIGHT)}.

random_velocity() ->
    #velocity{x = random_within_limit(?MAX_VELOCITY), y = random_within_limit(?MAX_VELOCITY)}.

clip_velocity(Velocity) ->
    Velocity#velocity{x = min(Velocity#velocity.x, ?MAX_VELOCITY),
                      y = min(Velocity#velocity.y, ?MAX_VELOCITY)}.

wrap_coordinate(Coordinate, Limit) ->
    case Coordinate of
        C when C < 0 ->
            Limit + C;
        C when C > Limit ->
            C - Limit;
        C ->
            C
    end.

wrap_position(#position{x = X, y = Y}) ->
    #position{x = wrap_coordinate(X, ?SIMULATION_WIDTH),
              y = wrap_coordinate(Y, ?SIMULATION_HEIGHT)}.

filter_out_non_perceived_boids(BoidState, BoidsStates, PerceptionRadius) ->
    lists:filter(fun(OtherBoidState) ->
                    Distance =
                        math:sqrt(math:pow(BoidState#boid_state.position#position.x
                                           - OtherBoidState#boid_state.position#position.x,
                                           2)
                                  + math:pow(BoidState#boid_state.position#position.y
                                             - OtherBoidState#boid_state.position#position.y,
                                             2)),
                    Distance < PerceptionRadius
                 end,
                 BoidsStates).

filter_out_self(BoidState, BoidsStates) ->
    lists:filter(fun(BoidState2) -> BoidState2 /= BoidState end, BoidsStates).

update_boid(BoidState, BoidsStates) ->
    {X, Y} = flock(BoidState, BoidsStates),
    io:format("Flock data: X: ~w, Y: ~w~n", [X, Y]),
    CurrentAcceleration = #acceleration{x = X, y = Y},
    NewVelocity =
        clip_velocity(#velocity{x =
                                    BoidState#boid_state.velocity#velocity.x
                                    + CurrentAcceleration#acceleration.x,
                                y =
                                    BoidState#boid_state.velocity#velocity.y
                                    + CurrentAcceleration#acceleration.y}),
    NewPosition =
        wrap_position(#position{x =
                                    BoidState#boid_state.position#position.x
                                    + NewVelocity#velocity.x,
                                y =
                                    BoidState#boid_state.position#position.y
                                    + NewVelocity#velocity.y}),
    BoidState#boid_state{position = NewPosition,
                         velocity = NewVelocity,
                         acceleration = #acceleration{x = 0, y = 0}}.

flock(BoidState, BoidsStates) ->
    {X1, Y1} = alignment(BoidState, BoidsStates),
    {X2, Y2} = separation(BoidState, BoidsStates),
    {X3, Y3} = cohesion(BoidState, BoidsStates),
    {X1 + X2 + X3, Y1 + Y2 + Y3}.

alignment(BoidState, BoidsStates) ->
    FilteredBoidsStates =
        filter_out_non_perceived_boids(BoidState,
                                       filter_out_self(BoidState, BoidsStates),
                                       ?ALIGNMENT_PERCEPTION_RADIUS),
    {X, Y} =
        lists:foldl(fun(OtherBoidState, {XAcc, YAcc}) ->
                       {XAcc + OtherBoidState#boid_state.velocity#velocity.x,
                        YAcc + OtherBoidState#boid_state.velocity#velocity.y}
                    end,
                    {0, 0},
                    FilteredBoidsStates),
    io:format("Alignment data: X: ~w, Y: ~w, state list length: ~w~n",
              [X, Y, length(FilteredBoidsStates)]),
    if length(FilteredBoidsStates) == 0 ->
           {0, 0};
       true ->
           {X / length(FilteredBoidsStates), Y / length(FilteredBoidsStates)}
    end.

separation(BoidState, BoidsStates) ->
    FilteredBoidsStates =
        filter_out_non_perceived_boids(BoidState,
                                       filter_out_self(BoidState, BoidsStates),
                                       ?SEPARATION_PERCEPTION_RADIUS),
    {X, Y} =
        lists:foldl(fun(OtherBoidState, {XAcc, YAcc}) ->
                       {XAcc
                        + (BoidState#boid_state.position#position.x
                           - OtherBoidState#boid_state.position#position.x),
                        YAcc
                        + (BoidState#boid_state.position#position.y
                           - OtherBoidState#boid_state.position#position.y)}
                    end,
                    {0, 0},
                    FilteredBoidsStates),
    io:format("Separation data: X: ~w, Y: ~w, state list length: ~w~n",
              [X, Y, length(FilteredBoidsStates)]),

    if length(FilteredBoidsStates) == 0 ->
           {0, 0};
       true ->
           {X / length(FilteredBoidsStates), Y / length(FilteredBoidsStates)}
    end.

cohesion(BoidState, BoidsStates) ->
    FilteredBoidsStates =
        filter_out_non_perceived_boids(BoidState,
                                       filter_out_self(BoidState, BoidsStates),
                                       ?COHESION_PERCEPTION_RADIUS),
    {X, Y} =
        lists:foldl(fun(OtherBoidState, {XAcc, YAcc}) ->
                       {XAcc + OtherBoidState#boid_state.position#position.x,
                        YAcc + OtherBoidState#boid_state.position#position.y}
                    end,
                    {0, 0},
                    FilteredBoidsStates),
    io:format("Cohesion data: X: ~w, Y: ~w, state list length: ~w~n",
              [X, Y, length(FilteredBoidsStates)]),
    if length(FilteredBoidsStates) == 0 ->
           {0, 0};
       true ->
           {X / length(FilteredBoidsStates), Y / length(FilteredBoidsStates)}
    end.
