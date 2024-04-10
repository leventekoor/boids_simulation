-module(flock_algorithms).

-include_lib("../include/simulation_common.hrl").

-export([filter_out_non_perceived_boids/3, filter_out_self/2, flock/2, alignment/2,
         separation/2, cohesion/2]).

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
