-module(flock_algorithms).

-include("simulation_common.hrl").

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

set_magnitude({X, Y}, Magnitude) ->
    {X * Magnitude / math:sqrt(math:pow(X, 2) + math:pow(Y, 2)),
     Y * Magnitude / math:sqrt(math:pow(X, 2) + math:pow(Y, 2))}.

limit_magnitude({X, Y}, Limit) ->
    case math:sqrt(math:pow(X, 2) + math:pow(Y, 2)) > Limit of
        true ->
            set_magnitude({X, Y}, Limit);
        false ->
            {X, Y}
    end.

subtract({X1, Y1}, {X2, Y2}) ->
    {X1 - X2, Y1 - Y2}.

multiply({X, Y}, Factor) ->
    {X * Factor, Y * Factor}.

flock(BoidState, BoidsStates) ->
    {X1, Y1} = multiply(alignment(BoidState, BoidsStates), 1.5),
    {X2, Y2} = multiply(separation(BoidState, BoidsStates), 2),
    {X3, Y3} = multiply(cohesion(BoidState, BoidsStates), 1),
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

    if length(FilteredBoidsStates) == 0 ->
           {0, 0};
       true ->
           limit_magnitude(subtract(set_magnitude({X / length(FilteredBoidsStates),
                                                   Y / length(FilteredBoidsStates)},
                                                  ?ACCELERATION_TARGET_MAGNITUDE),
                                    {BoidState#boid_state.velocity#velocity.x,
                                     BoidState#boid_state.velocity#velocity.y}),
                           ?ACCELERATION_MAGNITUDE_LIMIT)
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

    if length(FilteredBoidsStates) == 0 ->
           {0, 0};
       true ->
           limit_magnitude(subtract(set_magnitude({X / length(FilteredBoidsStates),
                                                   Y / length(FilteredBoidsStates)},
                                                  ?ACCELERATION_TARGET_MAGNITUDE),
                                    {BoidState#boid_state.velocity#velocity.x,
                                     BoidState#boid_state.velocity#velocity.y}),
                           ?ACCELERATION_MAGNITUDE_LIMIT)
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

    if length(FilteredBoidsStates) == 0 ->
           {0, 0};
       true ->
           limit_magnitude(subtract(set_magnitude(subtract({X / length(FilteredBoidsStates),
                                                            Y / length(FilteredBoidsStates)},
                                                           {BoidState#boid_state.position#position.x,
                                                            BoidState#boid_state.position#position.y}),
                                                  ?ACCELERATION_TARGET_MAGNITUDE),
                                    {BoidState#boid_state.velocity#velocity.x,
                                     BoidState#boid_state.velocity#velocity.y}),
                           ?ACCELERATION_MAGNITUDE_LIMIT)
    end.
