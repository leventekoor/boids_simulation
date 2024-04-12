-module(flock_algorithms_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/simulation_common.hrl").

% function to compare floats within a tolerance level
assert_float_equal_2d({Expected_X, Expected_Y}, {Actual_X, Actual_Y}) ->
    Tolerance = 0.00000000000000001,
    case (abs(Expected_X - Actual_X) < Tolerance) and (abs(Expected_Y - Actual_Y) < Tolerance)
    of
        true ->
            ok;
        false ->
            error
    end.

alignment_test() ->
    Boid1 =
        #boid_state{position = #position{x = 1, y = 1},
                    velocity = #velocity{x = 1, y = 1},
                    acceleration = #acceleration{x = 0, y = 0}},
    Boid2 =
        #boid_state{position = #position{x = 2, y = 2},
                    velocity = #velocity{x = 2, y = 2},
                    acceleration = #acceleration{x = 0, y = 0}},
    Boid3 =
        #boid_state{position = #position{x = 3, y = 3},
                    velocity = #velocity{x = 3, y = 3},
                    acceleration = #acceleration{x = 0, y = 0}},

    BoidsStates = [Boid1, Boid2, Boid3],
    BoidState = Boid1,

    ?assertEqual(ok,
                 assert_float_equal_2d(flock_algorithms:alignment(BoidState, BoidsStates),
                                       {0.1414213562373095, 0.1414213562373095})).

separation_test() ->
    Boid1 =
        #boid_state{position = #position{x = 1, y = 1},
                    velocity = #velocity{x = 1, y = 1},
                    acceleration = #acceleration{x = 0, y = 0}},
    Boid2 =
        #boid_state{position = #position{x = 2, y = 2},
                    velocity = #velocity{x = 2, y = 2},
                    acceleration = #acceleration{x = 0, y = 0}},
    Boid3 =
        #boid_state{position = #position{x = 3, y = 3},
                    velocity = #velocity{x = 3, y = 3},
                    acceleration = #acceleration{x = 0, y = 0}},

    BoidsStates = [Boid1, Boid2, Boid3],
    BoidState = Boid1,

    ?assertEqual(ok,
                 assert_float_equal_2d(flock_algorithms:separation(BoidState, BoidsStates),
                                       {-0.14142135623730953, -0.14142135623730953})).

cohesion_test() ->
    Boid1 =
        #boid_state{position = #position{x = 1, y = 1},
                    velocity = #velocity{x = 1, y = 1},
                    acceleration = #acceleration{x = 0, y = 0}},
    Boid2 =
        #boid_state{position = #position{x = 2, y = 2},
                    velocity = #velocity{x = 2, y = 2},
                    acceleration = #acceleration{x = 0, y = 0}},
    Boid3 =
        #boid_state{position = #position{x = 3, y = 3},
                    velocity = #velocity{x = 3, y = 3},
                    acceleration = #acceleration{x = 0, y = 0}},

    BoidsStates = [Boid1, Boid2, Boid3],
    BoidState = Boid1,

    ?assertEqual(ok,
                 assert_float_equal_2d(flock_algorithms:cohesion(BoidState, BoidsStates),
                                       {0.14142135623730953, 0.14142135623730953})).
