-define(SIMULATION_WIDTH, 1000).
-define(SIMULATION_HEIGHT, 1000).
-define(MAX_VELOCITY, 10).
-define(MAX_ACCELERATION, 1).
-define(STARTING_VELOCITY_MAGNIUDE_MIN, 1).
-define(STARTING_VELOCITY_MAGNIUDE_MAX, 5).
-define(ALIGNMENT_PERCEPTION_RADIUS, 25).
-define(SEPARATION_PERCEPTION_RADIUS, 25).
-define(COHESION_PERCEPTION_RADIUS, 50).

-record(position, {x :: float(), y :: float()}).
-record(velocity, {x :: float(), y :: float()}).
-record(acceleration, {x :: float(), y :: float()}).
-record(boid_state,
        {position :: #position{}, velocity :: #velocity{}, acceleration :: #acceleration{}}).
