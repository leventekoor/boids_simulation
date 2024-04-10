-define(SIMULATION_WIDTH, 1000).
-define(SIMULATION_HEIGHT, 1000).
-define(MAX_VELOCITY, 5).
-define(STARTING_VELOCITY_MAGNIUDE_MIN, 1).
-define(STARTING_VELOCITY_MAGNIUDE_MAX, 5).
-define(ALIGNMENT_PERCEPTION_RADIUS, 25).
-define(SEPARATION_PERCEPTION_RADIUS, 25).
-define(COHESION_PERCEPTION_RADIUS, 50).
-define(ACCELERATION_TARGET_MAGNITUDE, 5).
-define(ACCELERATION_MAGNITUDE_LIMIT, 0.2).

-record(position, {x :: float(), y :: float()}).
-record(velocity, {x :: float(), y :: float()}).
-record(acceleration, {x :: float(), y :: float()}).
-record(boid_state,
        {position :: #position{}, velocity :: #velocity{}, acceleration :: #acceleration{}}).
