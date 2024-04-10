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

update_boid(BoidState, BoidsStates) ->
    {X, Y} = flock_algorithms:flock(BoidState, BoidsStates),
    CurrentAcceleration = #acceleration{x = X, y = Y},
    NewPosition =
        wrap_position(#position{x =
                                    BoidState#boid_state.position#position.x
                                    + BoidState#boid_state.velocity#velocity.x,
                                y =
                                    BoidState#boid_state.position#position.y
                                    + BoidState#boid_state.velocity#velocity.y}),
    NewVelocity =
        clip_velocity(#velocity{x =
                                    BoidState#boid_state.velocity#velocity.x
                                    + CurrentAcceleration#acceleration.x,
                                y =
                                    BoidState#boid_state.velocity#velocity.y
                                    + CurrentAcceleration#acceleration.y}),
    BoidState#boid_state{position = NewPosition,
                         velocity = NewVelocity,
                         acceleration = #acceleration{x = 0, y = 0}}.
