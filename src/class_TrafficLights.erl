-module(class_TrafficLights).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, ActorName, NodeId ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, queryLightState/3 ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

    case ets:info(traffic_signals) of
		undefined -> ets:new(traffic_signals, [public, set, named_table]);
        _ -> ok
	end,
	
	io:format("inserting traffic_signal at node ~p, pid ~p", [NodeId, self()]),
	ets:insert(traffic_signals, {NodeId, self()}),
	io:format("ets contents: ~p\n", [ets:tab2list(traffic_signals)]),

    ActorState = class_Actor:construct( State, ActorSettings , ActorName ),

    setAttributes(ActorState, [{node_id, NodeId}] ).

% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) -> State.

% The City is a passive actor. Never start spontanely an action
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->
    CurrentTick = class_Actor:get_current_tick_offset( State ),
    executeOneway( State , addSpontaneousTick, CurrentTick + 600 ).


% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + 100 ),

	?wooper_return_state_only( ScheduledState ).


-spec queryLightState( wooper:state(), parameter(), pid() ) -> class_Actor:actor_oneway_return().
queryLightState( State , _NodeId , PersonPID ) ->
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	ColorChangePeriod = 30,
	TickInPeriod = CurrentTickOffset rem (ColorChangePeriod * 2),
	TicksUntilNextStateTransition = ColorChangePeriod - (TickInPeriod rem ColorChangePeriod),

	CurrentLightState = if 
		TickInPeriod > ColorChangePeriod -> {red, TicksUntilNextStateTransition};
		true -> {green, TicksUntilNextStateTransition}
	end,

    class_Actor:send_actor_message( PersonPID, { on_traffic_light_state_obtained, CurrentLightState }, State ).
