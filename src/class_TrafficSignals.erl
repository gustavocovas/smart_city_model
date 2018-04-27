-module(class_TrafficSignals).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, ActorName, Signal ).

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
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, querySignalState/3 ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

-spec construct( wooper:state(), class_Actor:actor_settings(), class_Actor:name() , parameter() ) 
	-> wooper:state().
construct( State, ?wooper_construct_parameters ) ->
  case ets:info(traffic_signals) of
		undefined -> ets:new(traffic_signals, [public, set, named_table]);
    _ -> ok
	end,

	{signal, [{nodes, [{node, [{id, NodeId}], []}]}, {groups, Groups}]} = Signal,

	{GroupsByDestination, _} = lists:foldl(
		fun({group, Destinations}, {GroupsByDestination, CurrentGroupId}) -> 
			{
				lists:foldl(
					fun({destination, [{id, DestinationId}], _}, GroupsByDestinationAcc) -> 
						maps:put(DestinationId, CurrentGroupId, GroupsByDestinationAcc) end, 
					GroupsByDestination, 
					Destinations),
				CurrentGroupId + 1
			}
		end, 
		{maps:new(), 0}, 
		Groups),

	io:format("group by destination (at the end): ~p\n", [GroupsByDestination]),
	
	ets:insert(traffic_signals, {NodeId, self()}),
	ActorState = class_Actor:construct( State, ActorSettings , ActorName ),

	setAttributes(ActorState, [{signal, Signal}] ).

% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) -> State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->
  CurrentTick = class_Actor:get_current_tick_offset( State ),
  executeOneway( State , addSpontaneousTick, CurrentTick + 600 ).


-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + 100 ),

	?wooper_return_state_only( ScheduledState ).


-spec querySignalState( wooper:state(), parameter(), pid() ) -> class_Actor:actor_oneway_return().
querySignalState( State , _NodeId , PersonPID ) ->
	CycleTime = 60,	
	CurrentTick = class_Actor:get_current_tick_offset( State ), 

	TickInCycle = CurrentTick rem (CycleTime),
	TicksUntilNextCycle = CycleTime - TickInCycle,

	CurrentLightState = if 
		TickInCycle > (CycleTime / 2) -> {red, TicksUntilNextCycle};
		true -> {green, TicksUntilNextCycle}
	end,

  class_Actor:send_actor_message( PersonPID, { receive_signal_state, CurrentLightState }, State ).
