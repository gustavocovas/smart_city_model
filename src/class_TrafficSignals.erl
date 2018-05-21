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
	{signal, [{nodes, Nodes}, {phases, Phases}]} = Signal,

	lists:foreach(fun(Node) -> 
		{node, [{id, NodeId}], _} = Node,
		case ets:info(traffic_signals) of
			undefined -> ets:new(traffic_signals, [public, set, named_table]);
   	 	_ -> ok
		end,

		ets:insert( traffic_signals , {NodeId, self()} )
	end, Nodes),

	PhaseMap = buildPhaseMap(Phases),
	
	ActorState = class_Actor:construct( State , ActorSettings , ActorName ),
	setAttributes( ActorState , [{signal, Signal}, {phase_map, PhaseMap}] ).

% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) -> State.

buildPhaseMap(single_phase, PhaseId, Routes) ->
	AccumulateDestination = fun(Route, AccPhaseMap) ->
		{route, [{orig, OriginId}, {dest, DestinationId}], _} = Route,
		maps:put({OriginId, DestinationId}, PhaseId, AccPhaseMap)
	end,
	lists:foldl(AccumulateDestination, maps:new(), Routes).

buildPhaseMap(Phases) ->
	AccumulatePhase = fun(Phase, {PhaseId, AccPhaseMap}) ->
		{phase, _, Routes} = Phase,
		SinglePhaseMap = buildPhaseMap(single_phase, PhaseId, Routes),
		{PhaseId + 1, maps:merge(AccPhaseMap, SinglePhaseMap)} 
	end,

	{_, PhaseMap} = lists:foldl(AccumulatePhase, {0, maps:new()}, Phases),
	PhaseMap.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->
  CurrentTick = class_Actor:get_current_tick_offset( State ),
  executeOneway( State , addSpontaneousTick, CurrentTick + 600 ).


-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + 100 ),

	?wooper_return_state_only( ScheduledState ).
	

ticksUntilNextPhase(CycleTime, PhaseTime, TickInCycle) ->
	ToNextPhase = (CycleTime - TickInCycle) rem PhaseTime,
	if
		ToNextPhase > 0 -> ToNextPhase;
		true -> PhaseTime
	end.


-spec querySignalState( wooper:state(), parameter(), pid() ) -> class_Actor:actor_oneway_return().
querySignalState( State , {OriginId, DestinationId} , PersonPID ) ->
	PhaseTime = 60,	
	CycleTime = 2 * PhaseTime,

	CurrentTick = class_Actor:get_current_tick_offset( State ), 
	OriginStr = lists:flatten(io_lib:format("~s", [OriginId])),
	DestinationStr = lists:flatten(io_lib:format("~s", [DestinationId])),

	PhaseMap = getAttribute( State, phase_map ),
	PhaseId = maps:get({OriginStr, DestinationStr}, PhaseMap),
	
	TickInCycle = CurrentTick rem (CycleTime),
	GreenPhase = if 
		TickInCycle >= PhaseTime -> 0;
		true -> 1
	end,

	TicksUntilNextPhase = ticksUntilNextPhase(CycleTime, PhaseTime, TickInCycle),

	% io:format("Current tick: ~p, TickInCycle: ~p, TicksUntilNextCycle: ~p, Phase map: ~p, destination id: ~p, phase id: ~p, GreenPhase: ~p\n", 
		% [CurrentTick, TickInCycle, TicksUntilNextPhase, PhaseMap, DestinationStr, PhaseId, GreenPhase]),

	CurrentLightState = if 
		GreenPhase == PhaseId -> {green, TicksUntilNextPhase};
		true -> {red, TicksUntilNextPhase}
	end,

  class_Actor:send_actor_message( PersonPID, { receive_signal_state, CurrentLightState }, State ).
