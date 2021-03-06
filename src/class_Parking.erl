%Class that manage the parking spots in the city
-module(class_Parking).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings , SpotName , ListOfSpots ).

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
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, spot_available/3 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Creates a list with the parking spots in the city
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

    case ets:info(options) of
	undefined -> ets:new(options, [public, set, named_table]);
        _ -> ok
    end,
    ets:insert(options, {parking_pid, self() }),

    AvailableParkingSpots = dict:from_list( ListOfSpots ),
    UnavailableParkingSpots = dict:new(),

    %print( ListOfSpots ),

    ActorState = class_Actor:construct( State, ActorSettings , SpotName ),

    setAttributes( ActorState, [
                                { availableSpots , AvailableParkingSpots },
                                { unavailableSpots , UnavailableParkingSpots } ] ).


%print( [] ) ->
%    ok;

%print( [ Obj | List ] ) ->

%    Uuid = element( 1 , Obj ),
%    IdNo = element( 2 , Obj ),
%    io:format("spot: ~s ~s" , [ Uuid , IdNo ] ),
%    print( List ).

% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

    State.

% The City is a passive actor. Never start spontanely an action
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

    AvailableParkingSpots = getAttribute( State, availableSpots ),
    UnavailableParkingSpots = getAttribute( State, unavailableSpots ),

    CurrentTick = class_Actor:get_current_tick_offset( State ),

    { NewAPS , NewUPS } = update_spots( dict:to_list( UnavailableParkingSpots ) , { AvailableParkingSpots , UnavailableParkingSpots } , CurrentTick ),

    NewState = setAttribute( State, unavailableSpots , NewUPS ),
    FinalState = setAttribute( NewState, availableSpots , NewAPS ),

    executeOneway( FinalState , addSpontaneousTick, CurrentTick + 600 ).



update_spots( [] , { AvailableParkingSpots , UnavailableParkingSpots } , _CurrentTick ) ->
   
    { AvailableParkingSpots , UnavailableParkingSpots };

update_spots( [ Spot | List ] , { AvailableParkingSpots , UnavailableParkingSpots } , CurrentTick ) ->

    Elemento = list_utils:get_element_at( element( 2 , Spot ) , 1 ),
    { NewAPS , NewUPS } = case ( element( 3 , Elemento ) - CurrentTick ) < 1200 of
        true -> { AvailableParkingSpots , UnavailableParkingSpots };
        false -> { dict:append( element( 1 , Spot ) , { element( 2 , Spot ) , element( 3 , Spot) } , AvailableParkingSpots ),
		   dict:erase( element( 1 , Spot ) , UnavailableParkingSpots ) }
    end,
    update_spots( List , { NewAPS , NewUPS } , CurrentTick ).

% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + 100 ),

	?wooper_return_state_only( ScheduledState ).


-spec spot_available( wooper:state(), parameter(), pid() ) -> class_Actor:actor_oneway_return().
spot_available( State , SpotUUID , PersonPID ) ->
	

    AvailableParkingSpots = getAttribute( State, availableSpots ),
    
	UUID = element( 1 , SpotUUID ),

    case dict:find( UUID , AvailableParkingSpots ) of
        { ok, GraphNodeID } ->

     	    LogPID = ets:lookup_element(options, log_pid, 2 ),
	    UnavailableParkingSpots = getAttribute( State, unavailableSpots ),

	    CurrentTick = class_Actor:get_current_tick_offset( State ),


	    NewState = setAttribute( State, availableSpots, dict:erase( UUID, AvailableParkingSpots ) ),
	    NewNewState = setAttribute( NewState , unavailableSpots, dict:append( UUID, { element ( 1 , GraphNodeID ) , element( 2 , GraphNodeID ) , CurrentTick }, UnavailableParkingSpots ) ),

	    NewNewNewState = change_spot_state( NewNewState , UUID, false, LogPID ),
            class_Actor:send_actor_message( PersonPID, { get_parking_spot, { element( 1 , GraphNodeID ) } }, NewNewNewState  );
        error ->
    	    NotAvailableParkingSpots = getAttribute( State, unavailableSpots ),
	    CurrentLocation = dict:fetch( UUID , NotAvailableParkingSpots ),    
            class_Actor:send_actor_message( PersonPID, { get_parking_spot, { nok , element( 2 , CurrentLocation ) } }, State )
    end.


change_spot_state( State , SpotUUID, Available, LogPID ) ->

    Topic = "data_stream",
    RoutingKey = string:concat( SpotUUID, ".parking_monitoring.simulated" ),

    { { Year, Month, Day }, { Hour, Minute, Second } } = calendar:local_time(),
    Timestamp = lists:flatten( io_lib:format( "~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
                                          [ Year, Month, Day, Hour, Minute, Second ] ) ),

    SpotState = lists:flatten( io_lib:format( "~p", [ Available ] ) ),

    Message = "{\"parking_monitoring\": [
                  {\"available\": \"" ++ SpotState ++ "\"," ++
                   "\"timestamp\": \"" ++ Timestamp ++ "\"}]}",

    class_Actor:send_actor_message( LogPID, { publish_data, { Topic, RoutingKey, Message } }, State ).

