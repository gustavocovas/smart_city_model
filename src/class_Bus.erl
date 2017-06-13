%Class that represents a bus that can moves around the city graph
-module(class_Bus).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName, ListVertex , ListTripsFinal , StartTime , Interval , LogPID ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/7, new_link/7,
		 synchronous_new/7, synchronous_new_link/7,
		 synchronous_timed_new/7, synchronous_timed_new_link/7,
		 remote_new/8, remote_new_link/8, remote_synchronous_new/8,
		 remote_synchronous_new_link/8, remote_synchronisable_new_link/8,
		 remote_synchronous_timed_new/8, remote_synchronous_timed_new_link/8,
		 construct/8, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, go/3 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->


	ActorState = class_Actor:construct( State, ActorSettings, CarName ),

        DictVertices = dict:from_list( ListVertex ),

	setAttributes( ActorState, [
		{ car_name, CarName },
		{ dict , DictVertices },
		{ trips , ListTripsFinal },
		{ trip_index , 1 },
		{ log_pid, LogPID },
		{ distance , 0 },
		{ car_position, -1 },
		{ start_time , StartTime },	
		{ path , ok },
		{ interval , Interval }
						] ).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->
	
	Trips = getAttribute( State , trips ), 

	TripIndex = getAttribute( State , trip_index ), 
	
	case TripIndex > length( Trips ) of

		true ->
		
			
			Path = getAttribute( State , path ), 

			case Path of 

				finish -> 
					
					executeOneway( State , declareTermination );

				_ ->

					Type = getAttribute( State , type ),
							
					TotalLength = getAttribute( State , distance ),

					StartTime = getAttribute( State , start_time ),

					CarId = getAttribute( State , car_name ),	

					CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

					TotalTime =   CurrentTickOffset - StartTime, 	

					LastPosition = getAttribute( State , car_position ),

					LeavesTraffic = io_lib:format( "<event time=\"~w\" type=\"vehicle leaves traffic\" person=\"~s\" link=\"~s\" vehicle=\"~s\" relativePosition=\"1.0\" />\n", [ CurrentTickOffset , CarId , LastPosition , CarId ] ),
			
					LeavesVehicles = io_lib:format( "<event time=\"~w\" type=\"PersonLeavesVehicle\" person=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , CarId , CarId ] ),
						
					Arrival = io_lib:format( "<event time=\"~w\" type=\"arrival\" person=\"~s\" vehicle=\"~s\" link=\"~s\" legMode=\"car\" trip_time=\"~w\" distance=\"~w\" action=\"~s\"/>\n", [ CurrentTickOffset , CarId , CarId ,  LastPosition, TotalTime , TotalLength , Type ] ),

					ActStart = io_lib:format( "<event time=\"~w\" type=\"actstart\" person=\"~s\"  link=\"~s\"  actType=\"h\"  />\n", [ CurrentTickOffset , CarId , LastPosition ] ),

					TextFile = lists:concat( [ LeavesTraffic , LeavesVehicles , Arrival , ActStart ] ),

					LogPid = ?getAttr(log_pid),

					NewState = setAttribute( State , path , finish ),

					FinalState = class_Actor:send_actor_message( LogPid ,
						{ receive_action, { TextFile } }, NewState ),

					executeOneway( FinalState, scheduleNextSpontaneousTick )

				end;


		_ ->
	
			CurrentTrip = list_utils:get_element_at( Trips , TripIndex ),

			NewState = request_position( State , CurrentTrip ),
			?wooper_return_state_only( NewState )

	end.
	
remove_first( [ _First | List ] ) ->
	
	List.	

-spec request_position( wooper:state() , parameter() ) -> wooper:state().
request_position( State , Trip ) ->
	
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 	

	PathTest = getAttribute( State , path ),

	PathState = case PathTest of

		ok -> 
			
			PathTrip = element( 5 , Trip ),
			setAttribute( State, path, PathTrip );

		_ ->

			State

	end,

			
	Path = getAttribute( PathState , path ),

	case Path of 

		finish ->
			
			NextTrip = getAttribute( PathState , trip_index ) + 1,

			NewState = setAttribute( PathState , trip_index, NextTrip ),
			
			FinalState = setAttribute( NewState, path, ok ),

			executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + 1 );	
	
		false ->

			State;

		_ ->

			case length( Path ) > 1 of

				true ->	

					% get the current and the next vertex in the path	
					InitialVertice = list_utils:get_element_at( Path , 1 ),

					FinalVertice = list_utils:get_element_at( Path , 2 ),

					DictVertices = getAttribute( PathState , dict ),

					%the mode that the person will make the trip, walking or by car
					Mode = element( 1 , Trip ),

					Vertices = list_to_atom(lists:concat( [ InitialVertice , FinalVertice ] )),

					VertexPID = element( 2 , dict:find( InitialVertice , DictVertices)),	

					PathRest = remove_first( Path ),

					FinalState = setAttribute( PathState , path, PathRest ),
	
					class_Actor:send_actor_message( VertexPID ,
						{ getPosition, { Vertices , Mode } }, FinalState );

				false ->							

					LastPosition = getAttribute( PathState , car_position ),

					case LastPosition == -1 of

						true ->
							
							executeOneway( PathState , declareTermination );	

						false ->
		
							FinalState = setAttribute( PathState, path, finish ),

							executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + 1 )

					end

			end

	end.

	

% Called by the route with the requested position. Write the file to show the position of the car in the map.
%
% (actor oneway)
%
-spec go( wooper:state(), value(), pid() ) -> class_Actor:actor_oneway_return().
go( State, PositionTime , _GraphPID ) ->

	move ( State , PositionTime ).

-spec move( wooper:state(), car_position() ) -> class_Actor:actor_oneway_return().
move( State, PositionTime ) ->

	% get the current time of the simulation
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	% get the response from the city graph
	NewPosition = element( 1 , PositionTime ),
	Time = element( 2 , PositionTime),
	Length = element( 3 , PositionTime),

	% Calculate the total distance that the person moved until now.
	TotalLength = getAttribute( State , distance ) + Length,
	LengthState = setAttribute( State, distance , TotalLength ),

	LastPosition = getAttribute( LengthState , car_position ),	
	NewState = setAttribute( LengthState, car_position, NewPosition ),
		
  	CarId = getAttribute( LengthState , car_name ),
  	Type = getAttribute( LengthState , type ),

	TripIndex = getAttribute( State , trip_index ), 

	Trips = getAttribute( State , trips ), 
	
	CurrentTrip = list_utils:get_element_at( Trips , TripIndex ),

	FinalState = case LastPosition == -1 of

		false ->

			LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , atom_to_list(LastPosition) , CarId , Type ] ),
			NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId , Type ] ),

			TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

			LogPID = ?getAttr(log_pid),

			class_Actor:send_actor_message( LogPID,
				{ receive_action, { TextFile } }, NewState );

		true -> 

			LinkOrigin = element( 3 , CurrentTrip ),
	   
   			Text1 = io_lib:format( "<event time=\"~w\" type=\"actend\" person=\"~s\" link=\"~s\" actType=\"h\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , Type ] ),
   			Text2 = io_lib:format( "<event time=\"~w\" type=\"departure\" person=\"~s\" link=\"~s\" legMode=\"car\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , Type ] ),
  			Text3 = io_lib:format( "<event time=\"~w\" type=\"PersonEntersVehicle\" person=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , CarId , Type ] ),
  			Text4 = io_lib:format( "<event time=\"~w\" type=\"wait2link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , CarId , Type ] ),
  	
			NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId , Type ] ),

			TextFile = lists:concat( [ Text1 , Text2 , Text3 , Text4 , NextPositionText  ] ),

			LogPID = ?getAttr(log_pid),
			
			class_Actor:send_actor_message( LogPID,
				{ receive_action, { TextFile } }, NewState )

	end,

	executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + Time ).


% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	_Time = getAttribute( State, start_time ),

    	_CurrentTickOffset = class_Actor:get_current_tick_offset( State ),   	

	State. % Don't do nothing yet. To be implemented.

%	ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + Time ),

%	?wooper_return_state_only( ScheduledState ).