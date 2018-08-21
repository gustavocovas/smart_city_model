-module(create_agents).

-export([iterate_list/5]).

iterate_list( ListCount , Lista , Graph , Name , MainPID ) -> 
	ListaFinal = verify_list( ListCount , Lista , Graph , Name , MainPID ),
	class_Actor:create_initial_actor( class_CarManager, [ Name , ListaFinal ] ),
	MainPID ! { Name }.

verify_list( _ListCount , [ ] , _Graph , _Name , _MainPID ) -> [];
verify_list( ListCount , [ Car | MoreCars] , Graph , Name , MainPID ) ->

	Element = create_person( Car , Graph ),
	[ Element | verify_list( ListCount + 1 , MoreCars , Graph , Name , MainPID ) ].

create_person( Car , Graph ) ->
	{ Origin , Destination , CarCount , ST , LinkOrigin , Type , Mode , NameFile , Park, TrafficModel } = Car,
        { STInteger , _ } = string:to_integer( ST ),

	StartTime = STInteger,

	ModeFinal = case Mode of
		ok ->
			car; % if the mode is not set in the input file, "car" is the default value.
		_ ->
			list_to_atom( Mode ) % Otherwise, car or walk.
	end,

	NewPath = case Destination of 
		"random_walk" -> digital_rails_random_walk(Graph, list_to_atom(Origin), list_to_atom(LinkOrigin), 5);
		_ -> digraph:get_short_path( Graph , list_to_atom(Origin) , list_to_atom(Destination) )
	end,

	io:format("Path: ~p ~n", [NewPath]),

	ListTripsFinal = [ { ModeFinal , NewPath , LinkOrigin } ],

	{ StartTime , [ { NameFile , ListTripsFinal , Type , Park , ModeFinal , element (1 , string:to_integer(CarCount)), list_to_atom(TrafficModel) } ] }.

digital_rails_random_walk(_, Origin, _, 0) ->
	[Origin];

digital_rails_random_walk(Graph, Origin, LinkOrigin, Count) ->
	Neighbours = digraph:out_neighbours(Graph, Origin),
	OutboundEdges = lists:map(fun(Neighbour) -> {Origin, Neighbour} end, Neighbours),

	io:format("Outbound edges are: ~p ~n", [OutboundEdges]),

	DigitalRailsOutboundEdges = lists:filter(fun (E) -> is_edge_digital_rail(E) end, OutboundEdges),
	RegularOutboundEdges = lists:filter(fun (E) -> not is_edge_digital_rail(E) end, OutboundEdges),

	case length(DigitalRailsOutboundEdges) == 0 of
		true -> 
			io:format("No outbound Digital rails, ending trip (~p) ~n", [RegularOutboundEdges]),
			{_, Destination} = lists:nth(1, RegularOutboundEdges),
			[Origin, Destination];
		false -> 
			case length(RegularOutboundEdges) == 0 of
				true -> 
					io:format("No choice but to stay in digital rails (~p) ~n", DigitalRailsOutboundEdges),
					{_, Destination} = lists:nth(1, DigitalRailsOutboundEdges),
					[Origin] ++ digital_rails_random_walk(Graph, Destination, LinkOrigin, Count - 1);
				false -> 
					case rand:uniform() < 0.75 of
						true -> 
							io:format("Remaining in digital rails (~p) ~n", DigitalRailsOutboundEdges),
							{_, Destination} = lists:nth(1, DigitalRailsOutboundEdges),
							[Origin] ++ digital_rails_random_walk(Graph, Destination, LinkOrigin, Count - 1);
						false -> 
							io:format("Leaving digital rails and ending trip (~p) ~n", [RegularOutboundEdges]),
							{_, Destination} = lists:nth(1, RegularOutboundEdges),
							[Origin, Destination]
					end
			end
	end.
	
is_edge_digital_rail({_Origin, Destination}) ->
	case Destination of 
		'2098758071' -> true;
		'5381067434' -> true;
		'4124282297' -> true;
		'1953466364' -> true;
		_ -> false
	end.
