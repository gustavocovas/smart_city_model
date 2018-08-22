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
		"random_walk" -> digital_rails_random_walk(Graph, list_to_atom(Origin), false, 5);
		_ -> digraph:get_short_path( Graph , list_to_atom(Origin) , list_to_atom(Destination) )
	end,

	io:format("Path: ~p ~n", [NewPath]),

	ListTripsFinal = [ { ModeFinal , NewPath , LinkOrigin } ],

	{ StartTime , [ { NameFile , ListTripsFinal , Type , Park , ModeFinal , element (1 , string:to_integer(CarCount)), list_to_atom(TrafficModel) } ] }.

digital_rails_random_walk(_Graph, Origin, _UsedDigitalRails, 0) ->
	[Origin];

digital_rails_random_walk(Graph, Origin, UsedDigitalRails, RemainingLinks) ->
	Neighbours = digraph:out_neighbours(Graph, Origin),
	OutboundEdges = lists:map(fun(Neighbour) -> {Origin, Neighbour} end, Neighbours),

	% io:format("Outbound edges are: ~p ~n", [OutboundEdges]),

	DigitalRailsOutboundEdges = lists:filter(fun (E) -> is_edge_digital_rail(E) end, OutboundEdges),
	RegularOutboundEdges = lists:filter(fun (E) -> not is_edge_digital_rail(E) end, OutboundEdges),

	case length(DigitalRailsOutboundEdges) == 0 of
		true -> 
			{_, Destination} = lists:nth(rand:uniform(length(RegularOutboundEdges)), RegularOutboundEdges),
			case UsedDigitalRails of
				true ->
					% io:format("No outbound Digital rails and already used DR, ending trip (~p) ~n", [RegularOutboundEdges]),
					[Origin, Destination];
				false ->
					% io:format("No outbound Digital rails and have not used DR (~p) ~n", [RegularOutboundEdges]),
					[Origin] ++ digital_rails_random_walk(Graph, Destination, false, RemainingLinks - 1)
				end;
		false -> 
			case length(RegularOutboundEdges) == 0 of
				true -> 
					% io:format("No choice but to stay in digital rails (~p) ~n", DigitalRailsOutboundEdges),
					{_, Destination} = lists:nth(1, DigitalRailsOutboundEdges),
					[Origin] ++ digital_rails_random_walk(Graph, Destination, true, RemainingLinks);
				false -> 
					case rand:uniform() < 0.75 of
						true -> 
							% io:format("Remaining in digital rails (~p) ~n", DigitalRailsOutboundEdges),
							{_, Destination} = lists:nth(1, DigitalRailsOutboundEdges),
							[Origin] ++ digital_rails_random_walk(Graph, Destination, true, RemainingLinks);
						false -> 
							% io:format("Leaving digital rails and ending trip (~p) ~n", [RegularOutboundEdges]),
							{_, Destination} = lists:nth(rand:uniform(length(RegularOutboundEdges)), RegularOutboundEdges),
							[Origin, Destination]
					end
			end
	end.
	
is_edge_digital_rail(Edge) ->
	case Edge of 
		% Paraíso DR:
		{'1952545091', '5381067434'} -> true; 
		{'5381067434', '2098758071'} -> true; 
		{'2098758071', '4124282297'} -> true; 
		{'4124282297', '1953466364'} -> true;
		{'1953466364', '165466550'} -> true;
		{'165466550' , '1953466363'} -> true;
		{'1953466363', '60609673'} -> true; 
		{'60609673', '953466367'} -> true;
		{'1953466367', '1953466354'} -> true;
		{'1953466354', '2869020072'} -> true;
		{'2869020072', '60609692'} -> true; 
		{'60609692', '990911135'} -> true; 
		{'1990911135', '1953466370'} -> true;
		{'1953466370', '1990934598'} -> true;
		{'1990934598', '1953466361'} -> true;
		{'1953466361', '856727276'} -> true; 
		{'856727276' , '1953466358'} -> true;
		{'1953466358', '1953466373'} -> true;
		{'1953466373', '303863453'} -> true;
		{'303863453', '60609866'} -> true;
		{'60609866', '60609874'} -> true;

		% Consolação DR:
		{'60609819', '60609822'} -> true;
		{'60609822', '303863647'} -> true;
		{'303863647', '1953466378'} -> true;
		{'1953466378', '1953466359'} -> true;
		{'1953466359', '1990934597'} -> true;
		{'1990934597', '1953466360'} -> true;
		{'1953466360', '1990911134'} -> true;
		{'1990911134', '2021000708'} -> true;
		{'2021000708', '2352453476'} -> true;
		{'2352453476', '60609697'} -> true;
		{'60609697', '165463204'} -> true;
		{'165463204', '1953466375'} -> true;
		{'1953466375', '1953466365'} -> true;
		{'1953466365', '165459105'} -> true;
		{'165459105', '1953466357'} -> true;
		{'1953466357', '1819616337'} -> true;
		{'1819616337', '1953466362'} -> true;
		{'1953466362', '4236712736'} -> true;
		{'4236712736', '60609643'} -> true;
		{'60609643', '5381067437'} -> true;
		{'5381067437', '4236712716'} -> true;
		{'4236712716', '1421376041'} -> true;

		_ -> false
	end.
