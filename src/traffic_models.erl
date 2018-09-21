-module(traffic_models).

-export([get_speed_car/2, get_speed_walk/2]).

% There is DR in link and car can use it:
get_speed_car({_, Id, Length, _RawCapacity, Freespeed, _NumberCars, _Lanes, {_DigitalRailsLanes, _Cycle, _Signalized, _Offset}}, true) ->
	Time = Length / Freespeed,
	{Id, round(Time), round(Length)};

% There is DR but not effective:
get_speed_car({Whatever, Id, Length, RawCapacity, Freespeed, NumberCars, Lanes, {_DigitalRailsLanes, _Cycle, _Signalized, _Offset}}, noeffect) ->
	get_speed_car({Whatever, Id, Length, RawCapacity, Freespeed, NumberCars, Lanes, {}}, noeffect);

% There is DR but car cannot use it:
get_speed_car({_, Id, Length, RawCapacity, Freespeed, NumberCars, Lanes, {DigitalRailsLanes, _Cycle, _Signalized, _Offset}}, false) ->
	Capacity = ((Lanes - DigitalRailsLanes) / Lanes) * RawCapacity,

	MinimumDensity = Capacity * 0.3,
	Speed = case NumberCars > MinimumDensity of
		true ->
			case NumberCars >= Capacity of
				true -> 1.0;
				false -> Freespeed * math:pow(1 - (NumberCars / Capacity), 0.45)
			end;
		false -> Freespeed
	end,

	Time = (Length / Speed) + 1,
	{Id, round(Time), round(Length)};

% There is no DR:
get_speed_car({_, Id, Length, RawCapacity, Freespeed, NumberCars, _Lanes, {}}, _) ->
	Capacity = RawCapacity,

	MinimumDensity = Capacity * 0.3,
	Speed = case NumberCars > MinimumDensity of
		true ->
			case NumberCars >= Capacity of
				true -> 1.0;
				false -> Freespeed * math:pow(1 - (NumberCars / Capacity), 0.45)
			end;
		false -> Freespeed
	end,

	Time = (Length / Speed) + 1,
	{Id, round(Time), round(Length)}.

get_speed_walk(LinkData, _) ->
	{_, Id, Length, _, _, _} = LinkData,	
	Time = ( Length / 2 ) + 1,

	{Id, round(Time), round(Length)}.
