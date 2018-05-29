-module(traffic_models).

-export([get_speed_car/2, get_speed_walk/2]).

get_speed_car(LinkData, car_following) ->
	{_, Id, Length, Capacity, Freespeed, NumberCars} = LinkData,

	MinimumDensity = Capacity * 0.3,
	Speed = case NumberCars > MinimumDensity of
		true ->
			case NumberCars >= Capacity of
				true -> 0.8;
				false -> Freespeed * math:pow(1 - (NumberCars / Capacity), 0.6)
			end;
		false -> Freespeed
	end,

	Time = (Length / Speed) + 1,
	{Id, round(Time), round(Length)};

get_speed_car(LinkData, freespeed) ->
	{_, Id, Length, _, Speed , _} = LinkData,	

	Time = Length / Speed,
	{Id, round(Time), round(Length)}.

get_speed_walk(LinkData, _) ->
	{_, Id, Length, _, _, _} = LinkData,	
	Time = ( Length / 2 ) + 1,

	{Id, round(Time), round(Length)}.
