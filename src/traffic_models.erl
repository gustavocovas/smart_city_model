-module(traffic_models).

-export([get_speed_car/3, get_speed_walk/2]).

get_speed_car(LinkData, car_following, CapacityFactor) ->
	{_, Id, Length, RawCapacity, Freespeed, NumberCars} = LinkData,
	Capacity = CapacityFactor * RawCapacity,

	MinimumDensity = Capacity * 0.3,
	Speed = case NumberCars > MinimumDensity of
		true ->
			case NumberCars >= Capacity of
				true -> 0.8;
				false -> Freespeed * math:pow(1 - (NumberCars / Capacity), 0.05)
			end;
		false -> Freespeed
	end,

	% io:format("speed=~p n_cars=~p capacity=~p\n", [Speed, NumberCars, Capacity]),
	Time = (Length / Speed) + 1,
	{Id, round(Time), round(Length)};

get_speed_car(LinkData, freespeed, _) ->
	{_, Id, Length, _, Speed , _} = LinkData,	

	Time = Length / Speed,
	{Id, round(Time), round(Length)}.

get_speed_walk(LinkData, _) ->
	{_, Id, Length, _, _, _} = LinkData,	
	Time = ( Length / 2 ) + 1,

	{Id, round(Time), round(Length)}.
