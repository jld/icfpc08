-module(newworld).
-include("stuff.hrl").
-export([relay/1]).

-define(MSG_INIT, 0).
-define(MSG_WHERE, 1).
-define(MSG_SEEN, 2).
-define(MSG_MARTIANS, 3).
-define(MSG_CAST, 4).
-define(MSG_HIT, 5).

-define(pc(P,D), port_command(P,D)).

encode_mob(#mob{ x = X, y = Y, dir = D, speed = S }) ->
    <<X/float-native, Y/float-native, D/float-native, S/float-native>>.

relay(P) ->
    receive
	{init, I} ->
	    ?pc(P, <<?MSG_INIT, 0:56,
		    (I#init.x_limit)/float-native,
		    (I#init.y_limit)/float-native,
		    (I#init.min_sensor)/float-native,
		    (I#init.max_sensor)/float-native,
		    (I#init.max_speed)/float-native,
		    (I#init.max_turn)/float-native,
		    (I#init.max_hard_turn)/float-native>>),
	    relay(P);

	{where, Time, VM} ->
	    ?pc(P, [<<?MSG_WHERE, 0:56,
		     Time/float-native>>,
		    (encode_mob(VM))]),
	    relay(P);

	{seen, {Type, #mob{ x = X, y = Y, r = R}}} ->
	    [T |_] = atom_to_list(Type),
	    ?pc(P, <<?MSG_SEEN, T, 0:48,
		    X/float-native, Y/float-native, R/float-native>>),
	    relay(P);

	{martians, ML} ->
	    ?pc(P, [<<?MSG_MARTIANS, 0:24,
		     (length(ML)):32/native>>
		    | lists:map(fun encode_mob/1, ML)]),
	    relay(P);

	{cast, D, K} ->
	    ?pc(P, <<?MSG_CAST, 0:56,
		    D/float-native>>),
	    receive 
		{P, {data, <<?MSG_HIT, Type, Turn, _Pad:40,
			    Dist/float-native>>}} ->
		    K ! {hit, Type, Turn, Dist}
	    end,
	    relay(P);

	Other ->
	    io:format("newworld relay: unrecognized message ~w~n", [Other]),
	    relay(P)
    end.
	
	
