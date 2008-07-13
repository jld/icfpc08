-module(newworld).
-include("stuff.hrl").
-export([world/2, relay/1]).

world(Ini, Pvst) ->
    case os:getenv("ICFP_BIN") of
	false -> Dir = "."; 
	Dir -> ok
    end,
    P = open_port({spawn, Dir ++ "/newworld"},
		  [{packet, 4}, binary]),
    link(P),
    send_init(P, Ini),
    Pvst ! {observe, self()},
    relay(P).

-define(MSG_INIT, 0).
-define(MSG_WHERE, 1).
-define(MSG_SEEN, 2).
-define(MSG_MARTIANS, 3).
-define(MSG_CAST, 4).
-define(MSG_HIT, 5).
-define(MSG_RSET, 6).
-define(MSG_BOULDER, 7).

-define(pc(P,D), port_command(P,D)).

encode_mob(#mob{ x = X, y = Y, dir = D, speed = S }) ->
    <<X/float-native, Y/float-native, D/float-native, S/float-native>>;
encode_mob({_Type, #mob{ } = Mob}) ->
    encode_mob(Mob).

unabbrev_type($b) -> boulder;
unabbrev_type($c) -> crater;
unabbrev_type($h) -> home;
unabbrev_type($m) -> martian;
unabbrev_type(0) -> false;
unabbrev_type(C) -> list_to_atom([C |"..."]).

send_init(P, I) ->
    ?pc(P, <<?MSG_INIT, 0:56,
	    (I#init.x_limit)/float-native,
	    (I#init.y_limit)/float-native,
	    (I#init.min_sensor)/float-native,
	    (I#init.max_sensor)/float-native,
	    (I#init.max_speed)/float-native,
	    (I#init.max_turn)/float-native,
	    (I#init.max_hard_turn)/float-native>>).

relay(P) ->
    receive
	{vstate, #vstate{ time = Time, vmob = VM, vctlt = Turning,
			  others = Martians }} ->
	    ?pc(P, [<<?MSG_WHERE, 
		     Turning/signed, 0:48,		     
		     Time/float-native>>,
		    (encode_mob(VM))]),
	    ?pc(P, [<<?MSG_MARTIANS, 0:24,
		     (length(Martians)):32/native>>
		    | lists:map(fun encode_mob/1, Martians)]),
	    relay(P);

	{seen, {Type, #mob{ x = X, y = Y, r = R}}} ->
	    [T |_] = atom_to_list(Type),
	    ?pc(P, <<?MSG_SEEN, T, 0:48,
		    X/float-native, Y/float-native, R/float-native>>),
	    relay(P);

	{cast, D, Lat, K} ->
	    ?pc(P, <<?MSG_CAST, 0:56,
		    D/float-native, Lat/float-native>>),
	    receive
		{P, {data, <<?MSG_HIT, Type, Turn/signed, _Pad:40,
			    Dist/float-native>>}} ->
		    K ! {hit, D, unabbrev_type(Type), Turn, Dist}
	    % Should there be a timeout here, just in case?
	    end,
	    relay(P);
	{sync, K} ->
	    K ! sync;
	{boulder_hit, T} ->
	    ?pc(P, <<?MSG_BOULDER, 0:56,
		    T/float-native>>),
	    relay(P);
	{end_of_run, _T, _S} ->
	    ?pc(P, <<?MSG_RSET, 0:56>>),
	    relay(P);

	upgrade ->
	    newworld:relay(P);
	Other ->
	    io:format("newworld relay: unrecognized message ~w~n", [Other]),
	    relay(P)
    end.
