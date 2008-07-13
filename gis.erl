-module(gis).
-include("stuff.hrl").
-export([vstate/0, world/2, world/3, 
	 walls/1, cast/4, cast1/5]).

-define(FUDGE_M, 0.05).
-define(FUDGE_B, 0.60).

vstate() -> receive {set_vstate, VS} -> vstate(VS, []) end.

vstate(VS, Obs) ->
    receive
	{get_time, K} ->
	    K ! {time, VS#vstate.time}, vstate(VS, Obs);
	{get_vstate, K} ->
	    K ! {vstate, VS}, vstate(VS, Obs);
	{observe, K} -> 
	    K ! {vstate, VS}, vstate(VS, [K |Obs]);
	{unobserve, K} ->
	    vstate(VS, lists:delete(K, Obs));
	{set_vstate, NVS} ->
%% 	    #mob{ x = OX, y = OY, dir = OD } = VS#vstate.vmob,
%% 	    #mob{ x = NX, y = NY, dir = ND } = NVS#vstate.vmob,
%% 	    io:format("old dir = ~w, new dir = ~w, atan2 dir = ~w~n",
%% 		      [OD, ND, 180*math:atan2(NY-OY,NX-OX)/math:pi()]),
	    lists:foreach(fun (K) -> K ! {vstate, NVS} end, Obs),
	    vstate(NVS, Obs);
	Other ->
	    io:format("vstate: unreognized message ~w~n", [Other])
    end.

world(Ini, Pvst) -> 
    Pvst ! {observe, self()},
    receive 
	{vstate, #vstate{ vmob = VM }} -> 
	    world(Ini, VM, [{home, #mob{ x = 0.0, y = 0.0, r = 5.0 }}])
    end.

world(Ini, VM, Stuff) ->
    receive
	{vstate, #vstate{ vmob = NVM }} ->
	    world(Ini, NVM, Stuff);
	{seen, Obj} ->
	    case lists:member(Obj, Stuff) of
		true -> world(Ini, VM, Stuff);
		false -> world(Ini, VM, [Obj |Stuff])
	    end;

	{cast, TDir, K} ->
	    #mob{ x = X, y = Y, dir = Dir } = VM,
	    {Dist, {Type, _Obj}} = cast(X, Y, TDir, walls(Ini) ++ Stuff),
	    K ! {hit, TDir, Type, steerage:steer(Dir, TDir), Dist},
	    world(Ini, VM, Stuff);

	upgrade ->
	    ?MODULE:world(Ini, VM, Stuff);
	Other ->
	    io:format("world: unrecognized message ~w~n", [Other]),
	    world(Ini, VM, Stuff)
    end.

walls(#init{ x_limit = XL, y_limit = YL }) ->
    [{horiz, YL}, {horiz, -YL}, {vert, XL}, {vert, -XL}].

cast(X, Y, D, Stuff) ->
    RD = D * math:pi() / 180,
    cast([], X, Y, math:cos(RD), math:sin(RD), Stuff).
cast(Near, _X, _Y, _DX, _DY, []) -> Near;
cast(Near, X, Y, DX, DY, [Thing |Stuff]) ->
    Hit = cast1(X, Y, DX, DY, Thing),
    cast(if Hit < Near -> Hit; true -> Near end,
	 X, Y, DX, DY, Stuff).

cast1(_X, Y, _DX, DY, {horiz, OY} = Obj) ->
    if DY == 0 -> [];
       true ->
	    D = (OY - Y) / DY,
	    if D < 0 -> [];
	       true -> {D, Obj}
	    end
    end;
cast1(X, _Y, DX, _DY, {vert, OX} = Obj) ->
    if DX == 0 -> [];
       true ->
	    D = (OX - X) / DX,
	    if D < 0 -> [];
	       true -> {D, Obj}
	    end
    end;

cast1(X, Y, DX, DY, {_Type, #mob{ x = OX, y = OY, r = RR }} = Obj) ->
    R = RR * (1 + ?FUDGE_M) + ?FUDGE_B,
    FooX = X - OX,
    FooY = Y - OY,
    FooR2 = FooX * FooX + FooY * FooY,
    A = 1,
    B = 2 * (FooX * DX + FooY * DY),
    C = FooR2 - R * R,
    Det = B * B - 4 * A * C,
    if Det < 0 -> [];
       true -> RDet = math:sqrt(Det),
	       if -B - RDet > 0 -> {-B - RDet, Obj};
		  -B + RDet > 0 -> {-B + RDet, Obj};
		  true -> []
	       end
    end.
