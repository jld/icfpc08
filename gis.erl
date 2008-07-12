-module(gis).
-include("stuff.hrl").
-export([vstate/0, world/1, world/2, 
	 walls/1, cast/4, cast1/5]).

-define(FUDGE, 0.6).

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
	    lists:foreach(fun (K) -> K ! {vstate, NVS} end, Obs),
	    vstate(NVS, Obs);
	Other ->
	    io:format("vstate: unreognized message ~w~n", [Other])
    end.

world(Ini) -> world(Ini, [{home, #mob{ x = 0.0, y = 0.0, r = 5.0 }}]).

world(Ini, Stuff) ->
    receive
	{seen, Obj} ->
	    case lists:member(Obj, Stuff) of
		true -> world(Ini, Stuff);
		false -> world(Ini, [Obj |Stuff])
	    end;
	{cast, X, Y, D, K} ->
	    K ! {hit, D, cast(X, Y, D, walls(Ini) ++ Stuff)},
	    world(Ini, Stuff);
	{get_init, K} ->
	    K ! {init, Ini},
	    world(Ini, Stuff);
	{dump, K} -> 
	    K ! {world_dump, Stuff};
	upgrade -> ?MODULE:world(Ini, Stuff);
	Other ->
	    io:format("world: unreognized message ~w~n", [Other])
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
    R = RR + ?FUDGE,
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
