-module(raydec).
-include("stuff.hrl").
-export([caster_0/2, start/2]).

start(Serv, Pvst) ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    receive
	{set_world, Pworld, Ini} -> ok
    end,
    Pcast = spawn_link(?MODULE, caster_0, [Pworld, Ini]),
    Pvst ! {observe, self()},
    Pvst ! {observe, Pcast}, 
    trial(Serv, Pcast, Pworld, Ini).

trial(Serv, Pcast, Pworld, Ini) ->
    receive 
	start_of_run ->
	    Pcast ! start_of_run,
	    comms:scmd(Serv, a),
	    comms:scmd(Serv, a),
	    receive {vstate, VS} ->
		    run(Serv, Pcast, Pworld, VS, Ini)
	    end
    end.

-define(UTIL_DOOM, -1.0).
-define(MIN_SPEED, 0.3).
-define(ORBIT_XTRA, 2.0).

run(Serv, Pcast, Pworld, VS, Ini) ->
    receive
	decide ->
	    Pcast ! {get_best, self()},
	    receive
		{best, _Tang, Tut, Ttu, Tty} = _Be ->
%		    if (Tty == martian) ->
%			    io:format("Doom doom doom! ~w ~w~n", [Tut, Ttu]);
%		       true -> ok
%		    end,
		    orbit_check(Serv, VS#vstate.vmob, Ini),
		    bonk_check(Serv, VS#vstate.vmob, Tut, Tty, Ini),
%% 		    io:format("Best: ~w -> ~w~n",
%% 			      [(VS#vstate.vmob)#mob.dir, Be]),
		    steerage:do_turn(Serv, VS, Ttu)
	    end,
	    run(Serv, Pcast, Pworld, VS, Ini);
	{vstate, NVS} ->
	    run(Serv, Pcast, Pworld, NVS, Ini);
	{end_of_run, _T, _S} ->
	    trial(Serv, Pcast, Pworld, Ini);
	Other -> 
	    io:format("raydec: unhandled message ~w~n", [Other]),
	    run(Serv, Pcast, Pworld, VS, Ini)
    end.

orbit_check(Serv, #mob{ x = X, y = Y, speed = Speed, dir = Dir }, Ini) ->
    Rdir = Dir * math:pi() / 180,
    Hit = abs(X * math:sin(Rdir) - Y * math:cos(Rdir)),
    if Hit =< 4 -> nevermind;
       true ->
	    TSO = 360 / Ini#init.max_hard_turn,
	    Rad = ?ORBIT_XTRA * TSO * Speed / (2 * math:pi()),
	    if X * X + Y * Y =< Rad * Rad ->
%		    io:format("Breaking orbit!~n"),
		    comms:scmd(Serv, b),
		    comms:scmd(Serv, b);
	       true -> ok
	    end
    end.

bonk_check(Serv, #mob{ speed = Speed }, Tut, Tty, Ini) ->
    if (Tut < ?UTIL_DOOM) 
       and (Speed > (Ini#init.max_speed * ?MIN_SPEED))
       and (Tty /= martian) ->
%	    io:format("Eeeek! ~w~n", [VS#vstate.time]),
	    comms:scmd(Serv, b);
       true ->
	    comms:scmd(Serv, a)
    end.

-define(COEFF_HOME, 1.0).
-define(COEFF_HIT_H, 10.0).
-define(COEFF_HIT_B, -50.0).
-define(COEFF_HIT_C, -60.0).
-define(COEFF_HIT_M, -40.0).
-define(COEFF_UNSAFE, -1.5).

-record(raydec_cst, {vm, pworld, init}).

caster_0(Pworld, Ini) ->
    receive
	start_of_run -> ok
    end,
    receive
	{vstate, #vstate { vmob = VM }} ->
	    caster(#raydec_cst{ vm = VM, pworld = Pworld, init = Ini }, 
		   VM#mob.dir)
    end.

caster(ST, Bang) ->
    {But, Btu, Bty} = evaluate(ST, Bang),
    caster(ST, Bang, But, Btu, Bty).

caster(ST, Bang, But, Btu, Bty) ->
    receive
	{vstate, #vstate { vmob = VM }} ->
	    caster(ST#raydec_cst{ vm = VM }, Bang);
	{get_best, K} ->
	    K ! {best, Bang, But, Btu, Bty},
	    caster(ST, Bang, But, Btu, Bty);
	{end_of_run, _T, _S} ->
	    caster_0(ST#raydec_cst.pworld, ST#raydec_cst.init)
    after 0 ->
	    Rang = (ST#raydec_cst.vm)#mob.dir
		+ 180 * math:pow(2 * random:uniform() - 1, 3),
	    {Rut, Rtu, Rty} = evaluate(ST, Rang),
	    if Rut > But -> 
%% 		    if Rty == crater ->
%% 			    io:format("O NOES!~n");
%% 		       true -> ok
%% 		    end,
		    caster(ST, Rang, Rut, Rtu, Rty);
	       true ->
		    caster(ST, Bang, But, Btu, Bty)
	    end
    end.


gradto(HX, HY, OX, OY, VX, VY) ->
    DX = OX - HX,
    DY = OY - HY,
    R = math:sqrt(DX*DX + DY*DY),
    (DX * VX + DY * VY) / R.

evaluate(#raydec_cst{ vm = #mob{ x = HX, y = HY }, pworld = Pworld }, Cang) ->
    Pworld ! {cast, Cang, 75, self()}, % XXX constant
    Crad = Cang * math:pi() / 180,
    VX = math:cos(Crad), VY = math:sin(Crad),
    Uhome = ?COEFF_HOME * gradto(HX, HY, 0, 0, VX, VY),
    receive
	{hit, _D, Type, Turn, Dist, Unsafe} -> lovely
    end,
    Uuns = Unsafe * ?COEFF_UNSAFE,
    case Type of 
	boulder -> Co = ?COEFF_HIT_B;
	crater -> Co = ?COEFF_HIT_C;
	home -> Co = ?COEFF_HIT_H;
	martian -> Co = ?COEFF_HIT_M;
	_ -> %io:format("cast hit unknown type ~w~n", [Type]),
	    Co = 0
    end,
    Uobj = if Dist =< 1.0e-6 -> Co * 1.0e+6 ;
	      true -> Co / Dist
	   end,
    {Uhome + Uobj + Uuns, Turn, Type}.
