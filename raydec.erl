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
    trial(Serv, Pcast, Pworld).

trial(Serv, Pcast, Pworld) ->
    receive 
	start_of_run ->
	    Pcast ! start_of_run,
	    comms:scmd(Serv, a),
	    comms:scmd(Serv, a),
	    receive {vstate, VS} ->
		    run(Serv, Pcast, Pworld, VS)
	    end
    end.

run(Serv, Pcast, Pworld, VS) ->
    receive
	decide ->
	    Pcast ! {get_best, self()},
	    receive
		{best, _Tang, _Tut, Ttu} = _Be ->
%% 		    io:format("Best: ~w -> ~w~n",
%% 			      [(VS#vstate.vmob)#mob.dir, Be]),
		    steerage:do_turn(Serv, VS, Ttu)
	    end,
	    run(Serv, Pcast, Pworld, VS);
	{vstate, NVS} ->
	    run(Serv, Pcast, Pworld, NVS);
	{end_of_run, _T, _S} ->
	    trial(Serv, Pcast, Pworld);
	Other -> 
	    io:format("raydec: unhandled message ~w~n", [Other]),
	    run(Serv, Pcast, Pworld, VS)
    end.


-define(COEFF_HOME, 1.0).
-define(COEFF_HIT_H, 10.0).
-define(COEFF_HIT_B, -50.0).
-define(COEFF_HIT_C, -60.0).
-define(COEFF_HIT_M, -40.0).
-define(COEFF_UNSAFE, -1.0).

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
    {But, Btu, _} = evaluate(ST, Bang),
    caster(ST, Bang, But, Btu).

caster(ST, Bang, But, Btu) ->
    receive
	{vstate, #vstate { vmob = VM }} ->
	    caster(ST#raydec_cst{ vm = VM }, Bang);
	{get_best, K} ->
	    K ! {best, Bang, But, Btu},
	    caster(ST, Bang, But, Btu);
	{end_of_run, _T, _S} ->
	    caster_0(ST#raydec_cst.pworld, ST#raydec_cst.init)
    after 0 ->
	    Rang = (ST#raydec_cst.vm)#mob.dir
		+ 180 * math:pow(2 * random:uniform() - 1, 3),
	    {Rut, Rtu, _Rty} = evaluate(ST, Rang),
	    if Rut > But -> 
%% 		    if Rty == crater ->
%% 			    io:format("O NOES!~n");
%% 		       true -> ok
%% 		    end,
		    caster(ST, Rang, Rut, Rtu);
	       true ->
		    caster(ST, Bang, But, Btu)
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
