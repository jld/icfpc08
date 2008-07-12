-module(raydec).
-include("stuff.hrl").
-export([caster/1, start/2]).

start(Serv, Pvst) ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    receive
	{set_world, Pworld} -> ok
    end,
    Pcast = spawn_link(?MODULE, caster, [Pworld]),
    Pvst ! {observe, self()},
    Pvst ! {observe, Pcast}, 
    trial(Serv, Pcast, Pworld).

trial(Serv, Pcast, Pworld) ->
    receive 
	start_of_run ->
	    Pcast ! start_of_run,
	    comms:scmd(Serv, a),
	    comms:scmd(Serv, a),
	    run(Serv, Pcast, Pworld)
    end.

run(Serv, Pcast, Pworld) ->
    receive
	{vstate, VS} ->
	    Pcast ! {get_best, self()},
	    receive
		{best, Tang, _Tut} ->
%% 		    Pworld ! {get_init, self()}, % XXX hack
%% 		    receive 
%% 			{init, Ini} -> ok
%% 		    end,
%% 		    Spd = Ini#init.max_speed * (math:tanh(Tut) + 3) / 4,
%% 		    if Spd < (VS#vstate.vmob)#mob.speed ->
%% 			    io:format("Too fast!~n"),
%% 			    comms:scmd(Serv, b);
%% 		       true -> comms:scmd(Serv, a)
%% 		    end,
		    steerage:turn(Serv, VS, Tang)
	    end,
	    run(Serv, Pcast, Pworld);
	{end_of_run, _T, _S} ->
	    Pcast ! end_of_run,
	    trial(Serv, Pcast, Pworld);
	Other -> 
	    io:format("raydec: unhandled message ~w~n", [Other]),
	    run(Serv, Pcast, Pworld)
    end.


-define(COEFF_HOME, 1.0).
-define(POW_HOME, 0.0).
-define(COEFF_MARTIAN, -20.0).
-define(POW_MARTIAN, -1.0).

-define(COEFF_HIT_H, 20.0).
-define(COEFF_HIT_B, -20.0).
-define(COEFF_HIT_C, -40.0).
-define(COEFF_HIT_M, -80.0). % XXX irrelevant

-record(raydec_cst, {vm, pworld, grad, span = 10.0 }).

figure_span(#raydec_cst{ vm = VM, pworld = Pworld}) ->
    Pworld ! {cast, VM#mob.x, VM#mob.y, VM#mob.dir, self()},
    Pworld ! {get_init, self()}, % XXX hack
    receive 
	{hit, _D, {Dist, _Obj}} -> ok
    end,
    receive
	{init, Ini} -> ok
    end,
    TTL = Dist / (VM#mob.speed + 1.0e-12),
    Spin = Ini#init.max_hard_turn * TTL / 3,
    io:format("Dist=~w TTL=~w Spin=~w~n", [Dist, TTL, Spin]),
    if Spin > 180 -> 180;
       true -> Spin
    end.

caster(Pworld) ->
    receive
	start_of_run -> ok
    end,
    receive
	{vstate, #vstate { vmob = VM, others = Others }} ->
	    caster(#raydec_cst{ vm = VM, pworld = Pworld,
				grad = grad(VM, Others) }, VM#mob.dir)
    end.

caster(ST, Bang) ->
    caster(ST, Bang, evaluate(ST, Bang)).

caster(ST, Bang, But) ->
    receive
	{vstate, #vstate { vmob = VM, others = Others }} ->
	    NST = ST#raydec_cst{ vm = VM, grad = grad(VM, Others) }, % Ick.
	    caster(NST#raydec_cst{ span = figure_span(NST) }, Bang);
	{get_best, K} ->
	    K ! {best, Bang, But},
	    caster(ST, Bang, But);
	end_of_run ->
	    caster(ST#raydec_cst.pworld)
    after 0 ->
	    % XXX should randomize seed
	    Rang = (ST#raydec_cst.vm)#mob.dir 
		+ ST#raydec_cst.span * (2 * random:uniform() - 1),
	    Rut = evaluate(ST, Rang),
	    if Rut > But -> 
%% 		    if abs(Rang-Bang) > 5 ->
%% 			    io:format("~w (~w) > ~w (~w)~n",
%% 				      [Rang, Rut, Bang, But]);
%% 		       true -> ok
%% 		    end,
		    caster(ST, Rang, Rut);
	       true ->
		    caster(ST, Bang, But)
	    end
    end.

potential(HX, HY, OX, OY, Pw, Sc) ->
    DX = OX - HX,
    DY = OY - HY,
    Rc = Sc * math:pow(HX*HX + HY*HY, (Pw-1)/2),
    {Rc * DX, Rc * DY}.

gplus({X1, Y1}, {X2, Y2}) -> {X1 + X2, Y1 + Y2}.

grad(#mob{ x = HX, y = HY }, Others) ->
    Ghome = potential(HX, HY, 0, 0, ?POW_HOME, ?COEFF_HOME),
    lists:foldl(fun ({martian, #mob{ x = OX, y = OY }}, Acc) ->
			gplus(potential(HX, HY, OX, OY, ?POW_MARTIAN, 
					?COEFF_MARTIAN), Acc);
		    (_, Acc) -> Acc end, Ghome, Others).

evaluate(#raydec_cst{ vm = #mob{ x = HX, y = HY },
		      pworld = Pworld, grad = {GX, GY} }, Cang) ->
    Pworld ! {cast, HX, HY, Cang, self()},
    Crad = Cang * math:pi() / 180,
    VX = math:cos(Crad), VY = math:sin(Crad),
    Ugrad = GX * VX + GY * VY,
    receive 
	{hit, _D, {Dist, Obj}} -> lovely
    end,
    case Obj of 
	[] -> Uobj = 0;
	{horiz, _Y} -> Uobj = 0;
	{vert, _X} -> Uobj = 0;
	{Type, #mob {  }} ->
	    % io:format("HIT: ~w~n", [Type]),
	    case Type of
		boulder -> Co = ?COEFF_HIT_B;
		crater -> Co = ?COEFF_HIT_C;
		home -> Co = ?COEFF_HIT_H;
		martian -> Co = ?COEFF_HIT_M;
		_ -> io:format("cast hit unknown type ~w~n", [Type]),
		     Co = 0
	    end,
	    Uobj = Co / Dist;
	_ -> io:format("cast hit unknown object ~w~n", [Obj]),
	     Uobj = 0
    end,
    Ugrad + Uobj.
