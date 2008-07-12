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
		    io:format("Turning.~n"),
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
-define(COEFF_MARTIAN, -4.0).
-define(POW_MARTIAN, 4.0).
-define(MARTIAN_REAR_CUTOFF, 0.866).
-define(MARTIAN_REAR_SCALE, 0.7).

-define(COEFF_HIT_H, 20.0).
-define(COEFF_HIT_B, -20.0).
-define(COEFF_HIT_C, -60.0).
-define(COEFF_HIT_M, -90.0). % XXX irrelevant

-record(raydec_cst, {vm, pworld, martians = [], span = 10.0 }).

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
%    io:format("Dist=~w TTL=~w Spin=~w~n", [Dist, TTL, Spin]),
    if Spin > 180 -> 180;
       true -> Spin
    end.

caster(Pworld) ->
    receive
	start_of_run -> ok
    end,
    receive
	{vstate, #vstate { vmob = VM }} ->
	    caster(#raydec_cst{ vm = VM, pworld = Pworld }, VM#mob.dir)
    end.

caster(ST, Bang) ->
    caster(ST, Bang, evaluate(ST, Bang)).

caster(ST, Bang, But) ->
    receive
	{vstate, #vstate { vmob = VM, others = Others }} ->
	    NST = ST#raydec_cst{ vm = VM, martians = Others },
	    caster(NST#raydec_cst{ span = figure_span(NST) }, Bang);
	{get_best, K} ->
	    K ! {best, Bang, But},
	    caster(ST, Bang, But);
	end_of_run ->
	    caster(ST#raydec_cst.pworld)
    after 0 ->
	    Rang = (ST#raydec_cst.vm)#mob.dir 
		+ ST#raydec_cst.span * (2 * random:uniform() - 1),
	    Rut = evaluate(ST, Rang),
	    if Rut > But -> 
		    caster(ST, Rang, Rut);
	       true ->
		    caster(ST, Bang, But)
	    end
    end.
		       

gradto(HX, HY, OX, OY, VX, VY) ->
    DX = OX - HX,
    DY = OY - HY,
    R = math:sqrt(DX*DX + DY*DY),
    (DX * VX + DY * VY) / R.

evaluate(#raydec_cst{ vm = #mob{ x = HX, y = HY },
		      pworld = Pworld, martians = Mar }, Cang) ->
    Pworld ! {cast, HX, HY, Cang, self()},
    Crad = Cang * math:pi() / 180,
    VX = math:cos(Crad), VY = math:sin(Crad),
    Uhome = ?COEFF_HOME * gradto(HX, HY, 0, 0, VX, VY),
    Umars = lists:foldl
	      (fun ({martian, #mob{ x = MX, y = MY }}, Acc) ->
		       %% XXX maybe this should use the unit relative velocity?
		       Gmars = gradto(HX, HY, MX, MY, VX, VY),
		       Xmars = if Gmars > 0 -> Gmars;
				  Gmars < -?MARTIAN_REAR_CUTOFF ->
				       (-Gmars - ?MARTIAN_REAR_CUTOFF) /
					   (1 - ?MARTIAN_REAR_CUTOFF) *
					   ?MARTIAN_REAR_SCALE;
				  true -> 0
			       end,
		       Acc + ?COEFF_MARTIAN * math:pow(Xmars, ?POW_MARTIAN);
		   (_, Acc) -> Acc end,
	       0, Mar),
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
    Uhome + Umars + Uobj.
