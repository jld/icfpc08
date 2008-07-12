-module(raydec).
-include("stuff.hrl").
-export([caster/1, start/2]).

start(Serv, Pvst) ->
    receive
	{set_world, Pworld} -> ok
    end,
    Pcast = spawn(?MODULE, caster, [Pworld]),
    erlang:monitor(process, Pcast),
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
		{best, Tang} ->
		    steerage:turn(Serv, VS, Tang);
		{'DOWN', _, _, _, _} = Down ->
		    io:format("DOOM DOOM DOOM ~w~n", [Down])
	    end,
	    run(Serv, Pcast, Pworld);
	{end_of_run, _T, _S} ->
	    Pcast ! end_of_run,
	    trial(Serv, Pcast, Pworld);
	Other -> 
	    io:format("raydec: unhandled message ~w~n", [Other]),
	    run(Serv, Pcast, Pworld)
    end.


-define(SPAN_INIT, 10.0).
-define(SPAN_MIN, 5.0).
-define(SPAN_MAX, 180.0).
-define(SPAN_SCALE_GOOD, 0.5).
-define(SPAN_SCALE_BAD, 2.0).
-define(SPAN_DISMAY, 0.7). % XXX should probably be adaptive instead of fixed

-define(COEFF_HOME, 1.0).
-define(POW_HOME, 0.0).
-define(COEFF_MARTIAN, -20.0).
-define(POW_MARTIAN, -1.0).

-define(COEFF_HIT_H, 20.0).
-define(COEFF_HIT_B, -5.0).
-define(COEFF_HIT_C, -10.0).
-define(COEFF_HIT_M, -20.0). % XXX irrelevant

min(X, Y) when X < Y -> X;
min(_X, Y) -> Y.

max(X, Y) when X > Y -> X;
max(_X, Y) -> Y.

-record(raydec_cst, {vm, pworld, grad}).

caster(Pworld) ->
    receive
	start_of_run -> ok
    end,
    receive
	{vstate, #vstate { vmob = VM, others = Others }} ->
	    caster(#raydec_cst{ vm = VM, pworld = Pworld,
				grad = grad(VM, Others) },
		   VM#mob.dir, ?SPAN_INIT)
    end.

caster(ST, Bang, Span) ->
    caster(ST, Bang, evaluate(ST, Bang), Span).

caster(ST, Bang, But, Span) ->
    receive
	{vstate, #vstate { vmob = VM, others = Others }} ->
	    if But > ?SPAN_DISMAY ->
		    NSpan = max(Span * ?SPAN_SCALE_GOOD, ?SPAN_MIN);
	       true ->
		    NSpan = min(Span * ?SPAN_SCALE_BAD, ?SPAN_MAX)
	    end,
	    io:format("Span: ~w -> ~w~n", [Span, NSpan]),
	    caster(ST#raydec_cst{ vm = VM, grad = grad(VM, Others) },
		   Bang, NSpan);
	{get_best, K} ->
	    K ! {best, Bang},
	    caster(ST, Bang, But, Span);
	end_of_run ->
	    caster(ST#raydec_cst.pworld)
    after 0 ->
	    % XXX should randomize seed
	    Rang = Bang + Span * (2 * random:uniform() - 1),
	    Rut = evaluate(ST, Rang),
	    if Rut > But -> 
		    caster(ST, Rang, Rut, Span);
	       true ->
		    caster(ST, Bang, But, Span)
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
