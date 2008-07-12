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
-define(SPAN_DISMAY, 0.7).

-define(COEFF_HOME, 1.0).
-define(COEFF_HIT_H, 20.0).
-define(COEFF_HIT_B, -5.0).
-define(COEFF_HIT_C, -10.0).
-define(COEFF_HIT_M, -20.0). % XXX

min(X, Y) when X < Y -> X;
min(_X, Y) -> Y.

max(X, Y) when X > Y -> X;
max(_X, Y) -> Y.

caster(Pworld) ->
    receive
	start_of_run -> ok
    end,
    receive
	{vstate, #vstate { vmob = VM }} ->
	    Sang = VM#mob.dir,
	    caster(VM, Sang, evaluate(VM, Sang, Pworld), ?SPAN_INIT, Pworld)
    end.

caster(VM, Bang, But, Span, Pworld) ->
    receive
	{vstate, #vstate { vmob = NVM }} ->
	    if But > ?SPAN_DISMAY ->
		    NSpan = max(Span * ?SPAN_SCALE_GOOD, ?SPAN_MIN);
	       true ->
		    NSpan = min(Span * ?SPAN_SCALE_BAD, ?SPAN_MAX)
	    end,
	    io:format("Span: ~w -> ~w~n", [Span, NSpan]),
	    caster(NVM, Bang, evaluate(NVM, Bang, Pworld), NSpan, Pworld);
	{get_best, K} ->
	    K ! {best, Bang},
	    caster(VM, Bang, But, Span, Pworld);
	end_of_run ->
	    caster(Pworld)
    after 0 ->
	    Rang = Bang + Span * (2 * random:uniform() - 1),
	    Rut = evaluate(VM, Rang, Pworld),
	    if Rut > But -> 
		    %io:format("Better! ~w with ~w, over ~w with ~w (span ~w)~n",
			%      [Rang, Rut, Bang, But, Span]),
		    caster(VM, Rang, Rut, Span, Pworld);
	       true ->
		    caster(VM, Bang, But, Span, Pworld)
	    end
    end.
    
evaluate(#mob { x = HX, y = HY }, Cang, Pworld) ->
    Pworld ! {cast, HX, HY, Cang, self()},
    HR = math:sqrt(HX*HX + HY*HY),
    Crad = Cang * math:pi() / 180,
    Uhome = ?COEFF_HOME * -(HX/HR * math:cos(Crad) + HY/HR * math:sin(Crad)),
    % TODO: martians... oh hell.  Who's watching them?
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
    Uhome + Uobj.
