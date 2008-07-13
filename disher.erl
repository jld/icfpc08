-module(disher).
-include("stuff.hrl").
-export([start/2]).
-ifndef(WORLD).
-define(WORLD,newworld).
-endif.

start(Pvst, Pdec) ->
    receive
	{initialize, Ini} -> 
	    Pworld = spawn(?WORLD, world, [Ini, Pvst]),
	    Pdec ! { set_world, Pworld, Ini },
	    trial(Pvst, Pworld, Pdec)
    end.

trial(Pvst, Pworld, Pdec) ->
    receive 
	{telemetry, _S, _O} = Tele ->
	    do_tele(Pvst, Pworld, Tele),
	    % sync!
	    Pvst ! {get_time, self()},
	    receive { time, _ } -> ok end,
	    % ready now
	    Pdec ! start_of_run,
	    run(Pvst, Pworld, Pdec, true)
    end.

nab_martians(Objs) ->
    lists:filter(fun ({martian, _}) -> true; (_) -> false end, Objs).

do_tele(Pvst, Pworld, {telemetry, VSt, Objs}) ->
    Martians = nab_martians(Objs),
    Pvst ! {set_vstate, VSt#vstate{ others = Martians }},
    lists:foreach(fun ({martian, _}) -> nope;
		      (Obj) -> Pworld ! {seen, Obj} end, Objs).

run(Pvst, Pworld, Pdec, Ticking) ->
    After = if Ticking -> 25; true -> infinity end,
    receive
	{telemetry, _S, _O} = Tele ->
	    do_tele(Pvst, Pworld, Tele),
	    if Ticking -> Pdec ! decide;
	       true -> ok
	    end,
	    run(Pvst, Pworld, Pdec, true);
	{end_of_run, _T, _S} = End ->
	    Pdec ! End,
	    Pworld ! rset,
	    trial(Pvst, Pworld, Pdec);
	{boulder_hit, _T} = BH ->
	    Pworld ! BH,
	    Pdec ! BH,
	    run(Pvst, Pworld, Pdec, Ticking);
	Other ->
	    Pdec ! Other,
	    run(Pvst, Pworld, Pdec, Ticking)
    after After ->
	    Pdec ! decide,
	    run(Pvst, Pworld, Pdec, false)
    end.
