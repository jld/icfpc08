-module(disher).
-export([start/2]).

start(Pvst, Pdec) ->
    receive
	{initialize, Ini} -> 
	    Pworld = spawn(gis, world, [Ini]),
	    Pdec ! { set_world, Pworld },
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
	    run(Pvst, Pworld, Pdec)
    end.

do_tele(Pvst, Pworld, {telemetry, VSt, Objs}) ->
    Pvst ! {set_vstate, VSt},
    lists:foreach(fun (Obj) -> Pworld ! {seen, Obj} end, Objs).

run(Pvst, Pworld, Pdec) ->
    receive
	{telemetry, _S, _O} = Tele ->
	    do_tele(Pvst, Pworld, Tele),
	    run(Pvst, Pworld, Pdec);
	{end_of_run, _T, _S} = End ->
	    Pdec ! End,
	    trial(Pvst, Pworld, Pdec);
	Other ->
	    Pdec ! Other,
	    run(Pvst, Pworld, Pdec)
    end.
	    
