-module(simpledec).
-include("stuff.hrl").
-export([start/2]).

start(Serv, Pvst) ->
    receive
	{set_world, Pworld} -> ok
    end,
    Pvst ! {observe, self()},
    trial(Serv, Pworld).

trial(Serv, Pworld) ->
    receive
	start_of_run ->
	    comms:scmd(Serv, a),
	    comms:scmd(Serv, a),
	    run(Serv, Pworld)
    end.

fixang(D) when D < -180 -> fixang(D + 360);
fixang(D) when D >= 180 -> fixang(D - 360);
fixang(D) -> D.

run(Serv, Pworld) ->
    receive
	{vstate, #vstate{ vmob = VM }} ->
	    Dfwd = VM#mob.dir,
	    Dhome = math:atan2(-VM#mob.y, -VM#mob.x) * 180 / math:pi(),
	    Ddel = fixang(Dhome - Dfwd),
	    if Ddel > 0 -> comms:scmd(Serv, l);
	       true ->     comms:scmd(Serv, r)
	    end,
	    run(Serv, Pworld);
	{end_of_run, _T, _S} ->
	    trial(Serv, Pworld);
	Other -> 
	    io:format("simpledec: unhandled message ~w~n", [Other]),
	    run(Serv, Pworld)
    end.
