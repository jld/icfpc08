-module(simpledec).
-include("stuff.hrl").
-export([start/2]).

start(Serv, Pvst) ->
    receive
	{set_world, Pworld, _Ini} -> ok
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

run(Serv, Pworld) ->
    receive
	{vstate, #vstate{ vmob = VM } = VS} ->
	    Dhome = math:atan2(-VM#mob.y, -VM#mob.x) * 180 / math:pi(),
	    steerage:turn(Serv, VS, Dhome),
	    run(Serv, Pworld);
	{end_of_run, _T, _S} ->
	    trial(Serv, Pworld);
	Other -> 
	    io:format("simpledec: unhandled message ~w~n", [Other]),
	    run(Serv, Pworld)
    end.
