-module(steerage).
-include("stuff.hrl").
-export([do_turn/3, steer/2, turn/3]).
-define(BEAM_WIDTH, 5.0). % XXX this is a wild guess & may need tuning

fixang(D) when D < -180.000001 -> fixang(D + 360);
fixang(D) when D >= 180.000001 -> fixang(D - 360);
fixang(D) -> D.

send_turn(Serv, TC, TG) when (TC < TG) and (TC < 2) -> 
%    io:format("l!~n"),
    comms:scmd(Serv, l), send_turn(Serv, TC+1, TG);
send_turn(Serv, TC, TG) when (TC > TG) and (TC > -2) ->
%    io:format("r!~n"),
    comms:scmd(Serv, r), send_turn(Serv, TC-1, TG);
send_turn(_Serv, _TC, _TG) -> ok.

do_turn(Serv, VS, TG) ->
    % XXX do not use twice without intervening telemetry; this should be fixed
    TC = VS#vstate.vctlt,
    if TC /= TG ->
	    io:format("Turning from ~w to ~w.~n", [TC, TG]);
       true -> ok
    end,
    send_turn(Serv, TC, TG).

steer(Dfwd, Dtarg) ->
    Ddel = fixang(Dtarg - Dfwd),
    trunc(Ddel / ?BEAM_WIDTH).
    
turn(Serv, VS, Dtarg) ->
    Dfwd = (VS#vstate.vmob)#mob.dir,
    do_turn(Serv, VS, steer(Dfwd, Dtarg)).

%% TODO: speed control to avoid spinning forever around the center
