-module(steerage).
-include("stuff.hrl").
-export([turn/3]).
-define(BEAM_WIDTH, 5.0).

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

turn(Serv, VS, Dtarg) ->
    % XXX do not use twice without intervening telemetry
    Dfwd = (VS#vstate.vmob)#mob.dir,
    Ddel = fixang(Dtarg - Dfwd),
    Ttarg = trunc(Ddel / ?BEAM_WIDTH),
    send_turn(Serv, VS#vstate.vctlt, Ttarg).

%% TODO: speed control to avoid spinning forever around the center
