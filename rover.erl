-module(rover).
-export([start/1, start/2, call_server/3]).

start([Host, SPort]) ->
    start(Host, list_to_integer(SPort)).

start(Host, Port) ->
    Pmspl = spawn(?MODULE, call_server, [Host, Port, self()]),
    Pmfmt = spawn(comms, msgfmt, []),
    Pvst = spawn(gis, vstate, []),
    receive
	{ server_is, Serv } -> ok
    end,
    Pdec = spawn(simpledec, start, [Serv, Pvst]),
    Pdsh = spawn(disher, start, [Pvst, Pdec]),
    Pmspl ! {set_proc, Pmfmt},
    Pmfmt ! {set_proc, Pdsh},
    receive
	% TODO: supervision
	never -> ok
    end.

call_server(Host, Port, K) ->
    {ok, S} = gen_tcp:connect(Host, Port, [binary, {nodelay, true}]),
    K ! { server_is, S },
    comms:msgsplit().
