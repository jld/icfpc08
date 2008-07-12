-module(rover).
-export([start/1, start/3, call_server/3]).
-ifndef(DECIDER).
-define(DECIDER, raydec).
-endif.

start([Host, SPort |Stuff]) ->
    start(Host, list_to_integer(SPort), Stuff).

start(Host, Port, Stuff) ->
    Pmspl = spawn(?MODULE, call_server, [Host, Port, self()]),
    Pmfmt = spawn(comms, msgfmt, []),
    Pvst = spawn(gis, vstate, []),
    receive
	{ server_is, Serv } -> ok
    end,
    Pdec = spawn(?DECIDER, start, [Serv, Pvst]),
    Pdsh = spawn(disher, start, [Pvst, Pdec]),
    Pmspl ! {set_proc, Pmfmt},
    Pmfmt ! {set_proc, Pdsh},
    receive
	done -> 
	    case Stuff of 
		[M, F |A] -> apply(list_to_atom(M), list_to_atom(F), A);
		_ -> whatever
	    end
    end.

call_server(Host, Port, K) ->
    {ok, S} = gen_tcp:connect(Host, Port, [binary, {nodelay, true}]),
    K ! { server_is, S },
    comms:msgsplit(),
    K ! done.

