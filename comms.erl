-module(comms).
-include("stuff.hrl").
-export([msgfmt/0, msgfmt/1, 
	 msgsplit/0, msgsplit/1,
	 to_struct/1, to_fields/1, list_to_num/1,
	 binsplit/2]).

msgfmt() ->
    receive
	{set_proc, P} -> msgfmt(P)
    end.
msgfmt(P) ->
    receive 
	eof -> bees;
	{line, IOL} -> P ! to_struct(to_fields(list_to_binary(IOL))),
		       msgfmt(P);
	{set_proc, Q} -> msgfmt(Q)
    end.

list_to_num(S) ->
    case io_lib:fread("~d", S) of
	{ok, [I], []} -> I;
	_ -> list_to_float(S)  
    end.

bton(B) -> list_to_num(binary_to_list(B)).
btoa(B) -> list_to_atom(binary_to_list(B)).

to_struct([<<"I">>,DX,DY,TL,MinSen,MaxSen,MaxSpd,MaxTurn,MaxHT]) ->
    {initialize, #init{
       x_limit = bton(DX)/2, y_limit = bton(DY)/2, time_limit = bton(TL),
       min_sensor = bton(MinSen), max_sensor = bton(MaxSen), 
       max_speed = bton(MaxSpd),
       max_turn = bton(MaxTurn), max_hard_turn = bton(MaxHT) }};

to_struct([<<"T">>,T,VC,VX,VY,VD,VS |Objs]) ->
    {telemetry,
     #vstate { time = bton(T), vctl = btoa(VC),
	       vmob = #mob{ x = bton(VX), y = bton(VY),
			    dir = bton(VD), speed = bton(VS) }},
     objs_to_struct(Objs)};

to_struct([<<"B">>,T]) -> {boulder_hit, bton(T)};
to_struct([<<"C">>,T]) -> {crater_hit, bton(T)};
to_struct([<<"K">>,T]) -> {killed, bton(T)};
to_struct([<<"S">>,T]) -> {success, bton(T)};
to_struct([<<"E">>,T,S]) -> {end_of_run, bton(T), bton(S)}.

objs_to_struct([]) -> [];
objs_to_struct([<<"b">> |Rest]) -> objs_to_struct_st(boulder, Rest);
objs_to_struct([<<"c">> |Rest]) -> objs_to_struct_st(crater, Rest);
objs_to_struct([<<"h">> |Rest]) -> objs_to_struct_st(home, Rest);
objs_to_struct([<<"m">> |Rest]) -> 
    objs_to_struct_dy(martian, ?RAD_MARTIAN, Rest).

objs_to_struct_st(Type, [X,Y,R |Objs]) ->
    [{Type, #mob{ x = bton(X), y = bton(Y), r = bton(R) }}
     |objs_to_struct(Objs)].

objs_to_struct_dy(Type, R, [X,Y,D,S |Objs]) ->
    [{Type, #mob{ x = bton(X), y = bton(Y), dir = bton(D), speed = bton(S),
		  r = R }} |objs_to_struct(Objs)].


to_fields(<<>>) -> [];
to_fields(B) ->
    case binsplit($ , B) of
	{C, D} -> [C |to_fields(D)];
	C -> [C]
    end.


%% Reblocks incoming TCP data as a series of semicolon-terminated lines.
msgsplit() ->
    receive
	{set_proc, P} -> msgsplit(P)
    end.
msgsplit(P) ->
    msgsplit(P, []).
msgsplit(P, A) ->
    receive
	{tcp, _, D} -> msgsplit(P, A, D);
	{tcp_closed, _} -> P ! eof, ok;
	{tcp_error, _, _} -> P ! eof, error;
	{set_proc, Q} -> msgsplit(Q, A)
    end.
msgsplit(P, A, D) ->
    case binsplit($;, D) of 
	{B, C} -> P ! {line, [A|B]}, msgsplit(P, [], C);
	 B     -> msgsplit(P, [A|B])
    end.

% Splits a binary at the first occurrence of a given byte delimiter,
% if present.
binsplit(D, B) -> binsplit(D, B, 0, size(B)).
binsplit(_, B, I, E) when I >= E -> B;
binsplit(D, B, I, E) ->
    case B of 
	<<A:I/binary, D, Z/binary>> -> {A, Z};
	_ -> binsplit(D, B, I+1, E)
    end.
