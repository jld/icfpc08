-module(comms).
-include("stuff.hrl").

msgfmt(P) ->
    receive 
	eof -> bees;
	{line, IOL} -> P ! to_struct(to_fields(list_to_binary(IOL))),
		       msgfmt(P);
	{set_proc, Q} -> msgfmt(Q)
    end.

btof(B) -> list_to_float(binary_to_list(B)).
btoa(B) -> list_to_atom(binary_to_list(B)).

to_struct([<<"I">>,DX,DY,TL,MinSen,MaxSen,MaxSpd,MaxTurn,MaxHT]) ->
    {initialize, #init{
       x_limit = btof(DX)/2, y_limit = btof(DY)/2, time_limit = btof(TL),
       min_sensor = btof(MinSen), max_sensor = btof(MaxSen), 
       max_speed = btof(MaxSpd),
       max_turn = btof(MaxTurn), max_hard_turn = btof(MaxHT) }};

to_struct([<<"T">>,T,VC,VX,VY,VD,VS |Objs]) ->
    {telemetry,
     #vstate { time = btof(T), vctl = btoa(VC),
	       vmob = #mob{ x = btof(VX), y = btof(VY),
			    dir = btof(VD), speed = btof(VS) }},
     objs_to_struct(Objs)};

to_struct([<<"B">>,T]) -> {boulder_hit, btof(T)};
to_struct([<<"C">>,T]) -> {crater_hit, btof(T)};
to_struct([<<"K">>,T]) -> {killed, btof(T)};
to_struct([<<"S">>,T]) -> {success, btof(T)};
to_struct([<<"E">>,T,S]) -> {end_of_run, btof(T), btof(S)}.

objs_to_struct([<<"b">> |Rest]) -> objs_to_struct_st(boulder, Rest);
objs_to_struct([<<"c">> |Rest]) -> objs_to_struct_st(crater, Rest);
objs_to_struct([<<"h">> |Rest]) -> objs_to_struct_st(home, Rest);
objs_to_struct([<<"m">> |Rest]) -> 
    objs_to_struct_dy(martian, ?RAD_MARTIAN, Rest).

objs_to_struct_st(Type, [X,Y,R |Objs]) ->
    [{Type, #mob{ x = btof(X), y = btof(X), r = btof(R) }}
     |objs_to_struct(Objs)].

objs_to_struct_dy(Type, R, [X,Y,D,S |Objs]) ->
    [{Type, #mob{ x = btof(X), y = btof(X), dir = btof(D), speed = btof(S),
		  r = R }} |objs_to_struct(Objs)].


to_fields(<<>>) -> [];
to_fields(B) ->
    case binsplit($ , B) of
	{B, C} -> [B |to_fields(C)];
	B -> [B]
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
