
:- initialization(main).

:- op(15, xfx, '=>').
=>(X,Y).

main:- consult('main.pl'),
	consult('inciso 2.pl'),
	consult('inciso 3.pl'),
	consult('inciso 4.pl').
	
rb(Y):- open_kb('bd.txt',Y).
guardarBD(Y):- save_kb('bd.txt',Y).

primerTermino(X,Y) :-
	X = =>(Y,W).
segundoTermino(X,Y):- 
	X = =>(W,Y).

valor(X,[],Y):- fail.
valor(X,[H|T],Y):- primerTermino(H,X), segundoTermino(H,Y), !.
valor(X,[H|T],Y):- primerTermino(H,Z), X\=Z, valor(X,T,Y).

cabeza([H|_],Y):- Y=H.
cabeza(X,Y):- Y=X.
cola([H|T],Y):- Y=T.

sus(_,_,[],[]).
sus(X,Y,[X|T],[Y|S]):-!,sus(X,Y,T,S).
sus(X,Y,[Z|T],[Z|S]):-sus(X,Y,T,S).

eliminaClase(X,[X|T],T).
eliminaClase(X,[H|T],[H|T1]):-
	eliminaClase(X,T,T1).

concatenar([],L,L).
concatenar([X|L1],L2,[X|L3]):-concatenar(L1,L2,L3).

quieroClase(X,[H|T],P):-
	nth0(2,H,Props),
	cabeza(Props,S),
	segundoTermino(S,X)->
		P=H ; quieroClase(X,T,P).

set([],[]).
set([H|T],[H|Out]) :-
	primerTermino(H,Y),
	Cp= Y=>X,
	not(member(Cp,T)),
    set(T,Out).
set([H|T],Out) :-
	primerTermino(H,Y),
	Cp= Y=>X,
    member(Cp,T),
    set(T,Out).

nuevoId(Id) :-
	rb(X),
	reverse(X,Y),
	[H|T] = Y,
	[A|B] = H,
	segundoTermino(A,Idmax), 
	atom_length(Idmax, Len), 
	L is Len - 1, 
	sub_atom(Idmax, 1, L, W, S), 
	atom_number(S,S1), 
	Id is S1 + 1, nl, write('Nuevo Id listo...'),nl.

esCorrecto([]).
esCorrecto([H|T]) :-
	H = =>(X,Y),
	esCorrecto(T).




