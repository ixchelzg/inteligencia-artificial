:- initialization(main).
:- op(15, xfx, '=>').
=>(X,Y).
main:- consult('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/main.pl'),
	consult('/home/ixchel/git/UNAM/IA/Proyecto/eliminar.pl'),
	consult('/home/ixchel/git/UNAM/IA/Proyecto/modificar.pl').
	
rb(Y):- open_kb('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/bd.txt',Y).
guardarBD(Y):- save_kb('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/bd.txt',Y).

primerTermino(X,Y):- X = W=>Z, Y=W.
segundoTermino(X,Y):- X = W=>Z, Y=Z.
valor(X,[],Y):- fail.
valor(X,[H|T],Y):- primerTermino(H,X), segundoTermino(H,Y), !.
valor(X,[H|T],Y):- primerTermino(H,Z), X\=Z, valor(X,T,Y).

cabeza([H|_],Y):- Y=H.
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