:- initialization(main).
:- op(15, xfx, '=>').
=>(X,Y).
main:- consult('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/main.pl').
rb(Y):- open_kb('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/bd.txt',Y).
guardarBD(Y):- save_kb('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/bd.txt',Y).

primerTermino(X,Y):- X = W=>Z, Y=W.
segundoTermino(X,Y):- X = W=>Z, Y=Z.
valor(X,[],Y):- fail.
valor(X,[H|T],Y):- primerTermino(H,X), segundoTermino(H,Y), !.
valor(X,[H|T],Y):- primerTermino(H,Z), X\=Z, valor(X,T,Y).

cabeza([H|T],Y):- Y=H.
cola([H|T],Y):- Y=T.
cabeza(X,Y):- Y=X.

sus(_,_,[],[]).
sus(X,Y,[X|T],[Y|S]):-!,sus(X,Y,T,S).
sus(X,Y,[Z|T],[Z|S]):-sus(X,Y,T,S).

eliminaClase(X,[X|T],T).
eliminaClase(X,[H|T],[H|T1]):-
	eliminaClase(X,T,T1).

concatenar([],L,L).
concatenar([X|L1],L2,[X|L3]):-concatenar(L1,L2,L3).

quieroClase(X,[],P).
quieroClase(X,[H|T],P):-
	nth0(2,H,Props),
	cabeza(Props,S),
	segundoTermino(S,X),
	P=H;
	quieroClase(X,T,P).

elimina(X):-
	rb(W),
	quieroClase(X,W,P),
	nth0(2,P,Pr),
	nth0(3,P,Rel),
	cola(Rel, Rels),
	cola(Pr,Props),
	valor(id_padre,P,Pop),
	valor(id,P,ID),
	Pop\=c0,
	crearNuevaListaInicio(ID,Props,Rels,Pop),
	rb(W1),
	eliminaClase(P,W1,S),
	guardarBD(S).

eliminaPropiedad(X,Y):-
	rb(W),
	quieroClase(Y,W,P),
	crearNuevaListaProps(P,X).

eliminaRelacion(X,Y):-
	rb(W),
	quieroClase(Y,W,P),
	crearNuevaListaRels(P,X).

modificaNombre(X,Y):-
	rb(W),
	quieroClase(Y,W,P),
	creaNuevoNombre(P,X).

modificaPropiedad(P,X,P1):-
	rb(W),
	quieroClase(X,W,Cl),
	nth0(0,Cl,N),
	nth0(1,Cl,Pa),
	nth0(2,Cl,Props),
	nth0(3,Cl,Rels),
	PB= P=>P1,
	Elem= P=>Xv,
	select(Elem,Props,L1),
	sus(Elem,PB,Props,S),
	H1=[N,Pa,S,Rels],
	rb(W),
	sus(Cl,H1,W,S1),
	guardarBD(S1).

modificaRelacion(P,X,P1):-
	rb(W),
	quieroClase(X,W,Cl),
	quieroClase(P1,W,Cl1),
	nth0(0,Cl,N),
	nth0(1,Cl,Pa),
	nth0(2,Cl,Props),
	nth0(3,Cl,Rels),
	nth0(0,Cl1,IDCl1),
	segundoTermino(IDCl1,Id),
	PB= P=>Id,
	Elem= P=>Xv,
	select(Elem,Rels,L1),
	sus(Elem,PB,Rels,S),
	H1=[N,Pa,Props,S],
	rb(W),
	sus(Cl,H1,W,S1),
	guardarBD(S1).

creaNuevoNombre(P,X):-
	nth0(0,P,N),
	nth0(1,P,Pa),
	nth0(2,P,Props),
	nth0(3,P,Rels),
	PB= nombre=>X,
	cabeza(Props,Ca),
	sus(Ca,PB,Props,S),
	H1=[N,Pa,S,Rels],
	rb(W),
	sus(P,H1,W,S1),
	guardarBD(S1).

crearNuevaListaProps(X,Y):-
	nth0(0,X,N),
	nth0(1,X,P),
	nth0(2,X,Props),
	nth0(3,X,Rels),
	PB= Y=>XX,
	Y \= nombre,
	eliminaClase(PB,Props,P1),
	H1=[N,P,P1,Rels],
	rb(W),
	sus(X,H1,W,S),
	guardarBD(S).

crearNuevaListaProps(X,Y):-
	nth0(0,X,N),
	nth0(1,X,P),
	nth0(2,X,Props),
	nth0(3,X,Rels),
	PB= Y=>XX,
	eliminaClase(PB,Props,P1),
	H1=[N,P,P1,Rels],
	rb(W),
	sus(X,H1,W,S),
	guardarBD(S).

crearNuevaListaRels(X,Y):-
	nth0(0,X,N),
	nth0(1,X,P),
	nth0(2,X,Props),
	nth0(3,X,Rels),
	PB= Y=>XX,
	eliminaClase(PB,Rels,P1),
	H1=[N,P,Props,P1],
	rb(W),
	sus(X,H1,W,S),
	guardarBD(S).

crearNuevaListaInicio(X,M,R,Pop):- rb(W), crearNuevaLista(X,M,R,Pop,W).
crearNuevaLista(X,M,R,Pop,[]).
crearNuevaLista(X,M,R,Pop,[H|T]):-
	(
		valor(id_padre,H,X),
		nth0(2,H,P2),
		nth0(3,H,R2),
		concatenar(M,P2,L1),
		concatenar(R,R2,L2),
		nth0(0,H,ID),
		H1=[ID,id_padre=>Pop,L1,L2],
		rb(W),
		sus(H,H1,W,S),
		guardarBD(S),
		crearNuevaLista(X,M,R,Pop,T)
	);(
		nth0(0,H,PP),
		nth0(3,H,Rel),
		RR= XXX=>X,
		member(RR,Rel),
		eliminaClase(RR,Rel,Rs),
		nth0(2,H,P2),
		nth0(0,H,ID),
		H1=[ID,PP,P2,Rs],
		rb(W),
		sus(H,H1,W,S),
		guardarBD(S),
		crearNuevaLista(X,M,R,Pop,T)
	);crearNuevaLista(X,M,R,Pop,T).

