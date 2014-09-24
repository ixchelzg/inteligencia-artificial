:- initialization(main).
:- op(15, xfx, '=>').
=>(X,Y).

main:-
	consult('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/main.pl').
rb(Y):-
	open_kb('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/bd.txt',Y).
guardarBD(Y):-
	save_kb('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/bd.txt',Y).

primertermino(X,Y):- X = W=>Z, Y=W.
segundotermino(X,Y):- X = W=>Z, Y=Z.
valor(X,[],Y):- fail.
valor(X,[H|T],Y):- primertermino(H,X), segundotermino(H,Y), !.
valor(X,[H|T],Y):- primertermino(H,Z), X\=Z, valor(X,T,Y).

cabeza([H|T],Y):- Y=H.
cola([H|T],Y):- Y=T.
cabeza(X,Y):- Y=X.
clasesHijas(X,Y):- findall(W,clase(W,X,_,_,_),Y).
clasesHijasRecursivoCabeza(X,[]):-
							findall(W,clase(W,X,_,_,_),[]),!.
clasesHijasRecursivoCabeza(X,Y):- 
							findall(W,clase(W,X,_,_,_),R),
							cabeza(R,F),
							cola(R,G),
							clasesHijasRecursivoCabeza(F,S),
							clasesHijasRecursivoCola(G,M),
							append(S,M,L),
							append(L,R,Y),!.
clasesHijasRecursivoCola([],Y):-Y = []. 
clasesHijasRecursivoCola([H|T],Y):- 
							clasesHijasRecursivoCabeza(H,S),
							clasesHijasRecursivoCola(T,M),
							append(S,M,Y),!.
							
recorreLista([]):-!.							
recorreLista([H|T]):-write(H),nl,recorreLista(T),!.

recorreBaseInicio():-rb(Y), recorreBase(Y).
recorreBase([]):-!.							
recorreBase([H|T]):-write(H), nl, recorreBase(T),!.

regresaBaseInicio():-rb(Y), recorreBase(Y).
regresaBase([]):-!.							
regresaBase([H|T]):-write(H), nl, regresaBase(T),!.

anade([],X,[X]).
anade(X,[],[X]).
anade(X,[H|T],[H|Y]):- anade(X,T,Y).
anade([H|T],X,[H|Y]):- anade(X,T,Y).

sacaNombresDeClasesInicio(Y):- rb(W),  sacaNombresDeClases(W,Y).
sacaNombresDeClases([],Y):- Y = [].
sacaNombresDeClases([H|T],Y):- valor(nombre,H,R), 
								sacaNombresDeClases(T,S), 
								anade(S,R,Y),!.
								
sacaNombresDePadresInicio(Y):- rb(W),  sacaNombresDePadres(W,Y).
sacaNombresDePadres([],Y):- Y = [].
sacaNombresDePadres([H|T],Y):- valor(padre,H,R), 
								sacaNombresDePadres(T,S), 
								anade(S,R,Y),!.
								
sacaClasesHijasInicio(X,Y):- rb(W), sacaClasesHijas(X,W,Y).
sacaClasesHijas(X,[],Y):-  Y = [].
sacaClasesHijas(X,[H|T],Y):- 
								valor(padre,H,X), 
								valor(nombre,H,S),
								sacaClasesHijas(X,T,R),
								anade(S,R,Y);
								sacaClasesHijas(X,T,Y), !.

sacaClasesHijasRecursivoInicioFlat(X,Y):- sacaClasesHijasRecursivoInicio(X,Z), flatten(Z,Y).
								
sacaClasesHijasRecursivoInicio(X,Y):- rb(W), sacaClasesHijasRecursivo(X,W,Y).
sacaClasesHijasRecursivo(X,[],Y):- Y=[].
sacaClasesHijasRecursivo(X,[H|T],Y):- 
								valor(padre,H,X), 
								valor(nombre,H,S),
								sacaClasesHijasRecursivo(X,T,R),
								anade(R,S,E),
								sacaClasesHijasRecursivoInicio(S,V),
								anade(V,E,Y);
								sacaClasesHijasRecursivo(X,T,Y), !.

iteraIndividuos([],Y).
iteraIndividuos([H|T],Y):- 
								cabeza(H,S),
								segundotermino(S,M),
								iteraIndividuos(T,R),
								anade(M,R,Y),!
								.
								
sacaIndividuosDeClase([],Y).								
sacaIndividuosDeClase([H|T],Y):-
								cabeza(H,W),
								cabeza(W,M),
								primertermino(M,F),
								F == individuo,
								segundotermino(M,S),
								iteraIndividuos(H,Y),
								sacaIndividuosDeClase(T,R),
								anade(S,R,Y);
								sacaIndividuosDeClase(T,Y),!.
			
sacaObjetosHijosDeClaseInicio(X,Y):- rb(W), sacaObjetosHijosDeClase(X,W,Y).
sacaObjetosHijosDeClase(X,[],Y):- Y=[].
sacaObjetosHijosDeClase(X,[H|T],Y):- 
								valor(nombre,H,X),
								sacaIndividuosDeClase(H,Y),!;
								sacaObjetosHijosDeClase(X,T,Y),!.

sacaObjetosHijosDeClaseRecursivoInicio(X,Y):-
								sacaClasesHijasRecursivoInicioFlat(X,S), 
								write('S: '),
								write(S),
								nl,
								cabeza(S,U),
								write('U: '),
								write(U),
								nl,
								sacaObjetosHijosDeClaseRecursivo(X,S,Y).




sacaObjetosHijosDeClaseRecursivo(X,[],Y):- Y=[].						
sacaObjetosHijosDeClaseRecursivo(X,[],Y):- 	
								
								write('S: '),
								write(S),
								nl
								.


sus(_,_,[],[]).
sus(X,Y,[X|T],[Y|S]):-!,sus(X,Y,T,S).
sus(X,Y,[Z|T],[Z|S]):-sus(X,Y,T,S).

eliminaClase(X,[X|T],T).
eliminaClase(X,[H|T],[H|T1]):-
	eliminaClase(X,T,T1).

concatenar([],L,L).
concatenar([X|L1],L2,[X|L3]):-concatenar(L1,L2,L3).


elimina(X):-
	rb(W),
	(
		MM=[nombre=>X,_,_,_,_],
		quieroPadre(MM,W,P),
		nth0(2,P,Pr),
		nth0(3,P,Rel),
		valor(padre,P,Pop),
		Pop\=top,
		crearNuevaListaInicio(X,Pr,Rel,Pop),
		rb(W1),
		eliminaClase(MM,W1,S),
		guardarBD(S)
	); crearNuevaListaInicioInd(X).


eliminaPropiedad(X,Y):-
	(
		rb(W),
		MM=[nombre=>Y,_,_,_,_],
		quieroPadre(MM,W,P),
		crearNuevaListaProps(P,X)
	);(
		rb(W),
		write('aqui nomas'),nl,
		crearNuevaListaIndProps(X,Y,W,W)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Recibe un Nombre de la nueva clase y el que sería su padre
anadeClase(Nom,Pad) :-
	rb(X),
	quieroClase(Pad,X,Cla),
	nth0(0,Cla,IdP),
	segundoTermino(IdP,IdPa),
	IdPad = id_padre=>IdPa,
	nuevoId(I),
	atomic_concat(c,I,Id),
	Pr = [nombre=>Nom],
	Clase = [id=>Id,IdPad,Pr,[]],
	sustPadre(IdPad,X,id_padre=>Id),
	reverse(X,Y),
	Z = [Clase|Y],
	reverse(Z,S),
	guardarBD(S).
% Recibe el Nombre, el Padre, y las listas de Props y Rels
anadeClase(Nom,Pad,Props,Rels) :-
	rb(X),
	quieroClase(Pad,X,Cla),
	nth0(0,Cla,IdP),
	segundoTermino(IdP,IdPa),
	IdPad = id_padre=>IdPa,
	nuevoId(I),
	atomic_concat(c,I,Id),
	Pr = [nombre=>Nom|Props], [H|T] = Rels,
	Clase = [id=>Id,IdPad,Pr,Rels],
	sustPadre(IdPad,X,id_padre=>Id),
	reverse(X,Y),
	Z = [Clase|Y],
	reverse(Z,S),
	guardarBD(S).

% Recibe el Nombre del Objeto, y quien sería su Padre
anadeObjeto(Nom,Pad) :-
	rb(X),
	quieroClase(Pad,X,Cla),
	nth0(0,Cla,IdP),
	segundoTermino(IdP,IdPa),
	IdPad = id_padre=>IdPa,
	nuevoId(I),
	atomic_concat(o,I,Id),
	Pr = [nombre=>N],
	Clase = [id=>Id,IdPad,Pr,[]],
	reverse(X,Y),
	Z = [Clase|Y],
	reverse(Z,S),
	guardarBD(S).
% Recibe el Nombre, el Padre, y las listas de Props y Rels
anadeObjeto(Nom,Pad,Props,Rels) :-
	rb(X),
	quieroClase(Pad,X,Cla),
	nth0(0,Cla,IdP),
	segundoTermino(IdP,IdPa),
	IdPad = id_padre=>IdPa,
	nuevoId(I),
	atomic_concat(o,I,Id),
	Pr = [nombre=>Nom|Props], [H|T] = Rels,
	Clase = [id=>Id,IdPad,Pr,Rels],
	reverse(X,Y),
	Z = [Clase|Y],
	reverse(Z,S),
	guardarBD(S).

% Recibe el Nombre de la Clase u Objeto, y una lista de Propiedades
% auqnue sólo sea una propiedad, tiene que ser en una lista
anadePropiedad(Nom,Props) :-
	rb(X), [H|T] = Props,
	quieroClase(Nom, X, Cla),
	nth0(2,Cla,Pr),
	append(Pr,Props,Pro),
	nth0(2,Cla,E,R),
	nth0(2,L,Pro,R),
	select(Cla,X,L,Y),!,
	guardarBD(Y).
% Lo mismo pero para Relaciones
anadeRelacion(Nom,Rels) :-
	rb(X), [H|T] = Rels,
	quieroClase(Nom, X, Cla),
	nth0(3,Cla,Re),
	append(Re,Rels,Rel),
	nth0(3,Cla,E,R),
	nth0(3,L,Rel,R),
	select(Cla,X,L,Y),!,
	guardarBD(Y).

% AYUDAAAAAAA %
sustPadre(Pa,[],NPa).
sustPadre(Pa,[H|T],NPa) :-
	H = [_,Pa,_,_],
	nth0(1,H,E,R),
	nth0(1,L,NPa,R),
	sustPadre(Pa,T,NPa),
	guardarBD([L|T]).
sustPadre(Pa,[H|T],NPa) :- \+
	H = [_,Pa,_,_],
	sustPadre(Pa,T,NPa).

nuevoId(Id) :-
	rb(X),
	reverse(X,Y),
	[H|T] = Y,
	[A|B] = H,
	segundoTermino(A,Idmax),
	atom_length(Idmax, Len),
	L is Len - 1,
	sub_atom(Idmax, 1, L, A, S),
	atom_number(S,S1), 
	Id is S1 + 1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
quieroClase(X,[],P).
quieroClase(X,[H|T],P):-
	nth0(2,H,Props),
	cabeza(Props,S),
	segundotermino(S,X),
	P=H;
	quieroClase(X,T,P).

crearNuevaListaProps(X,Y):-
	nth0(0,X,N),
	nth0(1,X,P),
	nth0(2,X,Props),
	nth0(3,X,Rels),
	nth0(4,X,Inds),
	PB= Y=>XX,
	eliminaClase(PB,Props,P1),
	H1=[N,P,P1,Rels,Inds],
	rb(W),
	sus(X,H1,W,S),
	guardarBD(S).

crearNuevaListaIndProps(X,Y,W,[]).
crearNuevaListaIndProps(X,Y,W,[H|T]):-
	nth0(4,H,Inds),
	crearNuevaListaIndDeepProps(X,Y,H,Inds,Inds),
	crearNuevaListaIndProps(X,Y,W,T).

crearNuevaListaIndDeepProps(X,Y,Lw,Inds,[]).
crearNuevaListaIndDeepProps(X,Y,Lw,Inds,[H|T]):-
	(
		RR= X=>XX,
		valor(individuo,H,Y),
		nth0(0,H,Nm),
		nth0(1,H,Props),
		nth0(2,H,Rels),
		member(RR,Props),
		eliminaClase(RR,Props,LP),
		H1=[Nm,LP,Rels],
		rb(W),
		sus(H,H1,Inds,S),
		sus(Inds,S,Lw,S1),
		eliminaClase(Inds,Lw,LL),
		concatenar(LL,S,L2),
		sus(Lw,L2,W,STs),
		guardarBD(STs)
	)
	;
	crearNuevaListaIndDeepProps(X,Y,Lw,Inds,T).

quieroPadre(X,[X|T],X).
quieroPadre(X,[H|T],Y):-
	quieroPadre(X,T,Y).

crearNuevaListaIndRel(X,W,TI,[]).
crearNuevaListaIndRel(X,W,TI,[H|T]):-
	(
		nth0(2,H,Rels),
		RR= XXX=>X,
		nth0(1,H,P2),
		nth0(0,H,Nm),
		member(RR,Rels),
		eliminaClase(RR,Rels,LR),
		H1=[Nm,P2,LR],
		rb(W1),
		sus(H,H1,TI,S),
		member(TI,W),
		sus(T1,S,W,S1),
		eliminaClase(TI,W,LL),
		concatenar(LL,S,L2),
		sus(W,L2,W1,STs),
		guardarBD(STs),
		crearNuevaListaIndRel(X,W,TI,T)
	)
	;
	crearNuevaListaIndRel(X,W,TI,T).

crearNuevaListaInicioInd(X):- rb(W), crearNuevaListaInd(X,W).
crearNuevaListaInd(X,[]).
crearNuevaListaInd(X,[H|T]):-
	valor(padre,H,P),
		(
		nth0(4,H,Is),
		IM = [individuo=>X,_,_],
		member(IM,Is),
		eliminaClase(IM,Is,ISs),
		nth0(2,H,P2),
		nth0(3,H,R2),
		valor(nombre,H,Nm),
		H1=[nombre=>Nm,padre=>P,P2,R2,ISs],
		rb(W),
		sus(H,H1,W,S),
		guardarBD(S),
		crearNuevaListaInd(X,T)
	);
	(
		nth0(3,H,Rel),
		RR= XXX=>X,
		(
			member(RR,Rel),
			eliminaClase(RR,Rel,Rs),
			nth0(2,H,P2),
			nth0(4,H,ISs),
			valor(nombre,H,Nm),
			H1=[nombre=>Nm,padre=>P,P2,Rs,ISs],
			rb(W),
			sus(H,H1,W,S),
			guardarBD(S),
			crearNuevaListaInd(X,T)
		);
		(
			nth0(4,H,Inds),
			crearNuevaListaIndRel(X,H,Inds,Inds),
			crearNuevaListaInd(X,T)
		)
	)
	; crearNuevaListaInd(X,T).

crearNuevaListaInicio(X,M,R,Pop):- rb(W), crearNuevaLista(X,M,R,Pop,W).

crearNuevaLista(X,M,R,Pop,[]).
crearNuevaLista(X,M,R,Pop,[H|T]):-
	(
		valor(padre,H,X),
		nth0(2,H,P2),
		nth0(3,H,R2),
		concatenar(M,P2,L1),
		concatenar(R,R2,L2),
		valor(nombre,H,Nm),
		H1=[nombre=>Nm,padre=>Pop,L1,L2,[]],
		rb(W),
		sus(H,H1,W,S),
		guardarBD(S),
		crearNuevaLista(X,M,R,Pop,T)
	);(
		valor(padre,H,P),
		nth0(3,H,Rel),
		RR= XXX=>X,
		member(RR,Rel),
		eliminaClase(RR,Rel,Rs),
		nth0(2,H,P2),
		nth0(4,H,ISs),
		valor(nombre,H,Nm),
		H1=[nombre=>Nm,padre=>P,P2,Rs,ISs],
		rb(W),
		sus(H,H1,W,S),
		guardarBD(S),
		crearNuevaLista(X,M,R,Pop,T)
	);crearNuevaLista(X,M,R,Pop,T).