:- initialization(main).
:- op(15, xfx, '=>').
=>(X,Y).

main:-
	consult('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/main.pl').
rb1(Y):-
	open_kb('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/bd.txt',Y).
guardarBD(Y):-
	save_kb('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/bd.txt',Y).


my_remove_one_element(X, [X|Xs], Xs).

my_remove_one_element(X, [Y|Ys], [Y|Zs]):-
          my_remove_one_element(X, Ys, Zs).

primertermino(X,Y):- X = W=>Z, Y=W.
segundotermino(X,Y):- X = W=>Z, Y=Z.
valor(X,[],Y):- fail.
valor(X,[H|T],Y):- primertermino(H,X), segundotermino(H,Y), !.
valor(X,[H|T],Y):- primertermino(H,Z), X\=Z, valor(X,T,Y).


props(X,Y):- 
	slice(3,3,X,Y).
rels(X,Y):-
	slice(4,4,X,Y).


valor(X,[H|T],Y):- primertermino(H,X), segundotermino(H,Y), !.
valor(X,[H|T],Y):- primertermino(H,Z), X\=Z, valor(X,T,Y).

slice([X|_],1,1,[X]).
slice([X|T],1,K,[X|T1]) :- K > 1, 
   K1 is K - 1, slice(T,1,K1,T1).
slice([_|T],I,K,Y) :- I > 1, 
   I1 is I - 1, K1 is K - 1, slice(T,I1,K1,Y).


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
	rb1(W),
	MM=[nombre=>X,_,_,_,_],
	quieroPadre(MM,W,P),
	nth0(2,P,Pr),
	nth0(3,P,Rel),
	valor(padre,P,Pop),
	(	
		Pop\=top,
		crearNuevaListaInicio(X,Pr,Rel,Pop),
		rb1(W1),
		eliminaClase(MM,W1,S),
		guardarBD(S)
	);
	(
		crearNuevaListaInicioInd(X)
	)
	.

quieroPadre(X,[X|T],X).
quieroPadre(X,[H|T],Y):-
	quieroPadre(X,T,Y).

crearNuevaListaInicioInd(X):- rb1(W), crearNuevaListaInd(X,W).


crearNuevaListaIndRel(X,W,[]).
crearNuevaListaIndRel(X,W,TI,[H|T]):-
	(
		nth0(2,H,Rels),
		RR= XXX=>X,
		nth0(1,H,P2),
		nth0(0,H,Nm),
		member(RR,Rels),
		eliminaClase(RR,Rels,LR),
		H1=[Nm,P2,LR],
		rb1(W1),
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


crearNuevaListaInd(X,[]).
crearNuevaListaInd(X,[H|T]):-
	(
		valor(padre,H,P),
		P\=top,
		nth0(4,H,Is),
		IM = [individuo=>X,_,_],
		member(IM,Is),
		eliminaClase(IM,Is,ISs),
		nth0(2,H,P2),
		nth0(3,H,R2),
		valor(nombre,H,Nm),
		H1=[nombre=>Nm,padre=>P,P2,R2,ISs],
		rb1(W),
		sus(H,H1,W,S),
		guardarBD(S),
		crearNuevaListaInd(X,T)
	);
	(
	valor(padre,H,P),
	nth0(3,H,Rel),
	RR= XXX=>X,
		(
			member(RR,Rel),
			P\=top,
			eliminaClase(RR,Rel,Rs),
			nth0(2,H,P2),
			nth0(4,H,ISs),
			valor(nombre,H,Nm),
			H1=[nombre=>Nm,padre=>P,P2,Rs,ISs],
			rb1(W),
			sus(H,H1,W,S),
			guardarBD(S),
			crearNuevaListaInd(X,T)
		);
		(
			P\=top,
			nth0(4,H,Inds),
			crearNuevaListaIndRel(X,H,Inds,Inds),
			crearNuevaListaInd(X,T)
		)
	)
	;
	P\=top,crearNuevaListaInd(X,T).

crearNuevaListaInicio(X,M,R,Pop):- rb1(W), crearNuevaLista(X,M,R,Pop,W).

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
	rb1(W),
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
	rb1(W),
	sus(H,H1,W,S),
	guardarBD(S),
	crearNuevaLista(X,M,R,Pop,T)
	);crearNuevaLista(X,M,R,Pop,T).
								
rb(Y):- Y = [
	[nombre=>animal, padre=>top, 
	[vida=>finita, ojos=>2],
	[odia=>pinguino],
	[
		[individuo=>'estrella de mar',[ojos=>0, movimiento=>arrastra],[odia=>leon, ama=>pinguino]], 
		[individuo=>gusano, [ojos=>0, movimiento=>arrastra],[]]
	]
	],
	[nombre=>oviparo,padre=>animal,
	[nace=>huevo],
	[odia=>viviparo],
	[
		[individuo=>hormiga, [carga=>mucho, ojos=>100], [come=>gusano]]
	]
	],
	[nombre=>viviparo,padre=>animal,
	[nace=>placenta],
	[odia=>oviparo],
	[
		[individuo=>mosca, [movimiento=>vuela], [come=>gusano]],
		[individuo=>delfin, [movimiento=>nada], []]
	]
	],
	[nombre=>ave,padre=>oviparo,
	[movimiento=>vuela],
	[],
	[
		[individuo=>phoenix, [vida=>infinita], [come=>leon]]
	]
	],
	[nombre=>pez,padre=>oviparo,
	[movimiento=>nada],
	[odia=>leon],
	[]
	],
	[nombre=>mamifero,padre=>viviparo,
	[movimiento=>vuela],
	[],
	[
		[individuo=>leon, [armas=>garra], [come=>pinguino]]
	]
	],
	[nombre=>pato,padre=>ave,
	[movimiento=>nada],
	[come=>pez],
	[
		[individuo=>hugo, [color=>rojo], [hermano=>paco]],
		[individuo=>paco, [color=>azul], [hermano=>luis]],
		[individuo=>luis, [color=>verde], []]
	]
	],
	[nombre=>aguila,padre=>ave,
	[movimiento=>vuela, arma=>garras],
	[],
	[
		[individuo=>'aguila calva', [pelo=>no], []]
	]
	],
	[nombre=>pinguino,padre=>ave,
	[movimiento=>nada],
	[come=>pez],
	[]
	],
	[nombre=>huachinango,padre=>pez,
	[movimiento=>nada, sabor=>delicioso],
	[],
	[]
	],
	[nombre=>'pez volador',padre=>pez,
	[movimiento=>vuela],
	[come=>gusano],
	[
		[individuo=>flippy, [fama=>mucha], []]
	]
	]
].