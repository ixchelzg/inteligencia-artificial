:- op(15, xfx, '=>').
=>(X,Y).
primertermino(X,Y):- X = W=>Z, Y=W.
segundotermino(X,Y):- X = W=>Z, Y=Z.
valor(X,[],Y):- fail.
valor(X,[H|T],Y):- primertermino(H,Z), X=Z, segundotermino(H,W), Y=W, !.
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