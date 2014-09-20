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


sacaNombreDeClase([H|T],Y):- segundotermino(H,Y), !.
sacaPadreDeClase([H|T],Y):- cabeza(T,Z), segundotermino(Z,Y), !.
sacaPropiedadesDeClase([H|T],Y):- cola(T,Z), cabeza(Z,Y), !.
sacaRelacionesDeClase([H|T],Y):- cola(T,L), cola(L,P), cabeza(P,Y), !.
sacaListaIndividuosDeClase([H|T],Y):- cola(T,L), cola(L,P), cola(P,M), cabeza(M,Y), !.

buscaIndividuoEnListaDeIndividuos(X,[],Y):- Y = false.		
buscaIndividuoEnListaDeIndividuos(X,[H|T],Y):- 
											cabeza(H,L),
											segundotermino(L,K),
											X == K,
											Y = true,!
											;
											buscaIndividuoEnListaDeIndividuos(X,T,Y),!.

encuentraClasesALasQuePerteneceUnIndividuo(X,Y):- encuentraNombreClaseDeIndividuo(X,Z), sacaClasesPadreDeUnaClaseInicio(Z,W), anade(W,Z,Y),!.
											
encuentraNombreClaseDeIndividuo(X,Y):-	encuentraClaseDeIndividuoInicio(X,Z), sacaNombreDeClase(Z,Y).

encuentraClaseDeIndividuoInicio(X,Y):- rb(W), encuentraClaseDeIndividuo(X,W,Y).	
encuentraClaseDeIndividuo(X,[H|T],Y):-  
										sacaListaIndividuosDeClase(H,N),
										buscaIndividuoEnListaDeIndividuos(X,N,M),
										M == true,
										Y = H,
										!;
										encuentraClaseDeIndividuo(X,T,Y)
										.
								
sacaObjetosHijosDeClaseRecursivoInicioFlat(X,Y):- sacaObjetosHijosDeClaseRecursivoInicio(X,Z), flatten(Z,Y).

sacaObjetosHijosDeClaseRecursivoInicio(X,Y):-
								sacaClasesHijasRecursivoInicioFlat(X,S), 
								sacaObjetosHijosDeClaseRecursivo(X,S,Y),!.
sacaObjetosHijosDeClaseRecursivo(X,[],Y):- Y=[].						
sacaObjetosHijosDeClaseRecursivo(X,[H|T],Y):- 	
								sacaObjetosHijosDeClaseInicio(H,F),
								sacaObjetosHijosDeClaseRecursivo(X,T,G),
								anade(F,G,Y),!.

sacaObjetosHijosDeClaseRecursivo(X,[],Y):- Y=[].						
sacaObjetosHijosDeClaseRecursivo(X,[H|T],Y):- 	
								sacaObjetosHijosDeClaseInicio(H,F),
								sacaObjetosHijosDeClaseRecursivo(X,T,G),
								anade(F,G,Y),!.

iteraPropiedades([],Y).
iteraPropiedades([H|T],Y):- 
								cabeza(H,S),
								segundotermino(S,M),
								iteraIndividuos(T,R),
								anade(M,R,Y),!.
								
sacaPropiedadesDirectasDeClaseInicio(X,Y):- rc(X), sacaPropiedadesDirectasDeClase(X,Y).
sacaPropiedadesDirectasDeClase([],Y).
sacaPropiedadesDirectasDeClase([H|T],Y):-
								cola(T,R),
								cabeza(R,Y),!.

								
sacaPropiedadesDirectasDeIndividuoInicio(X,Y):- ri(X), sacaPropiedadesDirectasDeIndividuo(X,Y).
sacaPropiedadesDirectasDeIndividuo([],Y).
sacaPropiedadesDirectasDeIndividuo([H|T],Y):-
								write('T: '),
								write(T),
								nl,
								cabeza(T,Y),								
								write('Y: '),
								write(Y),
								nl,!.


sacaIndividuosYClasesHijasDeUnaClaseInicioFlat(X,Y):- sacaIndividuosYClasesHijasDeUnaClaseInicio(X,Z), flatten(Z,Y).
								
sacaIndividuosYClasesHijasDeUnaClaseInicio(X,Y):- sacaClasesHijasRecursivoInicioFlat(X,Z), sacaObjetosHijosDeClaseRecursivoInicioFlat(X,W), anade(Z,W,Y), !.



sacaClasesPadreDeUnaClase(X,[],Y).
sacaClasesPadreDeUnaClaseInicio(X,Y):- rb(W), sacaClasesPadreDeUnaClase(X,W,Y).
sacaClasesPadreDeUnaClase(X,[H|T],Y):- cabeza(H,Z),
										cabeza(Z,R),
										segundotermino(R,M),
										X == M,
										cola(H,U),
										cabeza(U,L),
										segundotermino(L,K),
										sacaClasesPadreDeUnaClaseInicio(K,W),
										K \= 0,
										anade(K,W,Y),
										!;
										sacaClasesPadreDeUnaClase(X,T,Y),
										!.

sacaClasePorNombre(X,[],Y).
sacaClasePorNombreInicio(X,Y):-rb(W), sacaClasePorNombre(X,W,Y).
sacaClasePorNombre(X,[H|T],Y):- sacaNombreDeClase(H,Z), X == Z , Y = H,!; sacaClasePorNombre(X,T,Y).

sacaIndividuoDeListaDeIndividuos(X,[],Y):- Y = [].
sacaIndividuoDeListaDeIndividuos(X,[H|T],Y):-
								cabeza(H,W),
								segundotermino(W,L),
								X==L,
								Y=H,!;
sacaIndividuoDeListaDeIndividuos(X,T,Y),!.

sacaPropiedadesDeIndividuo([H|T],Y):- cabeza(T,Y),!.


decideSiEsClaseOIndividuo(X,Y):- rb(W),
encuentraClaseDeIndividuoInicio(X,S), Y = individuo,!;
sacaClasePorNombreInicio(X,R), Y = clase,!.

sacaPropiedadesDeListaDeClasesInicio(X,Y):- rb(W),sacaPropiedadesDeListaDeClases(X,W,Y).
sacaPropiedadesDeListaDeClases([],W,Y).
sacaPropiedadesDeListaDeClases([H|T],W,Y):-
sacaClasePorNombreInicio(H,U),
sacaPropiedadesDeClase(U,N),
sacaPropiedadesDeListaDeClases(T,W,G),
anade(N,G,K), flatten(K,Y),!.

sacalaspropiedadesIncluidasLasHeredadasDeUnaClase(X,Y):-
				sacaClasePorNombreInicio(X,W),
				sacaPropiedadesDeClase(W,R),
				sacaClasesPadreDeUnaClaseInicio(X,S),
				sacaPropiedadesDeListaDeClasesInicio(S,Y).


sacalaspropiedadesIncluidasLasHeredadasDeUnIndividuo(X,Y):-
						encuentraClaseDeIndividuoInicio(X,Z),
						sacaListaIndividuosDeClase(Z,W),
						sacaIndividuoDeListaDeIndividuos(X,W,S),
						sacaPropiedadesDeIndividuo(S,M),
						sacaLasPropiedadesDeClasesPadreParaIndividuoInicioFlat(X,K),
						anade(K,M,L),
						flatten(L,Y),!.

borraDeListaDeAtributos(X,[],Y).
borraDeListaDeAtributos(X,[H,T],Y):-
	write('H: '), write(H), nl,
	primertermino(H,S),
	write('S: '), write(S), nl,
	X == S,
	R = T;
	borraDeListaDeAtributos(X,T,Y).

sacalaspropiedadesMonotonicasInicio(X,Y):-
sacalaspropiedadesIncluidasLasHeredadasDeLoQueSea(X,W),
sacalaspropiedadesMonotonicas(X,W,Y).

sacalaspropiedadesMonotonicas(X,[],Y).
sacalaspropiedadesMonotonicas(X,[H|T],Y).


sacalaspropiedadesIncluidasLasHeredadasDeLoQueSea(X,Y):-
decideSiEsClaseOIndividuo(X,D),
D == clase,
sacalaspropiedadesIncluidasLasHeredadasDeUnaClase(X,Y),!
;
sacalaspropiedadesIncluidasLasHeredadasDeUnIndividuo(X,Y),!
.

sacaLasPropiedadesDeClasesPadreParaIndividuoInicioFlat(X,Y):-sacaLasPropiedadesDeClasesPadreParaIndividuoInicio(X,Z), flatten(Z,Y).
sacaLasPropiedadesDeClasesPadreParaIndividuoInicio(X,Y):-encuentraClasesALasQuePerteneceUnIndividuo(X,W),
sacaLasPropiedadesDeClasesPadreParaIndividuo(X,W,Y).
sacaLasPropiedadesDeClasesPadreParaIndividuo(X,[],Y).
sacaLasPropiedadesDeClasesPadreParaIndividuo(X,[H|T],Y):- sacaClasePorNombreInicio(H,Z), 
								sacaPropiedadesDeClase(Z,S), 
								sacaLasPropiedadesDeClasesPadreParaIndividuo(X,T,R), 
								anade(S,R,Y), 
								!. 
rli(Y):- Y = [
		[individuo=>'estrella de mar',[ojos=>0, movimiento=>arrastra],[odia=>leon, ama=>pinguino]], 
		[individuo=>gusano, [ojos=>0, movimiento=>arrastra],[]]
	].										

ri(Y):- Y =	[individuo=>'estrella de mar',[ojos=>0, movimiento=>arrastra],[odia=>leon, ama=>pinguino]].						
								
rc(Y):- Y = [id=>1, id_Padre=>0, 
	[vida=>finita, ojos=>2],
	[odia=>pinguino],
	[
		[individuo=>'estrella de mar',[ojos=>0, movimiento=>arrastra],[odia=>leon, ama=>pinguino]], 
		[individuo=>gusano, [ojos=>0, movimiento=>arrastra],[]]
	]
	].
								
rb(Y):- Y = [
	[id=>1, id_Padre=>0, 
	[nombre=>animal ,vida=>finita, ojos=>2],
	[odia=>pinguino],
	[
		[id=>101, [nombre=>'estrella de mar',ojos=>0, movimiento=>arrastra],[odia=>leon, ama=>pinguino]], 
		[id=>102, [nombre=>gusano, ojos=>0, movimiento=>arrastra],[]]
	]
	],
	[id=>2, id_Padre=>1,
	[nombre=>oviparo,nace=>huevo],
	[odia=>viviparo],
	[
		[id=>103, [nombre=>hormiga, carga=>mucho, ojos=>100], [come=>gusano]]
	]
	],
	[id=>3, id_Padre=>1
	[nace=>placenta],
	[odia=>oviparo],
	[
		[id=>104, [nombre=>mosca, movimiento=>vuela], [come=>gusano]],
		[id=>105, [nombre=>delfin, movimiento=>nada], []]
	]
	],
	[id=>4, id_padre=>2,
	[nombre=>ave, movimiento=>vuela],
	[],
	[
		[id=>105, [nombre=>phoenix, vida=>infinita], [come=>leon]]
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