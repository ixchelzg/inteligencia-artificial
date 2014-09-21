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
	[id=>c1, id_padre=>c0, [nombre=>animal ,vida=>finita, ojos=>2],[odia=>c20]],
	[id=>o2, id_padre=>c1, [nombre=>'estrella de mar',ojos=>0, movimiento=>arrastra],[odia=>o13, ama=>c20]], 
	[id=>o3, id_padre=>c1, [nombre=>gusano, ojos=>0, movimiento=>arrastra],[]],
	[id=>c4, id_padre=>c1, [nombre=>oviparo,nace=>huevo],[odia=>c6],[id_hijo=>o5]],
	[id=>o5, id_padre=>c4, [nombre=>hormiga, carga=>mucho, ojos=>100], [come=>o3]],
	[id=>c6, id_padre=>c1, [nombre=>viviparo, nace=>placenta],[odia=>c4]],
	[id=>o7, id_padre=>c6, [nombre=>mosca, movimiento=>vuela], [come=>o3]],
	[id=>o8, id_padre=>c6, [nombre=>delfin, movimiento=>nada], []],
	[id=>c9, id_padre=>c4, [nombre=>ave, movimiento=>vuela], []],
	[id=>o10, id_padre=>c9, [nombre=>phoenix, vida=>infinita], [come=>o13]],
	[id=>c11, id_padre=>c4, [nombre=>pez, movimiento=>nada], [odia=>leon]],
	[id=>c12, id_padre=>c6, [nombre=>mamifero, movimiento=>corre],[]],
	[id=>o13, id_padre=>c12, [nombre=>leon, armas=>garra], [come=>c20]],
	[id=>c14, id_padre=>c9, [nombre=>pato, movimiento=>nada],[come=>pez]],
	[id=>o15, id_padre=>c14, [nombre=>hugo, color=>rojo], [hermano=>o16]],
	[id=>o16, id_padre=>c14, [nombre=>paco, color=>azul], [hermano=>o17]],
	[id=>o17, id_padre=>c14, [nombre=>luis, color=>verde], []],
	[id=>c18, id_padre=>c9, [nombre=>aguila, movimiento=>vuela, arma=>garras],[]],
	[id=>o19, id_padre=>c18, [nombre=>'aguila calva', pelo=>no], []],
	[id=>c20, id_padre=>c9, [nombre=>pinguino, movimiento=>nada], [come=>pez]],
	[id=>c21, id_padre=>c11, [nombre=>huachinango, movimiento=>nada, sabor=>delicioso],[]],
	[id=>c22, id_padre=>c11, [nombre=>'pez volador', movimiento=>vuela], [come=>o3]],
	[id=>o23, id_padre=>c22, [nombre=>flippy, fama=>mucha], []]
].



comeLista([],X,Y):- Y = [].
comeLista([H|T],X,Y):-
								primerTermino(H,L), 
								X==L,
								write('1Este Termino: "'), write(H), write('" si lo quiero borrar') , nl,
								write('1Me voy con: "'), write(T), write('" a la siguente iteracion') , nl,
								comeLista(T,X,R), 
								Y = R,!; 
								write('2Me voy con: "'), write(T), write('" a la siguente iteracion') , nl,
								comeLista(T,X,R), 
								append([H],R,Y),!.


								borraPropiedadesRepetidas([nombre=>animal ,vida=>finita, ojos=>2, nombre=>'estrella de mar',ojos=>0, movimiento=>arrastra,nombre=>gusano, ojos=>0, movimiento=>arrastra,nombre=>oviparo,nace=>huevo],Y).



								borraPropiedadesRepetidas([],Y):- Y = [].
borraPropiedadesRepetidas([H|T],Y):- primerTermino(H,L),
								write('L:'), write(L), nl,
								borraPropiedadDeListaDePropiedades(T,L,R),
								write('R:'), write(R), nl,
								borraPropiedadesRepetidas(R,P),
								write('P:'), write(P), nl,
								append([H],P,Y), !.




								rb(Y):- Y = [
	[id=>c1, id_padre=>c0, [nombre=>animal ,vida=>finita, ojos=>2],[odia=>o2]],
	[id=>o2, id_padre=>c1, [nombre=>'estrella de mar',ojos=>0, movimiento=>arrastra],[odia=>o13, ama=>c20]], 
	[id=>o3, id_padre=>c1, [nombre=>gusano, ojos=>0, movimiento=>arrastra],[]],
	[id=>c4, id_padre=>c1, [nombre=>oviparo,nace=>huevo],[odia=>c6]],
	[id=>o5, id_padre=>c4, [nombre=>hormiga, carga=>mucho, ojos=>100], [come=>o3]],
	[id=>c6, id_padre=>c1, [nombre=>viviparo, nace=>placenta],[odia=>c4]],
	[id=>o7, id_padre=>c6, [nombre=>mosca, movimiento=>vuela], [come=>o3]],
	[id=>o8, id_padre=>c6, [nombre=>delfin, movimiento=>nada], []],
	[id=>c9, id_padre=>c4, [nombre=>ave, movimiento=>vuela], []],
	[id=>o10, id_padre=>c9, [nombre=>phoenix, vida=>infinita], [come=>o13]],
	[id=>c11, id_padre=>c4, [nombre=>pez, movimiento=>nada], [odia=>o13]],
	[id=>c12, id_padre=>c6, [nombre=>mamifero, movimiento=>corre],[]],
	[id=>o13, id_padre=>c12, [nombre=>leon, arma=>garras], [come=>c20]],
	[id=>c14, id_padre=>c9, [nombre=>pato, movimiento=>nada],[come=>c11]],
	[id=>o15, id_padre=>c14, [nombre=>hugo, color=>rojo], [hermano=>o16]],
	[id=>o16, id_padre=>c14, [nombre=>paco, color=>azul], [hermano=>o17]],
	[id=>o17, id_padre=>c14, [nombre=>luis, color=>verde], []],
	[id=>c18, id_padre=>c9, [nombre=>aguila, movimiento=>vuela, arma=>garras],[]],
	[id=>o19, id_padre=>c18, [nombre=>'aguila calva', pelo=>no], []],
	[id=>c20, id_padre=>c9, [nombre=>pinguino, movimiento=>nada], [come=>c11]],
	[id=>c21, id_padre=>c11, [nombre=>huachinango, movimiento=>nada, sabor=>delicioso],[]],
	[id=>c22, id_padre=>c11, [nombre=>'pez volador', movimiento=>vuela], [come=>o3]],
	[id=>o23, id_padre=>c22, [nombre=>flippy, fama=>mucha], []]
].