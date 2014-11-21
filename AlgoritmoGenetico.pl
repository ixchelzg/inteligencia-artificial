%%  Algoritmo Genético
%%  Ixchel, Vladimir, Eduardo
%%  Algoritmo Genetico para generar soluciones al problema del robot de servicio.

%% Esto Establece la generación aleatoria inicial y llama la parte cíclica del algoritmo.
algoritmoGenetico(NumeroDeIndividuos,Tamano,NumeroDeAcciones,ProbabilidadDeCruzamiento,ProbabilidadDeMutacion,Generaciones,MejorIndividuo):-
													generaPoblacionAleatoria(NumeroDeIndividuos,Tamano,NumeroDeAcciones,Poblacion),
													cicloBasico(NumeroDeIndividuos,Tamano,NumeroDeAcciones,ProbabilidadDeCruzamiento,ProbabilidadDeMutacion,Generaciones,Poblacion,MejorIndividuo),
													!.

%% Esto es el ciclo básico del algoritmo genético.
cicloBasico(NumeroDeIndividuos,Tamano,NumeroDeAcciones,ProbabilidadDeCruzamiento,ProbabilidadDeMutacion,0,Poblacion,MejorIndividuo):-
													MejorIndividuo = [0,0],
													!.
cicloBasico(NumeroDeIndividuos,Tamano,NumeroDeAcciones,ProbabilidadDeCruzamiento,ProbabilidadDeMutacion,Generaciones,Poblacion,MejorIndividuo):-
													Generaciones > 0,
													evaluaPoblacion(Poblacion,PoblacionEvaluada),
													ordenaPoblacion(PoblacionEvaluada,PoblacionOrdenada),
													PoblacionOrdenada = [MejorIndividuoActual|ElResto],
													cruzaPoblacion(PoblacionOrdenada,ProbabilidadDeCruzamiento,PoblacionCruzada),
													mutaPoblacion(PoblacionCruzada,ProbabilidadDeMutacion,NumeroDeAcciones,PoblacionMutada),
													%% Hacemos estoporque estamos usando elitismo total.
													append(PoblacionOrdenada,PoblacionMutada,PoblacionDoble),
													evaluaPoblacion(PoblacionDoble,PoblacionDobleEvaluada),
													ordenaPoblacion(PoblacionDobleEvaluada,PoblacionDobleOrdenada),
													%% Aqui regresamos a la población del tamaño original con los mejores individuos.
													split_at(NumeroDeIndividuos,PoblacionDobleOrdenada,PoblacionSuperior,PoblacionInferior),
													Generacioncitas is Generaciones - 1,
													cicloBasico(NumeroDeIndividuos,Tamano,NumeroDeAcciones,ProbabilidadDeCruzamiento,ProbabilidadDeMutacion,Generacioncitas,PoblacionSuperior,MejorIndividuoPreliminar),
													mejor(MejorIndividuoActual,MejorIndividuoPreliminar,MejorIndividuo),
													write('Generacion: '),
													write(Generaciones),
													write(', Mejor Individuo: '),
													write(MejorIndividuoActual),
													nl,
													!.

%% Esto regresa el individuo que tiene el fitness más grande.
mejor(PrimerIndividuo,SegundoIndividuo,MaximoIndividuo):-
													PrimerIndividuo = [PrimerFitness|PrimerosGenes],
													SegundoIndividuo = [SegundoFitness|SegundosGenes],
													PrimerFitnessEvaluado is PrimerFitness + 0,
													SegundoFitnessEvaluado is SegundoFitness + 0,
													PrimerFitnessEvaluado >= SegundoFitnessEvaluado,
													MaximoIndividuo = PrimerIndividuo,
													!
													;
													MaximoIndividuo = SegundoIndividuo,
													!.					

%% Esto va a generar una poblacion aleatoria.
generaPoblacionAleatoria(0,Tamano,NumeroDeAcciones,Poblacion):- 
													Poblacion = [],
													!.
generaPoblacionAleatoria(NumeroDeIndividuos,Tamano,NumeroDeAcciones,Poblacion):-
													NumeroDeIndividuos > 0,
													NumeritoDeIndividuos is NumeroDeIndividuos - 1,
													generaPoblacionAleatoria(NumeritoDeIndividuos,Tamano,NumeroDeAcciones,PoblacionAnterior),
													generaUNindividuoAleatorio(Tamano,NumeroDeAcciones,Individuo),
													append([0],Individuo,IndividuoConFitness),
													append(PoblacionAnterior,[IndividuoConFitness],Poblacion),
													!.

%% Esto van a generar un individuo aleatorio.
generaUNindividuoAleatorio(0,NumeroDeAcciones,Individuo):- 
													Individuo = [],
													!.
generaUNindividuoAleatorio(Tamano,NumeroDeAcciones,Individuo):-
 													Tamano > 0,	
													Tamanito is Tamano - 1,
													generaUNindividuoAleatorio(Tamanito,NumeroDeAcciones,IndividuoAnterior),
													random_between(1,NumeroDeAcciones,Aleatorio),
													append(IndividuoAnterior,[Aleatorio],Individuo),
													!.

%% Esto saca el fitness de la población entera.
evaluaPoblacion([],PoblacionEvaluada):-
													PoblacionEvaluada = [],
													!.
evaluaPoblacion([PrimerIndividuo|RestoDeIndividuos],PoblacionEvaluada):-
													evaluaPoblacion(RestoDeIndividuos,PoblacionEvaluadaIncompleta),
													PrimerIndividuo = [Fitness|Genes],
													evaluaIndividuo(Genes,Fitness2),
													append(PoblacionEvaluadaIncompleta,[[Fitness2|Genes]],PoblacionEvaluada),
													!.

%% Esto le saca el fitness a un individuo solo.
evaluaIndividuo([],Fitness):- 
													Fitness is 0,
													!.
evaluaIndividuo([Cabeza|Cola],Fitness):-
													evaluaIndividuo(Cola,FitnessIncompleto),
													Fitness is Cabeza + FitnessIncompleto,
													!.

%% Esto ordena de mayor a menor una población según el fitness (primer entrada) de cada individuo.
ordenaPoblacion(PoblacionEvaluada,PoblacionOrdenada):-
													msort(PoblacionEvaluada,PoblacionOrdenadaInvertida),
													reverse(PoblacionOrdenadaInvertida,PoblacionOrdenada),
													!.

%% Esto Reproduce los genomas según el esquema de reproduccion deterministica (el mejor con el peor, el segundo mejor con el segundo mejor, etc..).
cruzaPoblacion([],ProbabilidadDeCruzamiento,PoblacionCruzada):- 
													PoblacionCruzada = [],
													!.
cruzaPoblacion(PoblacionOrdenada,ProbabilidadDeCruzamiento,PoblacionCruzada):-
													PoblacionOrdenada = [ElMejor|LosOtros],
													reverse(LosOtros,LosOtrosInvertidos),
													LosOtrosInvertidos = [ElPeor|LosRestantesInvertidos],
													reverse(LosRestantesInvertidos,LosRestantes),
													cruzaPoblacion(LosRestantes,ProbabilidadDeCruzamiento,PoblacionCruzadaIncompleta),
													cruzaIndividuos(ElMejor,ElPeor,Hijo1,Hijo2,ProbabilidadDeCruzamiento),
													append(PoblacionCruzadaIncompleta,[Hijo1,Hijo2],PoblacionCruzada),
													!.

%% Esto toma 2 individuos y regresa 2 individuos con los genes mezclados.
cruzaIndividuos(ElMejor,ElPeor,Hijo1,Hijo2,ProbabilidadDeCruzamiento):-
													random(Rand),
													Aleatorio is Rand + 0,
													ProbabilidadDeCruzamiento >= Aleatorio,
													length(ElMejor,Largo), 
													Fin is Largo - 2,
													random_between(2,Fin,PuntoDeCorte),
													split_at(PuntoDeCorte,ElMejor,PrincipioDeElMejor,FinalDeElMejor),
													split_at(PuntoDeCorte,ElPeor,PrincipioDeElPeor,FinalDeElPeor),
													append(PrincipioDeElMejor,FinalDeElPeor,Hijo1),
													append(PrincipioDeElPeor,FinalDeElMejor,Hijo2),
													!
													;
													Hijo1 = ElMejor,
													Hijo2 = ElPeor,
													!.

%% Esto cambia algún gen de todos los individuos.
mutaPoblacion([],ProbabilidadDeMutacion,NumeroDeAcciones,PoblacionMutada):-
													PoblacionMutada = [],
													!.
mutaPoblacion([ElPrimero|LosDemas],ProbabilidadDeMutacion,NumeroDeAcciones,PoblacionMutada):-
													mutaPoblacion(LosDemas,ProbabilidadDeMutacion,NumeroDeAcciones,PoblacionMutadaIncompleta),
													mutaIndividuos(ElPrimero,ProbabilidadDeMutacion,NumeroDeAcciones,ElPrimeroMutado),
													append([ElPrimeroMutado],PoblacionMutadaIncompleta,PoblacionMutada),
													!.

%% Esto Cambia algún gen de un individuo.
mutaIndividuos(ElPrimero,ProbabilidadDeMutacion,NumeroDeAcciones,ElPrimeroMutado):-
													random(Rand),
													Aleatorio is Rand + 0,
													ProbabilidadDeMutacion >= Aleatorio,
													length(ElPrimero,Largo), 
													Fin is Largo - 2,
													random_between(0,Fin,PuntoDeCorte),
													split_at(PuntoDeCorte,ElPrimero,PrincipioDeElPrimero,FinalDeElPrimero),
													random_between(1,NumeroDeAcciones,Aleatorio),
													FinalDeElPrimero = [ElQueVamosAReemplazar|LosQueVamosADejarEnPaz],
													append(PrincipioDeElPrimero,[Aleatorio|LosQueVamosADejarEnPaz],ElPrimeroMutado),
													!
													;
													ElPrimero = ElPrimeroMutado,
													!.

%% Función split_at de hprolog.pl (me gustaría poder cargarlo pero no encuentro como).
split_at(0,L,[],L) :- !.
split_at(N,[H|T],[H|L1],L2) :-
 	M is N -1,
 	split_at(M,T,L1,L2).

% Uso esto para poder picarle w en swi-prolog para que me de todas las respuestas.
ho(hom).
ho(bar).
h(Y):- ho(Y).

% Esta función regresa toda la actividad posible del robot.

% Esta función regresa una tabla con todas las acciones posibles.

% Esta función regresa todos los movientos posibles.


% 1a Regresa la extensión de una clase. 
% La extensión de una clase (el conjunto de todos los objetos que pertenecen a la misma, ya sea porque se declaren directamente o porque están en la cerradura de la relación de herencia).
% Ej.
% ?- extensionDeUnaClaseInicio(ave,Y).
% Y = [phoenix, hugo, paco, luis, pingu].

% Regresa todos los objetos hijos directos o indirectos de una clase dada.
extensionDeUnaClaseInicio(X,Y):- rb(W), 
								regresaTuplaPorNombre(X,W,S), 
								regresaId(S,L), 
								extensionDeUnaClase(L,W,Y),!.
extensionDeUnaClase(X,[],Y):- Y = [].
extensionDeUnaClase(X,[H|T],Y):- 
								regresaId_padre(H,S), 
								X==S,
								regresaNombre(H,N),
								regresaId(H,I), 
								string_chars(I,J), 
								J=[C|V], 
								C == 'c',
								extensionDeUnaClaseInicio(N,P),
								extensionDeUnaClase(X,T,R), 
								append(P,R,Y),!
								;
								regresaId_padre(H,S), 
								X==S,
								regresaNombre(H,N),
								regresaId(H,I), 
								string_chars(I,J), 
								J=[C|V], 
								C == 'o',
								extensionDeUnaClase(X,T,R), 
								append([N],R,Y),!
								; 
								extensionDeUnaClase(X,T,Y),!.



% Aqui tengo la base 
rb(Y):- Y = [
	[
		id=>c1,
		id_padre=>c0,
		[nombre=>robot],
		[ubicacion=>o5]
	],
	[
		id=>o2,
		id_padre=>c1,
		[
		nombre=>brazoIzquierdo,
		],
		[]
	],
	[
		id=>o3,
		id_padre=>c1,
		[
		nombre=>brazoDerecho,
		],
		[]
	],
	[
		id=>c4,
		id_padre=>c0,
		[nombre=>lugares],
		[]
	],
	[
		id=>o5,
		id_padre=>c4,
		[nombre=>inicio],
		[movimiento=>[
			[descripcion=>o5,probabilidad=>1.00,costo=>0.0,recompensa=>0.0],
			[descripcion=>o6,probabilidad=>0.98,costo=>6.0,recompensa=>0.0],
			[descripcion=>o7,probabilidad=>0.98,costo=>8.0,recompensa=>0.0],
			[descripcion=>o8,probabilidad=>0.98,costo=>9.0,recompensa=>0.0],
			[descripcion=>o9,probabilidad=>0.98,costo=>10.0,recompensa=>0.0],
			[descripcion=>o10,probabilidad=>0.98,costo=>9.0,recompensa=>0.0]             
			]
		]
	],
	[
		id=>o6,
		id_padre=>c4,
		[nombre=>estante1],
		[movimiento=>[
			[descripcion=>o5,probabilidad=>0.98,costo=>6.0,recompensa=>0.0],
			[descripcion=>o6,probabilidad=>1.00,costo=>0.0,recompensa=>0.0],
			[descripcion=>o7,probabilidad=>0.97,costo=>12.0,recompensa=>0.0],
			[descripcion=>o8,probabilidad=>0.97,costo=>15.0,recompensa=>0.0],
			[descripcion=>o9,probabilidad=>0.96,costo=>16.0,recompensa=>0.0],
			[descripcion=>o10,probabilidad=>0.97,costo=>12.0,recompensa=>0.0]             
			]
		]
	],
	[
		id=>o7,
		id_padre=>c4,
		[nombre=>estante2],
		[movimiento=>[
			[descripcion=>o5,probabilidad=>0.98,costo=>8.0,recompensa=>0.0],
			[descripcion=>o6,probabilidad=>0.97,costo=>12.0,recompensa=>0.0],
			[descripcion=>o7,probabilidad=>1.00,costo=>0.0,recompensa=>0.0],
			[descripcion=>o8,probabilidad=>0.98,costo=>6.0,recompensa=>0.0],
			[descripcion=>o9,probabilidad=>0.98,costo=>8.0,recompensa=>0.0],
			[descripcion=>o10,probabilidad=>0.96,costo=>16.0,recompensa=>0.0]             
			]
		]
	],
	[
		id=>o8,
		id_padre=>c4,
		[nombre=>mesa1],
		[movimiento=>[
			[descripcion=>o5,probabilidad=>0.98,costo=>9.0,recompensa=>0.0],
			[descripcion=>o6,probabilidad=>0.97,costo=>15.0,recompensa=>0.0],
			[descripcion=>o7,probabilidad=>0.98,costo=>6.0,recompensa=>0.0],
			[descripcion=>o8,probabilidad=>1.00,costo=>0.0,recompensa=>0.0],
			[descripcion=>o9,probabilidad=>0.99,costo=>2.0,recompensa=>0.0],
			[descripcion=>o10,probabilidad=>0.97,costo=>13.0,recompensa=>0.0]             
			]
		]
	],
	[
		id=>o9,
		id_padre=>c4,
		[nombre=>mesa2],
		[movimiento=>[
			[descripcion=>o5,probabilidad=>0.98,costo=>10.0,recompensa=>0.0],
			[descripcion=>o6,probabilidad=>0.96,costo=>16.0,recompensa=>0.0],
			[descripcion=>o7,probabilidad=>0.98,costo=>8.0,recompensa=>0.0],
			[descripcion=>o8,probabilidad=>0.99,costo=>2.0,recompensa=>0.0],
			[descripcion=>o9,probabilidad=>1.00,costo=>0.0,recompensa=>0.0],
			[descripcion=>o10,probabilidad=>0.97,costo=>12.0,recompensa=>0.0]             
			]
		]
	],
	[
		id=>o10,
		id_padre=>c4,
		[nombre=>mesa3],
		[movimiento=>[
			[descripcion=>o5,probabilidad=>0.98,costo=>9.0,recompensa=>0.0],
			[descripcion=>o6,probabilidad=>0.97,costo=>12.0,recompensa=>0.0],
			[descripcion=>o7,probabilidad=>0.96,costo=>16.0,recompensa=>0.0],
			[descripcion=>o8,probabilidad=>0.97,costo=>13.0,recompensa=>0.0],
			[descripcion=>o9,probabilidad=>0.97,costo=>12.0,recompensa=>0.0],
			[descripcion=>o10,probabilidad=>1.00,costo=>0.0,recompensa=>0.0]             
			]
		]
	],
	[
		id=>c11,
		id_padre=>c0,
		[nombre=>cosas],
		[]
	],
	[
		id=>o12,
		id_padre=>c11,
		[
			nombre=>hamburguesa,
			acciones=>[
				[descripcion=>buscar,probabilidad=>0.95,costo=>5.0,recompensa=>50.0],
				[descripcion=>agarrar,probabilidad=>0.90,costo=>8.0,recompensa=>100.0],
				[descripcion=>entregar,probabilidad=>0.99,costo=>5.0,recompensa=>300.0]
			]
		],
		[ubicacion=>o6]
	],
	[
		id=>o13,
		id_padre=>c11,
		[
			nombre=>sandwich,
			acciones=>[
				[descripcion=>buscar,probabilidad=>0.80,costo=>5.0,recompensa=>50.0],
				[descripcion=>agarrar,probabilidad=>0.85,costo=>9.0,recompensa=>100.0],
				[descripcion=>entregar,probabilidad=>0.99,costo=>5.0,recompensa=>300.0]
			]
		],
		[ubicacion=>o6]
	],
	[
		id=>o14,
		id_padre=>c11,
		[
			nombre=>refresco,
			acciones=>[
				[descripcion=>buscar,probabilidad=>0.96,costo=>2.0,recompensa=>50.0],
				[descripcion=>agarrar,probabilidad=>0.95,costo=>2.0,recompensa=>80.0],
				[descripcion=>entregar,probabilidad=>0.99,costo=>3.0,recompensa=>300.0]
			]
		],
		[ubicacion=>o7]
	],
	[
		id=>o15,
		id_padre=>c11,
		[
			nombre=>agua,
			acciones=>[
				[descripcion=>buscar,probabilidad=>0.70,costo=>15.0,recompensa=>50.0],
				[descripcion=>agarrar,probabilidad=>0.95,costo=>10.0,recompensa=>80.0],
				[descripcion=>entregar,probabilidad=>0.99,costo=>10.0,recompensa=>300.0]
			]
		],
		[ubicacion=>o7]
	],
	[
		id=>o16,
		id_padre=>c11,
		[
			nombre=>cafe,
			acciones=>[
				[descripcion=>buscar,probabilidad=>0.85,costo=>10.0,recompensa=>50.0],
				[descripcion=>agarrar,probabilidad=>0.70,costo=>20.0,recompensa=>15.0],
				[descripcion=>entregar,probabilidad=>0.85,costo=>20.0,recompensa=>400.0]
			]
		],
		[ubicacion=>o7]
	]
].