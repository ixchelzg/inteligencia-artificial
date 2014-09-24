%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Equipo					%%
%%		Ixchel Zazueta		%%
%%		Vladimir Zanchez	%%
%%		Eduardo Tello		%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Carga librerias necesarias.
:- initialization(main).
% Operador para indicar una relación.
:- op(15, xfx, '=>').
=>(X,Y).

% Carga archivo de librerias para manejo de archivos.
main:-
	consult('C:\\Users\\USUARIO\\Desktop\\inteligencia-artificial\\Manejo_de_archivos\\main.pl').

% Regresa la base de datos con la que se trabaja.
rb(Y):-
	open_kb('C:\\Users\\USUARIO\\Desktop\\inteligencia-artificial\\Manejo_de_archivos\\bdSistemaSolar.txt',Y).

% Regresan ya sea el lado izquierdo o derecho del operador (siempre y cuando estos sean átomos).
primerTermino(X,Y):- X = W=>Z, 
								Y=W.
segundoTermino(X,Y):- X = W=>Z, 
								Y=Z.

% Busca en una lista de atributos si alguno coincide con el pedido y regresa su valor en caso de que sea así.
valor(X,[],Y):- fail.
valor(X,[H|T],Y):- primerTermino(H,Z), 
								X=Z, 
								segundoTermino(H,W), 
								Y=W,!.
valor(X,[H|T],Y):- primerTermino(H,Z), 
								X\=Z, 
								valor(X,T,Y).

% Regresan partes de la tupla.
regresaId([H|T],Y):- valor(id,[H],Y).
regresaId_padre([H|T],Y):- valor(id_padre,T,Y).
regresaPropiedades([H|T],Y):- T=[HT|TT], 
								TT = [Y|TTT].
regresaRelaciones([H|T],Y):- T=[HT|TT], 
								TT = [HTT|Y].

% Regresa el nombre asociado a una tupla.
regresaNombre(X,Y):- regresaPropiedades(X,S), 
								valor(nombre,S,Y).

% Regresa el nombre mas el operador asociado a una tupla.
regresaNombreCompleto(X,Y):- regresaPropiedades(X,S), 
								S=[H|T], Y = H.

% Regresa una tupla entera de la base de datos según un nombre.
regresaTuplaPorNombreInicio(X,Y):- rb(W), 
								regresaTuplaPorNombre(X,W,Y),!.
regresaTuplaPorNombre(X,[],Y):-fail.
regresaTuplaPorNombre(X,[H|T],Y):- regresaNombre(H,S), 
								X==S, 
								Y = H, !
								; 
								regresaTuplaPorNombre(X,T,Y), !.

% Regresa una tupla entera de la base de datos según un id.
regresaTuplaPorIdInicio(X,Y):- rb(W), 
								regresaTuplaPorId(X,W,Y),!.
regresaTuplaPorId(X,[],Y):-fail.
regresaTuplaPorId(X,[H|T],Y):- regresaId(H,S), 
								X==S, 
								Y = H, !
								; 
								regresaTuplaPorId(X,T,Y), !.  

% 1a Regresa la extensión de una clase. 
% La extensión de una clase (el conjunto de todos los objetos que pertenecen a la misma, ya sea porque se declaren directamente o porque están en la cerradura de la relación de herencia).
% Ej.
% ?- extensionDeUnaClaseInicio(ave,Y).
% Y = [phoenix, hugo, paco, luis, 'aguila calva'].

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

% 1b Regresa la extensión de una propiedad. 
% La extensión de una propiedad (mostrar todos los objetos que tienen una propiedad específica ya sea por declaración directa o por herencia, incluyendo su respectivo valor).
% Ej. buscaPropiedadEnlistaDePropiedadesDeObjetosInicio(vida,Y).
% Y = ['estrella de mar: vida=>finita', 'gusano: vida=>finita', 'hormiga: vida=>finita', 'mosca: vida=>finita', 'delfin: vida=>finita', 'phoenix: vida=>infinita', 'leon: vida=>finita', 'hugo: vida=>finita', 'paco: vida=>finita', 'luis: vida=>finita', 'aguila calva: vida=>finita', 'flippy: vida=>finita'].

% Regresa las propiedades Heredadas de un elemenmto de la base de datos, se detiene al llegar al elemento raiz.
propiedadesHeredadasDeUnArticuloInicio(X,Y):- rb(W),
								regresaTuplaPorNombre(X,W,S),
								regresaId_padre(S,L), 
								L \= c0,
								regresaTuplaPorId(L,W,N),
								regresaNombre(N,M),
								regresaPropiedades(N,J),
								propiedadesHeredadasDeUnArticuloInicio(M,K),
								append(J,K,Y),!
								;
								Y = [].

% Regresa las propiedades Heredadas y propias de un elemento de la base de datos.
propiedadesHeredadasYPropiasDeUnArticulo(X,Y):- rb(W),
								regresaTuplaPorNombre(X,W,S),
								regresaPropiedades(S,J),
								propiedadesHeredadasDeUnArticuloInicio(X,K),
								append(J,K,Y), !.

% Regresa la lista de propiedade según la ley de monoticidad.
propiedadesMonotonicasHeredadasYPropiasDeUnArticulo(X,Y):- propiedadesHeredadasYPropiasDeUnArticulo(X,S),
								borraPropiedadesRepetidas(S,Y).


% Borra una propiedad de la lista de propiedades.
borraPropiedadDeListaDePropiedades([],X,Y):- Y = [].
borraPropiedadDeListaDePropiedades([H|T],X,Y):-primerTermino(H,L), 
								X==L,
								borraPropiedadDeListaDePropiedades(T,X,R), 
								Y = R,!
								; 
								borraPropiedadDeListaDePropiedades(T,X,R), 
								append([H],R,Y),!.

% Borra las propiedades repetidas de una lista dando preferencia a las de lo hijos sobre las de los padres. (asi aseguramos Monoticidad).
borraPropiedadesRepetidas([],Y):- Y = [].
borraPropiedadesRepetidas([H|T],Y):- primerTermino(H,L),
								borraPropiedadDeListaDePropiedades(T,L,R),
								borraPropiedadesRepetidas(R,P),
								append([H],P,Y), !.

% Regresa lista de propiedades (ya sin repetidos) para todos los Objetos de la base de datos.
haceListaDePropiedadesParaTodosLosObjetosInicio(Y):- rb(W), 
								haceListaDePropiedadesParaTodosLosObjetos(W,Y).
haceListaDePropiedadesParaTodosLosObjetos([],Y):- Y = [].
haceListaDePropiedadesParaTodosLosObjetos([H|T],Y):- regresaId(H,I), 
								string_chars(I,J), 
								J=[C|V], 
								C == 'o',
								regresaNombre(H,P),
								propiedadesMonotonicasHeredadasYPropiasDeUnArticulo(P,R),
								haceListaDePropiedadesParaTodosLosObjetos(T,S),
								append([R],S,Y),!
								;
								regresaNombre(H,P),
								haceListaDePropiedadesParaTodosLosObjetos(T,S),
								Y = S,!.

% Busca una propiedad en particular en una lista de propiedades pára cada Objeto, lo regresa en el formato 'nombre_objeto: propiedad=>valor', hace esto para cada objeto de la base de datos por lo que al final regresa un lista.
buscaPropiedadEnlistaDePropiedadesDeObjetosInicio(X,Y):-haceListaDePropiedadesParaTodosLosObjetosInicio(W), 
								buscaPropiedadEnlistaDePropiedadesDeObjetos(X,W,Y).
buscaPropiedadEnlistaDePropiedadesDeObjetos(X,[],Y):- Y = [],!.
buscaPropiedadEnlistaDePropiedadesDeObjetos(X,[H|T],Y):- 
								valor(X,H,S),
								valor(nombre,H,U),
								buscaPropiedadEnlistaDePropiedadesDeObjetos(X,T,M),
								concat(U,': ',N),
								concat(N,X,L),
								concat(L,'=>',D),
								concat(D,S,A),
								append([A],M,Y),!
								;
								buscaPropiedadEnlistaDePropiedadesDeObjetos(X,T,M), 
								Y = M,!.

% 1c Regresa la extensión de una relación. 
% La extensión de una relación (mostrar todos los objetos que tienen una relación específica ya sea por declaración directa o por herencia, incluyendo con quién estén relacionados).
% Ej. ?- buscaRelacionEnlistaDeRelacionesDeObjetosInicio(come,Y).
% Y = ['hormiga: come=>gusano', 'mosca: come=>gusano', 'phoenix: come=>leon', 'leon: come=>pinguino', 'hugo: come=>pez', 'paco: come=>pez', 'luis: come=>pez', 'flippy: come=>gusano'].

% Regresa las relaciones heredadas de un elemento de la base de datos.
relacionesHeredadasDeUnArticuloInicio(X,Y):- rb(W),
								regresaTuplaPorNombre(X,W,S),
								regresaId_padre(S,L), 
								L \= c0,
								regresaTuplaPorId(L,W,N),
								regresaNombre(N,M),
								regresaRelaciones(N,J),
								J=[H|T],
								relacionesHeredadasDeUnArticuloInicio(M,K),
								append(H,K,Y),!
								;
								Y = [].			

% Regresa las relaciones heredadas y propias de un elemento de la base de datos.
relacionesHeredadasYPropiasDeUnArticulo(X,Y):- rb(W),
								regresaTuplaPorNombre(X,W,S),
								regresaRelaciones(S,J),
								J=[H|T],
								relacionesHeredadasDeUnArticuloInicio(X,K),
								append(H,K,G),
								regresaNombreCompleto(S,C),
								append([C],G,Y),
								!.

% Regresa la lista de relaciones según la ley de monoticidad.
relacionesMonotonicasHeredadasYPropiasDeUnArticulo(X,Y):- relacionesHeredadasYPropiasDeUnArticulo(X,S),
								borraRelacionesRepetidas(S,Y).

% Borra una realción de la lista de relaciones.
borraRelacionDeListaDeRelaciones([],X,Y):- Y = [].
borraRelacionDeListaDeRelaciones([H|T],X,Y):-primerTermino(H,L), 
								X==L,
								borraRelacionDeListaDeRelaciones(T,X,R), 
								Y = R,!
								; 
								borraRelacionDeListaDeRelaciones(T,X,R), 
								append([H],R,Y),!.

% Borra las relaciones repetidas de una lista dando preferencia a las de lo hijos sobre las de los padres. (asi aseguramos Monoticidad).
borraRelacionesRepetidas([],Y):- Y = [].
borraRelacionesRepetidas([H|T],Y):- 
								primerTermino(H,L),
								borraRelacionDeListaDeRelaciones(T,L,R),
								borraRelacionesRepetidas(R,P),
								append([H],P,Y),
								!.

% Regresa lista de relaciones (ya sin repetidos) para todos los Objetos de la base de datos.
haceListaDeRelacionesParaTodosLosObjetosInicio(Y):- rb(W), 
								haceListaDeRelacionesParaTodosLosObjetos(W,Y).

haceListaDeRelacionesParaTodosLosObjetos([],Y):- Y = [].
haceListaDeRelacionesParaTodosLosObjetos([H|T],Y):- regresaId(H,I), 
								string_chars(I,J), 
								J=[C|V], 
								C == 'o',
								regresaNombre(H,P),
								relacionesMonotonicasHeredadasYPropiasDeUnArticulo(P,R),
								haceListaDeRelacionesParaTodosLosObjetos(T,S),
								append([R],S,Y),!
								;
								regresaNombre(H,P),
								haceListaDeRelacionesParaTodosLosObjetos(T,S),
								Y = S,!.

% Busca una relación en particular en una lista de propiedades pára cada Objeto, lo regresa en el formato 'nombre_objeto: relacion=>valor', hace esto para cada objeto de la base de datos por lo que al final regresa un lista.
buscaRelacionEnlistaDeRelacionesDeObjetosInicio(X,Y):-haceListaDeRelacionesParaTodosLosObjetosInicio(W), 
								buscaRelacionEnlistaDeRelacionesDeObjetos(X,W,Y).
buscaRelacionEnlistaDeRelacionesDeObjetos(X,[],Y):- Y = [],!.
buscaRelacionEnlistaDeRelacionesDeObjetos(X,[H|T],Y):- 
								valor(X,H,S),
								regresaTuplaPorIdInicio(S,I),
								regresaNombre(I,O),
								valor(nombre,H,U),
								buscaRelacionEnlistaDeRelacionesDeObjetos(X,T,M),
								concat(U,': ',N),
								concat(N,X,L),
								concat(L,'=>',D),
								concat(D,O,A),
								append([A],M,Y),!
								;
								buscaRelacionEnlistaDeRelacionesDeObjetos(X,T,M), 
								Y = M,!.


% 1d Regresa las clases padres de un objeto.
% Todas las clases a las que pertenece un objeto.
% Ej.?- clasesPadresDeUnObjetoRevisado(hugo,Y).
% Y = [pato, ave, oviparo, animal].
%
% Lanza mensaje si la entidad consultada no es un Objeto
% Ej.?- clasesPadresDeUnObjetoRevisado(pato,Y).
% pato no es un objeto
% true.

% Regresa las clases padres de un elemento de la base de datos cerciorandose que le identificador del elemento indique que es un objeto.
clasesPadresDeUnObjetoRevisado(X,Y):- rb(W),
								regresaTuplaPorNombre(X,W,S),
								regresaId(S,I), 
								string_chars(I,J), 
								J=[C|V], 
								C == 'o',
								clasesPadresDeUnObjeto(X,Y),!;
								write(X),write(' no es un objeto'),nl,!
								.

% Regresa las clases padres de cualquier elemento en la base de datos.
clasesPadresDeUnObjeto(X,Y):- rb(W),
								regresaTuplaPorNombre(X,W,S),
								regresaId_padre(S,L), 
								L \= c0,
								regresaTuplaPorId(L,W,N),
								regresaNombre(N,M),
								clasesPadresDeUnObjeto(M,K),
								append([M],K,Y),!
								;
								Y = [].


% 1e Regresa las propiedades tanto propias como heredadas de un elemento (clase u objeto indistintamente)
% Todas las propiedades de un objeto o clase
% Para objeto.
% Ej.?- propiedadesMonotonicasHeredadasYPropiasDeUnElemento(phoenix,Y).
% Y = [nombre=>phoenix, vida=>infinita, movimiento=>vuela, nace=>huevo, ojos=>2].
% Para clase.
% Ej.?- propiedadesMonotonicasHeredadasYPropiasDeUnElemento(pato,Y).
% Y = [nombre=>pato, movimiento=>nada, nace=>huevo, vida=>finita, ojos=>2].

% Regresa propiedades heredadas de un elemento cualquiera de la base de datos.
propiedadesHeredadasDeUnElementoInicio(X,Y):- rb(W),
								regresaTuplaPorNombre(X,W,S),
								regresaId_padre(S,L), 
								L \= c0,
								regresaTuplaPorId(L,W,N),
								regresaNombre(N,M),
								regresaPropiedades(N,J),
								propiedadesHeredadasDeUnElementoInicio(M,K),
								append(J,K,Y),!
								;
								Y = [].

% Regresa propiedades heredadas y propias de un elemento cualquiera de la base de datos.
propiedadesHeredadasYPropiasDeUnElemento(X,Y):- rb(W),
								regresaTuplaPorNombre(X,W,S),
								regresaPropiedades(S,J),
								propiedadesHeredadasDeUnElementoInicio(X,K),
								append(J,K,Y), !.

% Regresa la lista de propiedades según la ley de monoticidad.
propiedadesMonotonicasHeredadasYPropiasDeUnElemento(X,Y):- propiedadesHeredadasYPropiasDeUnElemento(X,S),
								borraPropiedadesRepetidasDeUnElemento(S,Y).

% Borra una propiedad de la lista de propiedades.
borraPropiedadDeListaDePropiedadesDeUnElemento([],X,Y):- Y = [].
borraPropiedadDeListaDePropiedadesDeUnElemento([H|T],X,Y):-primerTermino(H,L), 
								X==L,
								borraPropiedadDeListaDePropiedadesDeUnElemento(T,X,R), 
								Y = R,!
								; 
								borraPropiedadDeListaDePropiedadesDeUnElemento(T,X,R), 
								append([H],R,Y),!.

% Borra las propiedades repetidas de una lista dando preferencia a las de lo hijos sobre las de los padres. (asi aseguramos Monoticidad).
borraPropiedadesRepetidasDeUnElemento([],Y):- Y = [].
borraPropiedadesRepetidasDeUnElemento([H|T],Y):- primerTermino(H,L),
								borraPropiedadDeListaDePropiedadesDeUnElemento(T,L,R),
								borraPropiedadesRepetidasDeUnElemento(R,P),
								append([H],P,Y), !.


% 1f Regresa las relaciones tanto propias como heredadas de un elemento (clase u objeto indistintamente)
% Todas las relaciones de un objeto o clase
% Para objeto.
% Ej.?- relacionesMonotonicasHeredadasYPropiasDeUnElementoNombre(hugo,Y).
% Y = ['hermano=>paco', 'come=>pez', 'odia=>viviparo'].
% Para clase.
% Ej.?- relacionesMonotonicasHeredadasYPropiasDeUnElementoNombre(pato,Y).
% Y = ['come=>pez', 'odia=>viviparo'].

% Regresa relaciones heredadas de un elemento cualquiera de la base de datos.
relacionesHeredadasDeUnElementoInicio(X,Y):- rb(W),
								regresaTuplaPorNombre(X,W,S),
								regresaId_padre(S,L), 
								L \= c0,
								regresaTuplaPorId(L,W,N),
								regresaNombre(N,M),
								regresaRelaciones(N,J),
								J=[H|T],
								relacionesHeredadasDeUnElementoInicio(M,K),
								append(H,K,Y),!
								;
								Y = [].	

% Regresa relaciones heredadas y propias de un elemento cualquiera de la base de datos.
relacionesHeredadasYPropiasDeUnElemento(X,Y):- rb(W),
								regresaTuplaPorNombre(X,W,S),
								regresaRelaciones(S,J),
								J=[H|T],
								relacionesHeredadasDeUnElementoInicio(X,K),
								append(H,K,Y),
								!.

% Regresa la lista de propiedades según la ley de monoticidad (con nombre en vez de id).
relacionesMonotonicasHeredadasYPropiasDeUnElementoNombre(X,Y):- relacionesMonotonicasHeredadasYPropiasDeUnElemento(X,S),
								traduceRelacionesDeUnELemento(S,Y).

% Regresa la lista de propiedades según la ley de monoticidad (Id).
relacionesMonotonicasHeredadasYPropiasDeUnElemento(X,Y):- relacionesHeredadasYPropiasDeUnElemento(X,S),
								borraRelacionesRepetidasDeUnElemento(S,Y).

% Borra una propiedad de la lista de propiedades.
borraRelacionDeListaDeRelacionesDeUnElemento([],X,Y):- Y = [].
borraRelacionDeListaDeRelacionesDeUnElemento([H|T],X,Y):-primerTermino(H,L), 
								X==L,
								borraRelacionDeListaDeRelacionesDeUnElemento(T,X,R), 
								Y = R,!
								; 
								borraRelacionDeListaDeRelacionesDeUnElemento(T,X,R), 
								append([H],R,Y),!.

% Borra las relaciones repetidas de una lista dando preferencia a las de lo hijos sobre las de los padres. (asi aseguramos Monoticidad).
borraRelacionesRepetidasDeUnElemento([],Y):- Y = [].
borraRelacionesRepetidasDeUnElemento([H|T],Y):- 
								primerTermino(H,L),
								borraRelacionDeListaDeRelacionesDeUnElemento(T,L,R),
								borraRelacionesRepetidasDeUnElemento(R,P),
								append([H],P,Y),
								!.

% Intercambia Ids por nombres en una lista de relaciones.
traduceRelacionesDeUnELemento([],Y):- Y = []. 
traduceRelacionesDeUnELemento([H|T],Y):- primerTermino(H,P),
								segundoTermino(H,S),
								regresaTuplaPorIdInicio(S,M),
								regresaNombre(M,J),
								concat(P,'=>',D),
								concat(D,J,A),
								traduceRelacionesDeUnELemento(T,K),
								append([A],K,Y),!.
