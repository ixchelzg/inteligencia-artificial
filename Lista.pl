% Operador para indicar una relación.
:- op(15, xfx, '=>').
=>(X,Y).

% Regresan ya sea el lado izquierdo o derecho del operador.
primerTermino(X,Y):- X = W=>Z, 
								Y=W.
segundoTermino(X,Y):- X = W=>Z, 
								Y=Z.

% Busca en una lista de atributos si alguno coincide con el pedido y regresa su valor en caso de que sea así.
valor(X,[],Y):- fail.
valor(X,[H|T],Y):- primerTermino(H,Z), 
								X=Z, 
								segundoTermino(H,W), 
								Y=W, 
								!.
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

% Regresa una tupla entera de la base de datos según un nombre.
regresaTuplaPorNombreInicio(X,Y):- rb(W), 
								regresaTuplaPorNombre(X,W,Y),!.
regresaTuplaPorNombre(X,[],Y):-fail.
regresaTuplaPorNombre(X,[H|T],Y):- regresaNombre(H,S), 
								X==S, 
								Y = H, 
								!; 
								regresaTuplaPorNombre(X,T,Y), 
								!. 

% 1a Regresa la extensión de una clase. 
% Ej.
% ?- extensionDeUnaClaseInicio(ave,Y).
% Y = [phoenix, hugo, paco, luis, 'aguila calva'].
extensionDeUnaClaseInicio(X,Y):- rb(W), 
								regresaTuplaPorNombre(X,W,S), 
								regresaId(S,L), 
								extensionDeUnaClase(L,W,Y), 
								!.
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
								append(P,R,Y),
								!;
								regresaId_padre(H,S), 
								X==S,
								regresaNombre(H,N),
								regresaId(H,I), 
								string_chars(I,J), 
								J=[C|V], 
								C == 'o',
								extensionDeUnaClase(X,T,R), 
								append([N],R,Y),
								!; 
								extensionDeUnaClase(X,T,Y),
								!.

rb(Y):- Y = [
	[id=>c1, id_padre=>c0, [nombre=>animal ,vida=>finita, ojos=>2],[odia=>c20]],
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
	[id=>o13, id_padre=>c12, [nombre=>leon, armas=>garra], [come=>c20]],
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