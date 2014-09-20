% Operador para indicar una relación.
:- op(15, xfx, '=>').
=>(X,Y).
% Regresan ya sea el lado izquierdo o derecho del operador.
primerTermino(X,Y):- X = W=>Z, Y=W.
segundoTermino(X,Y):- X = W=>Z, Y=Z.
% Busca en una lista de atributos si alguno coincide con el pedido y regresa su valor en caso de que sea así.
valor(X,[],Y):- fail.
valor(X,[H|T],Y):- primerTermino(H,Z), X=Z, segundoTermino(H,W), Y=W, !.
valor(X,[H|T],Y):- primerTermino(H,Z), X\=Z, valor(X,T,Y).
% Regresa la cabeza o la cola de un objeto que puede ser una lista o un átomo.
cabeza([H|T],Y):- Y=H.
cabeza(X,Y):- Y=X.
cola([H|T],Y):- Y=T.
cola(X,Y).
% Regresan partes de la tupla.
regresaId([H|T],Y):- valor(id,H,Y).
regresaId_padre([H|T],Y):-valor(id_padre,H,Y).
regresaPropiedades([H|T],Y):- cola(T,U), cabeza(U,Y).
regresaRelaciones([H|T],Y):- cola(T,S), cola(S,Y).
% Regresa el nombre asociado a una tupla
regresaNombre(X,Y):- regresaPropiedades(X,S), valor(nombre,S,Y).
% Regresa una tupla entera de la base de datos según un nombre.
regresaTuplaPorNombreInicio(X,Y):- rb(W), regresaTuplaPorNombre(X,W,Y),!.
regresaTuplaPorNombre(X,[],Y):-fail.
regresaTuplaPorNombre(X,[H|T],Y):- regresaNombre(H,S), X==S, Y = H, !; regresaTuplaPorNombre(X,T,Y), !. 

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