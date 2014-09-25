
:- op(15, xfx, '=>').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
esCorrecto([]).
esCorrecto([H|T]) :-
	H = =>(X,Y),
	esCorrecto(T).

existeInicio(X) :-
	rb(W), existe(X,W).
existe(X,[]) :- false.
existe(X,[H|T]) :-
	nth0(0,H,H1),
	segundoTermino(H1, S),
	X==S,!;
	existe(X,T),!.

verificaLista([]).
verificaLista([H|T]) :-
	segundoTermino(H,S),
	existeInicio(S),
	verificaLista(T),!;
	segundoTermino(H,S),
	\+existeInicio(S),
	nl, write('No es posible '), write(S), write(' en '), write(H),false,!.


nuevoId(Id) :-
	rb(X),
	reverse(X,Y),
	[H|T] = Y,
	[A|B] = H,
	segundoTermino(A,Idmax), 
	atom_length(Idmax, Len), 
	L is Len - 1, 
	sub_atom(Idmax, 1, L, W, S), 
	atom_number(S,S1), 
	Id is S1 + 1, nl, write(Id),nl.

% Recibe un Nombre de la nueva clase, el que sería su padre, y el hijo.
anadeClase(Nom,Pad,H) :-
	rb(X),
	(\+quieroClase(Nom,X,Lol) -> true; nl,write('Esa Clase u Objeto ya existe'),nl,false),
	(quieroClase(Pad,X,Cla) -> true; nl,write('Ese Padre no existe'),nl,false), 
	(quieroClase(H,X,Hi) -> true; nl,write('Ese Hijo no existe'),nl,false), 
	nth0(0,Cla,IdP),
	segundoTermino(IdP,IdPa),
	(nth0(1,Hi,id_padre=>IdPa) -> true; nl,write('El Padre y el Hijo son incompatibles'),nl,false), 
	atom_chars(IdPa, C), 
	(nth0(0,C,c) -> true; nl,write('Ese Padre no es una Clase'),nl,false),
	IdPad = id_padre=>IdPa,
	nuevoId(I),
	atomic_concat(c,I,Id),
	Pr = [nombre=>Nom],
	Clase = [id=>Id,IdPad,Pr,[]],
	select(IdPad,Hi,id_padre=>Id,Hij),!,
	select(Hi,X,Hij,Y),
	proper_length(Y,L), nth0(L,Z,Clase,Y),
	guardarBD(Z).
% Recibe un Nombre de la nueva clase, el que sería su padre,
% una lista de Propiedades, una lista de Relaciones y el hijo.
anadeClase(Nom,Pad,H,Props,Rels) :-
	rb(X),
	(\+quieroClase(Nom,X,Lol) -> true; nl,write('Esa Clase u Objeto ya existe'),nl,false),
	(quieroClase(Pad,X,Cla) -> true; nl,write('Ese Padre no existe'),nl,false), 
	(quieroClase(H,X,Hi) -> true; nl,write('Ese Hijo no existe'),nl,false), 
	nth0(0,Cla,IdP),
	segundoTermino(IdP,IdPa),
	(nth0(1,Hi,id_padre=>IdPa) -> true; nl,write('El Padre y el Hijo son incompatibles'),nl,false), 
	atom_chars(IdPa, C), 
	(nth0(0,C,c) -> true; nl,write('Ese Padre no es una Clase'),nl,false),
	IdPad = id_padre=>IdPa,
	nuevoId(I),
	atomic_concat(c,I,Id),
	(esCorrecto(Props) -> true; nl,write('Escribe las propiedades de la forma x=>y'),nl,false),
	Pro = [nombre=>Nom|Props], set(Pro,Pr),
	([U|T] = Rels -> true; nl,write('Relaciones no puede ser vacia'),nl, false),
	(esCorrecto(Rels) -> true; nl,write('Escribe las relaciones de la forma x=>y'),nl,false),
	verificaLista(Rels),
	Clase = [id=>Id,IdPad,Pr,Rela],
	quieroClase(H,X,Hi),
	select(IdPad,Hi,id_padre=>Id,Hij),!,
	select(Hi,X,Hij,Y),
	proper_length(Y,L), nth0(L,Z,Clase,Y),
	guardarBD(Z).


% Recibe un Nombre de la nueva clase y el que sería su padre
anadeClase(Nom,Pad) :-
	rb(X),
	(\+quieroClase(Nom,X,Lol) -> true; nl,write('Esa Clase u Objeto ya existe'),nl,false),
	(quieroClase(Pad,X,Cla) -> true; nl,write('Ese Padre no existe'),nl,false), 
	nth0(0,Cla,IdP),
	segundoTermino(IdP,IdPa),
	atom_chars(IdPa, C), 
	(nth0(0,C,c) -> true; nl,write('Ese Padre no es una Clase'),nl,false),
	IdPad = id_padre=>IdPa,
	nuevoId(I),
	atomic_concat(c,I,Id),
	Pr = [nombre=>Nom],
	Clase = [id=>Id,IdPad,Pr,[]],
	proper_length(X,L), nth0(L,Z,Clase,X),
	guardarBD(Z).
% Recibe el Nombre, el Padre, y las listas de Props y Rels
anadeClase(Nom,Pad,Props,Rels) :-
	rb(X),
	(\+quieroClase(Nom,X,Lol) -> true; nl,write('Esa Clase u Objeto ya existe'),nl,false),
	(quieroClase(Pad,X,Cla) -> true; nl,write('Ese Padre no existe'),nl,false), 
	nth0(0,Cla,IdP),
	segundoTermino(IdP,IdPa),
	atom_chars(IdPa, C), 
	(nth0(0,C,c) -> true; nl,write('Ese Padre no es una Clase'),nl,false),
	IdPad = id_padre=>IdPa,
	nuevoId(I),
	atomic_concat(c,I,Id),
	(esCorrecto(Props) -> true; nl,write('Escribe las propiedades de la forma x=>y'),nl,false),
	Pro = [nombre=>Nom|Props], set(Pro,Pr),
	([U|T] = Rels -> true; nl,write('Relaciones no puede ser vacia'),nl, false),
	(esCorrecto(Rels) -> true; nl,write('Escribe las relaciones de la forma x=>y'),nl,false),
	verificaLista(Rels),
	Clase = [id=>Id,IdPad,Pr,Rels],
	proper_length(X,L), nth0(L,Z,Clase,X),
	guardarBD(Z).

% Recibe el Nombre del Objeto, y quien sería su Padre
anadeObjeto(Nom,Pad) :-
	rb(X),
	(\+quieroClase(Nom,X,Lol) -> true; nl,write('Esa Clase u Objeto ya existe'),nl,false),
	(quieroClase(Pad,X,Cla) -> true; nl,write('Ese Padre no existe'),nl,false), 
	nth0(0,Cla,IdP),
	segundoTermino(IdP,IdPa),
	atom_chars(IdPa, C),
	(nth0(0,C,c) -> true; nl,write('Ese Padre no es una Clase'),nl,false),
	IdPad = id_padre=>IdPa,
	nuevoId(I),
	atomic_concat(o,I,Id),
	Pr = [nombre=>Nom],
	Clase = [id=>Id,IdPad,Pr,[]],
	proper_length(X,L), nth0(L,Z,Clase,X),
	guardarBD(Z).
% Recibe el Nombre, el Padre, y las listas de Props y Rels
anadeObjeto(Nom,Pad,Props,Rels) :-
	rb(X),
	(\+quieroClase(Nom,X,Lol) -> true; nl,write('Esa Clase u Objeto ya existe'),nl,false),
	(quieroClase(Pad,X,Cla) -> true; nl,write('Ese Padre no existe'),nl,false), 
	nth0(0,Cla,IdP),
	segundoTermino(IdP,IdPa),
	atom_chars(IdPa, C), nth0(0,C,c),
	IdPad = id_padre=>IdPa,
	nuevoId(I),
	atomic_concat(o,I,Id),
	(esCorrecto(Props) -> true; nl,write('Escribe las propiedades de la forma x=>y'),nl,false),
	Pro = [nombre=>Nom|Props], set(Pro,Pr),
	([U|T] = Rels -> true; nl,write('Relaciones no puede ser vacia'),nl, false),
	(esCorrecto(Rels) -> true; nl,write('Escribe las relaciones de la forma x=>y'),nl,false),
	verificaLista(Rels),
	Clase = [id=>Id,IdPad,Pr,Rels],
	proper_length(X,L), nth0(L,Z,Clase,X),
	guardarBD(Z).

% Recibe el Nombre de la Clase u Objeto, y una lista de Propiedades
% auqnue sólo sea una propiedad, tiene que ser en una lista
anadePropiedad(Nom,Props) :-
	rb(X), [H|T] = Props,
	(esCorrecto(Props) -> true; nl,write('Escribe las propiedades de la forma x=>y'),nl,false),
	quieroClase(Nom,X,Cla),
	([] \= Cla -> true; nl,write('Esa Clase u Objeto no existe'),nl,false),
	nth0(2,Cla,Pr),
	append(Props,Pr,Pro), set(Pro, Prop),
	nth0(2,Cla,E,R),
	nth0(2,L,Prop,R),
	select(Cla,X,L,Y),!,
	guardarBD(Y).

% Lo mismo pero para Relaciones
anadeRelacion(Nom,Rels) :-
	rb(X), 
	([H|T] = Rels -> true; nl,write('Relaciones no puede ser vacia'),nl, false),
	(esCorrecto(Rels) -> true; nl,write('Escribe las relaciones de la forma x=>y'),nl,false),
	verificaLista(Rels),
	quieroClase(Nom,X,Cla),
	([] \= Cla -> true; nl,write('Esa Clase u Objeto no existe'),nl,false),
	nth0(3,Cla,Re),
	append(Re,Rels,Rel),
	nth0(3,Cla,E,R),
	nth0(3,L,Rel,R),
	select(Cla,X,L,Y),!,
	guardarBD(Y).





