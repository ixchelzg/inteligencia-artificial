:- op(15, xfx, '=>').

main:- consult('Manejo_de_archivos/main.pl').
guardarBD(Y):-
	save_kb('pruVladi.pl',Y).
rb(Y):- open_kb('Manejo_de_archivos/bd.txt',Y).


cabeza([H|T],Y):- Y=H.
cola([H|T],Y):- Y=T.
cabeza(X,Y):- Y=X.

quieroClase(X,[H|T],P):-
	nth0(2,H,Props),
	cabeza(Props,S),
	segundoTermino(S,X)->
		P=H ; quieroClase(X,T,P).

set([],[]).
set([H|T],[H|Out]) :-
	primerTermino(H,Y),
	Cp= Y=>X,
	not(member(Cp,T)),
    set(T,Out).
set([H|T],Out) :-
	primerTermino(H,Y),
	Cp= Y=>X,
    member(Cp,T),
    set(T,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


esCorrecto([]).
esCorrecto([H|T]) :-
	H = =>(X,Y),
	esCorrecto(T).


primerTermino(X,Y) :-
	X = =>(Y,W).
segundoTermino(X,Y):- 
	X = =>(W,Y).

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
	rb(X), [H|T] = Rels,
	(esCorrecto(Rels) -> true; nl,write('Escribe las relaciones de la forma x=>y'),nl,false),
	quieroClase(Nom,X,Cla),
	([] \= Cla -> true; nl,write('Esa Clase u Objeto no existe'),nl,false),
	nth0(3,Cla,Re),
	append(Re,Rels,Rel),
	nth0(3,Cla,E,R),
	nth0(3,L,Rel,R),
	select(Cla,X,L,Y),!,
	guardarBD(Y).





