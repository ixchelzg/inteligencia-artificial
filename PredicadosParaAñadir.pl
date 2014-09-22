%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Recibe un Nombre de la nueva clase y el que sería su padre
anadeClase(Nom,Pad) :-
	rb(X),
	quieroClase(Pad,X,Cla),
	nth0(0,Cla,IdP),
	segundoTermino(IdP,IdPa),
	IdPad = id_padre=>IdPa,
	nuevoId(I),
	atomic_concat(c,I,Id),
	Pr = [nombre=>Nom],
	Clase = [id=>Id,IdPad,Pr,[]],
	sustPadre(IdPad,X,id_padre=>Id),
	reverse(X,Y),
	Z = [Clase|Y],
	reverse(Z,S),
	guardarBD(S).
% Recibe el Nombre, el Padre, y las listas de Props y Rels
anadeClase(Nom,Pad,Props,Rels) :-
	rb(X),
	quieroClase(Pad,X,Cla),
	nth0(0,Cla,IdP),
	segundoTermino(IdP,IdPa),
	IdPad = id_padre=>IdPa,
	nuevoId(I),
	atomic_concat(c,I,Id),
	Pr = [nombre=>Nom|Props], [H|T] = Rels,
	Clase = [id=>Id,IdPad,Pr,Rels],
	sustPadre(IdPad,X,id_padre=>Id),
	reverse(X,Y),
	Z = [Clase|Y],
	reverse(Z,S),
	guardarBD(S).

% Recibe el Nombre del Objeto, y quien sería su Padre
anadeObjeto(Nom,Pad) :-
	rb(X),
	quieroClase(Pad,X,Cla),
	nth0(0,Cla,IdP),
	segundoTermino(IdP,IdPa),
	IdPad = id_padre=>IdPa,
	nuevoId(I),
	atomic_concat(o,I,Id),
	Pr = [nombre=>N],
	Clase = [id=>Id,IdPad,Pr,[]],
	reverse(X,Y),
	Z = [Clase|Y],
	reverse(Z,S),
	guardarBD(S).
% Recibe el Nombre, el Padre, y las listas de Props y Rels
anadeObjeto(Nom,Pad,Props,Rels) :-
	rb(X),
	quieroClase(Pad,X,Cla),
	nth0(0,Cla,IdP),
	segundoTermino(IdP,IdPa),
	IdPad = id_padre=>IdPa,
	nuevoId(I),
	atomic_concat(o,I,Id),
	Pr = [nombre=>Nom|Props], [H|T] = Rels,
	Clase = [id=>Id,IdPad,Pr,Rels],
	reverse(X,Y),
	Z = [Clase|Y],
	reverse(Z,S),
	guardarBD(S).

% Recibe el Nombre de la Clase u Objeto, y una lista de Propiedades
% auqnue sólo sea una propiedad, tiene que ser en una lista
anadePropiedad(Nom,Props) :-
	rb(X), [H|T] = Props,
	quieroClase(Nom, X, Cla),
	nth0(2,Cla,Pr),
	append(Pr,Props,Pro),
	nth0(2,Cla,E,R),
	nth0(2,L,Pro,R),
	select(Cla,X,L,Y),!,
	guardarBD(Y).
% Lo mismo pero para Relaciones
anadeRelacion(Nom,Rels) :-
	rb(X), [H|T] = Rels,
	quieroClase(Nom, X, Cla),
	nth0(3,Cla,Re),
	append(Re,Rels,Rel),
	nth0(3,Cla,E,R),
	nth0(3,L,Rel,R),
	select(Cla,X,L,Y),!,
	guardarBD(Y).

% EN REMODELACION %
sustPadre(Pa,[],NPa).
sustPadre(Pa,[H|T],NPa) :-
	H = [_,Pa,_,_],
	nth0(1,H,E,R),
	nth0(1,L,NPa,R),
	sustPadre(Pa,T,NPa),
	guardarBD([L|T]).
sustPadre(Pa,[H|T],NPa) :- \+
	H = [_,Pa,_,_],
	sustPadre(Pa,T,NPa).
%%%%%%%%%%%%%%%%%%%

nuevoId(Id) :-
	rb(X),
	reverse(X,Y),
	[H|T] = Y,
	[A|B] = H,
	segundoTermino(A,Idmax),
	atom_length(Idmax, Len),
	L is Len - 1,
	sub_atom(Idmax, 1, L, A, S),
	atom_number(S,S1), 
	Id is S1 + 1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%