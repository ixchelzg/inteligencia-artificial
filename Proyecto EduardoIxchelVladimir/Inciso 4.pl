
:- op(15, xfx, '=>').

modificaNombre(X,Y):-
	rb(W), quieroClase(Y,W,P), creaNuevoNombre(P,X).

modificaPropiedad(P,X,P1):-
	rb(W), quieroClase(X,W,Cl),
	nth0(2,Cl,Props),
	member(P=>Xv,Props),
	sus(P=>Xv,P=>P1,Props,S), sus(Props,S,Cl,SC), sus(Cl,SC,W,S1),
	guardarBD(S1), write('Base actualizada... Listo').

modificaRelacion(P,X,P1):-
	rb(W),
	quieroClase(X,W,Cl), quieroClase(P1,W,Cl1),
	nth0(3,Cl,Rels), nth0(0,Cl1,IDCl1), segundoTermino(IDCl1,Id),
	member(P=>Xv,Rels),
	sus(P=>Xv,P=>Id,Rels,S), sus(Rels,S,Cl,H1), sus(Cl,H1,W,S1),
	guardarBD(S1), write('Base actualizada... Listo').

creaNuevoNombre(P,X):-
	rb(W),
	quieroClase(X,W,P1)-> 
		( write('No puedes usar ese nombre, ese nombre ya existe.') ) ;
		(
			nth0(2,P,Props),
			cabeza(Props,Ca),
			sus(Ca,nombre=>X,Props,S),
			sus(Props,S,P,H1),
			rb(W),
			sus(P,H1,W,S1),
			guardarBD(S1)
		).