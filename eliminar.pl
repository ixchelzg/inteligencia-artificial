elimina(X):-
	rb(W),
	quieroClase(X,W,P),
	write(W),
	nth0(2,P,Pr), nth0(3,P,Rel),write(Pr), cola(Pr,Props), valor(id_padre,P,Pop), valor(id,P,ID),
	Pop\=c0, %si no es la clase raiz
	crearNuevaListaInicio(ID,Props,Rel,Pop),
	rb(W1),
	eliminaClase(P,W1,S),
	guardarBD(S).

eliminaClaseConObjetos(X):-
	rb(W),
	quieroClase(X,W,P), nth0(0,P,IdP),
	segundoTermino(IdP,IdPa), atom_chars(IdPa, C), nth0(0,C,c), %si si es una clase
	nth0(2,P,Pr), nth0(3,P,Rel), cola(Pr,Props), valor(id_padre,P,Pop), valor(id,P,ID),
	Pop\=c0, %si no es la clase raiz
	crearNuevaListaInicioO(ID,Props,Rel,Pop),
	rb(W1),
	eliminaClase(P,W1,S),
	guardarBD(S).

eliminaPropiedad(X,Y):-
	rb(W),
	quieroClase(Y,W,P),
	crearNuevaListaProps(P,X).

eliminaRelacion(X,Y):-
	rb(W),
	quieroClase(Y,W,P),
	crearNuevaListaRels(P,X).

crearNuevaListaProps(X,Y):-
	Y \= nombre,
	nth0(2,X,Props),
	member(Y=>XX,Props),
	eliminaClase(Y=>XX,Props,P1),
	sus(Props,P1,X,H1),
	rb(W),
	sus(X,H1,W,S),
	guardarBD(S).

crearNuevaListaRels(X,Y):-
	nth0(3,X,Rels),
	member(Y=>XX,Rels),
	eliminaClase(Y=>XX,Rels,P1),
	sus(Rels,P1,X,H1),
	rb(W),
	sus(X,H1,W,S),
	guardarBD(S).

crearNuevaListaInicio(X,M,R,Pop):- rb(W), crearNuevaLista(X,M,R,Pop,W).
crearNuevaLista(X,M,R,Pop,[]).
crearNuevaLista(X,M,R,Pop,[H|T]):-
	(
		valor(id_padre,H,X),
		nth0(2,H,P2), nth0(3,H,R2), nth0(0,H,ID),
		concatenar(M,P2,L1), concatenar(R,R2,L2),
		set(L1,LPs),
		member(nombre=>Wtvr, LPs),
		eliminaClase(nombre=>Wtvr,LPs,LPs2),
		concatenar([nombre=>Wtvr],LPs2,LPs3),
		H1=[ID,id_padre=>Pop,LPs3,L2],
		rb(W),
		sus(H,H1,W,S),
		guardarBD(S),
		crearNuevaLista(X,M,R,Pop,T)
	);(
		nth0(3,H,Rel),
		member(XXX=>X,Rel),
		eliminaClase(XXX=>X,Rel,Rs),
		sus(Rel,Rs,H,H1),
		rb(W),
		sus(H,H1,W,S),
		guardarBD(S),
		crearNuevaLista(X,M,R,Pop,T)
	);crearNuevaLista(X,M,R,Pop,T).

crearNuevaListaInicioO(X,M,R,Pop):- rb(W), crearNuevaListaO(X,M,R,Pop,W).
crearNuevaListaO(X,M,R,Pop,[]).
crearNuevaListaO(X,M,R,Pop,[H|T]):-
	(
		valor(id_padre,H,X),
		nth0(0,H,IdP),
		segundoTermino(IdP,IdPa),
		atom_chars(IdPa, C), 
		nth0(0,C,o),
		rb(W),
		eliminaClase(H,W,S),
		guardarBD(S),
		crearNuevaListaO(X,M,R,Pop,T)
	);(
		valor(id_padre,H,X),
		nth0(2,H,P2),
		nth0(3,H,R2),
		concatenar(M,P2,L1),
		concatenar(R,R2,L2),
		set(L1,LPs),
		member(nombre=>Wtvr, LPs),
		eliminaClase(nombre=>Wtvr,LPs,LPs2),
		concatenar([nombre=>Wtvr],LPs2,LPs3),
		nth0(0,H,ID),
		H1=[ID,id_padre=>Pop,LPs3,L2],
		rb(W),
		sus(H,H1,W,S),
		guardarBD(S),
		crearNuevaListaO(X,M,R,Pop,T)
	);(
		nth0(1,H,PP),
		nth0(3,H,Rel),
		member(XXX=>X,Rel),
		eliminaClase(XXX=>X,Rel,Rs),
		nth0(2,H,P2),
		nth0(0,H,ID),
		H1=[ID,PP,P2,Rs],
		rb(W),
		sus(H,H1,W,S),
		guardarBD(S),
		crearNuevaListaO(X,M,R,Pop,T)
	);crearNuevaListaO(X,M,R,Pop,T).