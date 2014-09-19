:- initialization(main).
:- op(15, xfx, '=>').
=>(X,Y).

main:-
	consult('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/main.pl').
rb(Y):-
	open_kb('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/bd.txt',KB),
	Y=KB.
guardarBD(Y):-
	save_kb('/home/ixchel/git/UNAM/IA/Proyecto/Manejo_de_archivos/bd.txt',Y).

primertermino(X,Y):- X = W=>Z, Y=W.
segundotermino(X,Y):- X = W=>Z, Y=Z.
valor(X,[],Y):- fail.
valor(X,[H|T],Y):- primertermino(H,Z), X=Z, segundotermino(H,W), Y=W, !.
valor(X,[H|T],Y):- primertermino(H,Z), X\=Z, valor(X,T,Y).
cabeza([H|T],Y):- Y=H.
cola([H|T],Y):- Y=T.
clasesHijas(X,Y):- findall(W,clase(W,X,_,_,_),Y).

clase(animal, top,[vida=>finita, ojos=>2], [], ['estrella de mar', gusano]).
individuo('estrella de mar',[ojos=>0, movimiento=>arrastra]).
individuo(gusano,[ojos=>0, movimiento=>arrastra]).
clase(oviparo, animal, [nace=>huevo],[odia=>viviparos],[hormiga]).
individuo(hormiga, [carga=>mucho, ojos=>100]).
clase(viviparo, animal, [nace=>placenta],[odia=>oviparos],[mosca,delfin]).
individuo(mosca, [movimiento=>vuela]).
individuo(delfin, [movimiento=>nada]).
clase(ave, oviparo, [movimiento=>vuela],[],[phoenix]).
individuo(phoenix,[vida=>infinita]).
clase(pez, oviparo,[movimiento=>nada],[odia=>leon],[]).
clase(mamifero, viviparo,[],[],[leon]).
individuo(leon,[armas=>garras]).
clase(pato,ave,[movimiento=>nada],[odia=>pez],[hugo,paco,luis]).
individuo(hugo,[color=>rojo]).
individuo(paco,[color=>azul]).
individuo(luis,[color=>verde]).
clase(aguila, ave,[arma=>garras],[],['aguila calva']).
individuo('aguila calva', [pelo=>no]).
clase(pinguino, ave, [movimiento=>nada],[come=>pez],[]).
clase(huachinango, pez, [sabor=>delicioso],[],[]).
clase('pez volador', pez, [movimiento=>vuela],[come=>gusano],[flippy]).
individuo(flippy, [fama=>mucha]).

rb(Y):- Y = [clase(animal, top,[vida=>finita, ojos=>2], [], ['estrella de mar', gusano]),
individuo('estrella de mar',[ojos=>0, movimiento=>arrastra]),
individuo(gusano,[ojos=>0, movimiento=>arrastra]),
clase(oviparo, animal, [nace=>huevo],[odia=>viviparos],[hormiga]),
individuo(hormiga, [carga=>mucho, ojos=>100]),
clase(viviparo, animal, [nace=>placenta],[odia=>oviparos],[mosca,delfin]),
individuo(mosca, [movimiento=>vuela]),
individuo(delfin, [movimiento=>nada]),
clase(ave, oviparo, [movimiento=>vuela],[],[phoenix]),
individuo(phoenix,[vida=>infinita]),
clase(pez, oviparo,[movimiento=>nada],[odia=>leon],[]),
clase(mamifero, viviparo,[],[],[leon]),
individuo(leon,[armas=>garras]),
clase(pato,ave,[movimiento=>nada],[odia=>pez],[hugo,paco,luis]),
individuo(hugo,[color=>rojo]),
individuo(paco,[color=>azul]),
individuo(luis,[color=>verde]),
calse(aguila, ave,[arma=>garras],[],['aguila calva']),
individuo('aguila calva', [pelo=>no]),
clase(pinguino, ave, [movimiento=>nada],[come=>pez],[]),
clase(huachinango, pez, [sabor=>delicioso],[],[]),
clase('pez volador', pez, [movimiento=>vuela],[come=>gusano],[flippy]),
individuo(flippy, [fama=>mucha])].

clasesHijasRecursivoCabeza(X,[]):-
							findall(W,clase(W,X,_,_,_),[]).
clasesHijasRecursivoCabeza(X,Y):- 
							findall(W,clase(W,X,_,_,_),R),
							cabeza(R,F),
							cola(R,G),
							clasesHijasRecursivoCabeza(F,S),
							clasesHijasRecursivoCola(G,M),
							append(S,M,L),
							append(L,R,Y).
clasesHijasRecursivoCola([],Y):-Y = []. 
clasesHijasRecursivoCola([H|T],Y):- 
							clasesHijasRecursivoCabeza(H,S),
							clasesHijasRecursivoCola(T,M),
							append(S,M,Y).


