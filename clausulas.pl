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