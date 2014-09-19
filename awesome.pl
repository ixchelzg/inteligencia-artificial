%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% awesome.pl %
% NO ESTÁ TERMINADO% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


carga :-  %con consult se carga la base de conocimientos en la memoria
	consult('///Users/yunuenvladimirsanchezgarrido/Desktop/Maestria/Materias/Inteligencia Artificial/Proyecto 1/Pruebas/dynamic.pl'),
	awesome.

awesome :- 
	write('Escribe un Paiis? '),
	read(Pais), % read lee desde la consola pero no es necesario para el proyecto
	respuesta(Pais).

respuesta(elimina) :-
	write('Por favor, dime cual quieres eliminar.'), nl,
	read(Pais),
	eliminando(Pais).

eliminando(Pais) :-
	capital_of(Pais, Ciudad), nl,
	% retract elimina de la base de conocimientos el Hecho, pero en la memoria
	retract(capital_of(Pais,Ciudad)), write('Pais eliminado'), nl,
	awesome.

eliminando(Pais) :-
	\+ capital_of(Pais,Ciudad),
	write('No conozco ese país.'), nl,
	awesome.

% Si es "stop.", entonces grabar la nueva
% base de conocimientos, con todo lo que hay en la memoria (listing).
respuesta(stop) :- 
	write('Grabando la base de conocimientos...'), nl,
	% tell guarda el archivo con todo lo que hay en listing en ese momento
	tell('///Users/yunuenvladimirsanchezgarrido/Desktop/Maestria/Materias/Inteligencia Artificial/Proyecto 1/Pruebas/dynamic.pl'),
	listing(capital_of),
	told, write('Listo.'), nl.

% Si es un pais conocido, entonces regresa la capital
respuesta(Pais) :- 
	% capital_of es una función en el otro archivo
	capital_of(Pais, Ciudad),
	write('La capital de '),
	write(Pais),
	write(' es '),
	write(Ciudad), nl,
	% Repite la función awesome nl,
	awesome.

% Si el país no está en la base de conocimientos, entonces se  pide
% el nombre de la capital y se ingresa como Hecho a la base.
respuesta(Pais) :-  
	\+ capital_of(Pais, Ciudad),
	write('No conozco la capital de ese país.'), nl,
	write('Por favor, dime cual es.'), nl,
	write('Capital? '),
	read(Ciudad),
	write('Gracias.'), nl, nl,
	% assert agrega a la base en este momento, es decir, solo en el listing
	assert(capital_of(Pais, Ciudad)),
	awesome.





