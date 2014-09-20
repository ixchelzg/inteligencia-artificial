%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% awesome.pl %
% NO ESTÁ TERMINADO% 
% OJO!!!!!
% En Swi-Prolog escriban pwd. para ven en dónde está su directorio base
% Luego escriban cd('Ruta/completa/hasta/la/carpeta/inteligencia-artificial').
% Vuelvan a escribir pwd. para ver que se haya cambiado la Ruta
% De este modo ya pueden poner cosas como consult('awesome.pl') y ahorrarse dar clicks extras
% Es importante que hagan eso, pues este archivo parte de la idea de que ya están en
% la carpeta /inteligencia-artificial y así realiza lo que realiza. Lol
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


carga :-  %con consult se carga la base de conocimientos en la memoria
	see('basePruebaCrearBorrar.pl'), read(X),
	write(X).
	%awesome.

awesome :- 
	write('Esta es la lista que ya se lee: '), write(X), nl,
	write('Escribe elimia. o agrega.'),
	read(Opcion), % read lee desde la consola pero no es necesario para el proyecto
	respuesta(Opcion).

respuesta(elimina) :-
	write('Por favor, dime cual quieres eliminar. '), write(X), nl,
	read(Pais),
	eliminando(Pais).

eliminando(Pais) :-
	base(Pais, Ciudad), nl,
	% retract elimina de la base de conocimientos el Hecho, pero en la memoria
	retract(base(Pais,Ciudad)), write('Pais eliminado'), nl,
	awesome.

eliminando(Pais) :-
	\+ base(Pais,Ciudad),
	write('No conozco ese país.'), nl,
	awesome.

% Si es "stop.", entonces grabar la nueva
% base de conocimientos, con todo lo que hay en la memoria (listing).
respuesta(stop) :- 
	write('Grabando la base de conocimientos...'), nl,
	% tell guarda el archivo con todo lo que hay en listing en ese momento
	tell('basePruebaCrearBorrar.pl'),
	listing(base),
	told, write('Listo.'), nl.

% Si es un pais conocido, entonces regresa la capital
respuesta(Pais) :- 
	% base es una función en el otro archivo
	base(Pais, Ciudad),
	write('La capital de '),
	write(Pais),
	write(' es '),
	write(Ciudad), nl,
	% Repite la función awesome nl,
	awesome.

% Si el país no está en la base de conocimientos, entonces se  pide
% el nombre de la capital y se ingresa como Hecho a la base.
respuesta(Pais) :-  
	\+ base(Pais, Ciudad),
	write('No conozco la capital de ese país.'), nl,
	write('Por favor, dime cual es.'), nl,
	write('Capital? '),
	read(Ciudad),
	write('Gracias.'), nl, nl,
	% assert agrega a la base en este momento, es decir, solo en el listing
	assert(base(Pais, Ciudad)),
	awesome.





