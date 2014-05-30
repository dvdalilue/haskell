%:- op(500,xfy,[':']).
%:- op(900,fx,':').

ui :-
		write('\nPosee los siguiete comandos:\n\n þ Vuelos       - v\n þ Fechas       - f\n þ Directo      - d\n þ Ida y Vuelta - iv\n þ quit         - q\n\nAgente~® '),
		read_atom(S),
		manager(S),
		!,
		ui.
ui :- !.

manager(S) :-	S \= q, !.

todos(D) :- D == lun, !.
todos(D) :- D == mar, !.
todos(D) :- D == mie, !.
todos(D) :- D == jue, !.
todos(D) :- D == vie, !.
todos(D) :- D == sab, !.
todos(D) :- D == dom, !.

habiles(D) :- D == lun, !.
habiles(D) :- D == mar, !.
habiles(D) :- D == mie, !.
habiles(D) :- D == jue, !.
habiles(D) :- D == vie, !.

ruta(O,DE,DI,R) :-
		horario(O,DE,V),
		vuelos(V,DI,R), !.

vuelos([],_,[]) :- !.
vuelos([X:Y/Z:W/V/D|T],DI,[RH|RT]) :-
		verificarD(DI,D),
		RH = X:Y/Z:W/V, !,
		vuelos(T,DI,RT).

verificarD(D,F) :-
		atom(F), !,
		V =.. [F,D],
		V.
verificarD(D,L) :-
		member(D,L), !.

%:(_,_) :- !.
		

/*
  Hechos!!
*/

test( new_york, chicago, [foo/bar/baz,e/t/u]).

horario( new_york, chicago,
           [  9:40 / 10:50 / nw4733 / todos,
             13:40 / 14:50 / nw4773 / habiles,
             19:40 / 20:50 / nw4833 / [lun,mar,mie,jue,vie,dom] ] ). 
horario( chicago, new_york,
           [  9:10 / 10:00 / nw458 / todos,
             12:20 / 13:10 / aa511 / todos ] ). 

horario( chicago, dallas,
           [  9:40 / 10:50 / aa4732 / todos,
             11:40 / 12:50 / aa4752 / habiles,
             18:40 / 19:50 / aa4822 / [lun,mar,mie,jue,vie] ] ). 

horario( dallas, los_angeles,
           [ 13:20 / 16:20 / nw212 / [lun,mar,mie,vie,dom],
             16:30 / 19:30 / aa473 / [lun,mie,jue,sab] ] ). 

horario( new_york, washington,
           [  9:10 / 11:45 / united614 / todos,
             14:45 / 17:20 / united805 / todos ] ). 

horario( chicago, miami,
           [  8:30 / 11:20 / nw510 / todos,
             11:00 / 13:50 / aa459 / todos ] ). 


horario( los_angeles, san_francisco,
           [ 11:30 / 12:40 / sw322 / [mar,jue] ] ). 
horario( san_francisco, los_angeles,
           [  9:25 / 10:15 / aa621 / todos,
             12:45 / 13:35 / sw623 / todos ] ). 


horario( san_francisco, seattle,
           [ 11:10 / 12:20 / sw211 / [lun,mar,mie,vie,dom],
             20:30 / 21:30 / nw472 / [lun,mie,jue,sab] ] ). 
horario( seattle, san_francisco,
           [ 7:55 / 8:45 / aa620 / todos,
             11:25 / 12:15 / aa666 / habiles ] ).


horario( dallas, san_francisco,
           [ 13:30 / 14:40 / nw323 / [mar,jue] ] ). 


horario( boston, new_york,
           [ 9:00 / 9:40 / aa613 / [lun,mar,mie,jue,vie,sab],
            16:10 / 16:55 / united806 / [lun,mar,mie,jue,vie,dom] ] ). 
