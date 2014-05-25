:- dynamic([genNumbers/3]).
/*
  Pregunta 2 - Verificar que una matriz es
  diabolica.
*/

diabolico(A) :- 
		var(A),
		filas(A,16),
		diagonalP(A,16,0,34),
		diagonalS(A,16,3,0,34),
		diagonalm(A,16,2,0),
		diagonalu(A,16,3,0),
		diagonald(A,16,1,0),
		columnas(A,16).
diabolico(A) :- 
		filasV(A,16,3,0),
		diagonalP(A,16,0,34),
		diagonalS(A,16,3,0,34),
		diagonalm(A,16,2,0),
		diagonalu(A,16,3,0),
		diagonald(A,16,1,0),
		columnas(A,16),
		!.

filas(E,T) :-
		genNumbers(T,0,A),
		genPosible(A,4,T,E).

genPosible(_,0,_,M) :- M = [], !.
genPosible(P,N,T,M) :-
		N > 0,
		genSum34(P,4,0,G,X),
		NN is N - 1,
		genPosible(X,NN,T,R),
		append(G,R,M).

genSum34(A,0,B,R,X) :- R = [], X = A, B is 34, !.
genSum34(A,N,B,[E|R],X) :-
		N > 0,
		member(E,A),
		delete(A,E,NA),
		NN is N - 1,
		NB is B + E,
		genSum34(NA,NN,NB,R,X).

filasV([H|T],NN,I,B) :-
		NN > 0,
		I > 0,
		P is B + H,
		N is NN - 1,
		M is I - 1,
		!,
		filasV(T,N,M,P).
filasV([H|T],NN,0,B) :-
		NN > 0,
		34 is B + H,
		N is NN - 1,
		!,
		filasV(T,N,3,0).
filasV(T,0,_,_) :-
		T = [], !.

diagonalP([H|T],NN,0,B) :-
		NN > 0,
		N is B - H,
		M is NN - 1,
		!,
		diagonalP(T,M,4,N).
diagonalP([_|T],NN,I,B) :-
		NN > 0,
		I > 0,
		N is I - 1,
		M is NN - 1,
		!,
		diagonalP(T,M,N,B).
diagonalP(T,0,_,B)      :- B is 0, T = [], !.

diagonalS([_|T],NN,I,B,X) :-
		NN > 0, /*Creo poder quitar esto, si pongo de primero el predicado base*/
		I > 0,
		N is I - 1,
		M is NN - 1,
		!,
		diagonalS(T,M,N,B,X).
diagonalS([_|T],1,_,B,X)  :- T = [], X is B, !.
diagonalS([H|T],NN,0,B,X) :-
		NN > 0,
		N is H + B,
		M is NN - 1,
		!,
		diagonalS(T,M,2,N,X).

diagonalm([_|T],NN,I,B) :-
		NN > 0,
		I > 0,
		N is I - 1,
		M is NN - 1,
		!,
		diagonalm(T,M,N,B).
diagonalm([H|[NH|T]],9,0,B) :-
		N is H + NH + B,
		!,
		diagonalm(T,7,4,N).
diagonalm([H|T],NN,0,B) :-
		NN > 0,
		N is H + B,
		M is NN - 1,
		!,
		diagonalm(T,M,4,N).
diagonalm([],_,_,B) :-
		B is 34, !.

diagonalu([_|T],NN,I,B) :-
		NN > 0,
		I > 0,
		N is I - 1,
		M is NN - 1,
		!,
		diagonalu(T,M,N,B).
diagonalu([H|[NH|T]],13,0,B) :-
		N is H + NH + B,
		!,
		diagonalu(T,11,4,N).
diagonalu([H|T],NN,0,B) :-
		NN > 0,
		N is H + B,
		M is NN - 1,
		!,
		diagonalu(T,M,4,N).
diagonalu([],_,_,B) :-
		B is 34, !.

diagonald([_|T],NN,I,B) :-
		NN > 0,
		I > 0,
		N is I - 1,
		M is NN - 1,
		!,
		diagonald(T,M,N,B).
diagonald([H|[NH|T]],5,0,B) :-
		N is H + NH + B,
		!,
		diagonald(T,3,4,N).
diagonald([H|T],NN,0,B) :-
		NN > 0,
		N is H + B,
		M is NN - 1,
		!,
		diagonald(T,M,4,N).
diagonald([],_,_,B) :-
		B is 34, !.

columnas(A,T) :- columna(A,T,0,0), columna(A,T,1,0), columna(A,T,2,0), columna(A,T,3,0), !.

columna([_|T],NN,I,B) :-
		NN > 0,
		I > 0,
		N is NN - 1,
		M is I - 1,
		!,
		columna(T,N,M,B).
columna([H|T],NN,0,B) :-
		NN > 0,
		M is B + H,
		N is NN - 1,
		!,
		columna(T,N,3,M).
columna(T,0,_,B) :-
		B is 34,
		T = [],
		!.

/*
  Generador de una lista de numeros, tantos
  como se le haya indicado.
*/
genNumbers(0,_,R)     :- R = [], !.
genNumbers(T,V,[A|R]) :-
		N is T - 1,
		M is V + 1,
		genNumbers(N,M,R),
		A is M,
		asserta((genNumbers(T,V,[A|R]) :- !)).
