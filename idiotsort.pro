:- dynamic([genNumbers/2]).

/*
  Pregunta 1 - Verificar que un arreglo
  esta ordenado, a traves de todas las
  permutaciones posibles.
*/
idiotsort(A,B) :- 
		permutator(A,C),
		isEq(C,B).

permutator(A,[C|D]) :- 
		member(C,A),
		select(C,A,B),
		permutator(B,D).
permutator([],A)    :- A = [].

isEq([A|B],[C|D]) :-
		C is A,
		isEq(B,D).
isEq([],[]).

/*
  Pregunta 2 - Verificar que una matriz es
  diabolica.
*/

diabolico(A) :- 
		diagonalP(A,16,0,34),
		diagonalS(A,16,3,0,34),
		filas(A,16,3,0).%, columnas(A).

diagonalP([H|T],NN,0,B) :-
		NN > 0,
		genNumbers(16,G),
		member(H,G),
		N is B - H,
		M is NN - 1,
		diagonalP(T,M,4,N).
diagonalP([_|T],NN,I,B) :-
		NN > 0,
		I > 0,
		N is I - 1,
		M is NN - 1,
		diagonalP(T,M,N,B).
diagonalP(T,0,_,B)      :- B is 0, T = [].

/*
  Pudiese poner el X como el de abajo 
  y sacaria la suma de la diagonal pero
  tengo una variable que solo uso al final
*/

diagonalS([_|T],NN,I,B,X) :-
		NN > 0, /*Creo poder quitar esto, si pongo de primero el predicado base*/
		I > 0,
		N is I - 1,
		M is NN - 1,
		diagonalS(T,M,N,B,X).
diagonalS([_|T],1,_,B,X)  :- T = [], X is B.
diagonalS([H|T],NN,0,B,X) :-
		NN > 0,
		genNumbers(16,G),
		member(H,G),
		N is H + B,
		M is NN - 1,
		diagonalS(T,M,2,N,X).

filas([H|T],NN,I,B) :-
		NN > 0,
		I > 0,
		genNumbers(16,G),
		member(H,G),
		P is B + H,
		N is NN - 1,
		M is I - 1,
		filas(T,N,M,P).
filas([H|T],NN,0,B) :-
		NN > 0,
		genNumbers(16,G),
		member(H,G),
		34 is B + H,
		N is NN - 1,
		filas(T,N,3,0).
filas(T,0,_,_) :-
		T = [].

/*
  Generador de una lista de numeros, tantos
  como se le haya indicado.
*/
genNumbers(0,R)     :- R = [], !.
genNumbers(T,[A|R]) :- 
		A is T, N is T - 1,
		!,
		genNumbers(N,R),
		asserta((genNumbers(T,[A|R]) :- !)).

/* 
  Funcion Fold sobre lista
    1. Lista
    2. Functor de aridad 2
    3. Elemento Nuetro del Functor
    4. Variable Libre
*/
fold([H|T],FN,B,X) :-
		F =.. [FN,H,B],
		N is F,
		fold(T,FN,N,X).
fold([],_,B,X)     :- X is B.
