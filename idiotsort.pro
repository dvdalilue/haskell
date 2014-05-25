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
