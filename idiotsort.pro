/*
  Pregunta 1 - Verificar que un arreglo
  esta ordenado, a traves de todas las
  permutaciones posibles.
*/

idiotsort([],[]).
idiotsort(A,B) :-
		\+ var(B),
		verificador(B),
		permutator(B,A).
idiotsort(A,B) :-
		\+ var(A),
		permutator(A,B),
		verificador(B).

permutator([],[]).
permutator(A,[C|D]) :-
		select(C,A,B),
		permutator(B,D).

verificador([]).
verificador([_]).
verificador([A,B|C]) :-
		A =< B,
		verificador([B|C]).
