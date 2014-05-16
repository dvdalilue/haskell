
genMat(0,_,M) :- M = [], !.
genMat(T,M,[A|R]) :-
		genF(M,A),
		N is T - 1,
		genMat(N,M,R),
		!.
		
genF(0,F) :- F = [], !.
genF(T,[A|B]) :-
		A is 0,
		N is T - 1,
		genF(N,B),
		!.

gp(_,_,I,B) :- I > 9, B = [], !.
gp(F,C,I,[X/Y/Z|B]) :-
		X is F,
		Y is C,
		Z is I,
		N is I + 1,
		gp(F,C,N,B).
