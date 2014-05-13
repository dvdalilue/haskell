:- dynamic([fib/2]).
:- dynamic([fac/2]). %El 2 es la aridad del predicado. Numero de parametros
:- dynamic([ackermann/3]).

likes(mary,food).
likes(mary,wine).
likes(john,wine).
likes(john,mary).

padre(rafael,david).

fib(0,0).
fib(1,1).
fib(X,Y) :-
	X > 1,
	N is X - 1, fib(N,W),
	M is X - 2, fib(M,Z),
	Y is W + Z,
	asserta(fib(X,Y)),
	!. %cut

fac(0,1).
fac(X,Y) :-
	X > 0,
	N is X - 1, fac(N,M),
	Y is X * M,
	asserta(fac(X,Y)),
	!. %cut

ackermann(M,N,R) :-
	M = 0,
	R is N + 1,
	asserta(ackermann(M,N,R)),
	!.
ackermann(M,N,R) :-
	M > 0,
	N = 0,
	A is M - 1,
	ackermann(A,1,R),
	asserta(ackermann(M,N,R)),
	!.
ackermann(M,N,R) :-
	M > 0,
	N > 0,
	A is M - 1,
	B is N - 1,
	ackermann(M,B,C),
	ackermann(A,C,R),
	asserta(ackermann(M,N,R)),
	!.

meta(P) :-
	P =.. [_|_],
	call(P).