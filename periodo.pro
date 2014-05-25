/*
 Pregunta 4 - Calcular la lista de digitos
 que conforman el periodo de la division del
 dividendo y divisor pasados.
*/
periodo(Dividendo,Divisor,L) :- 
		Dividendo >= 0,
		Divisor > 0,
		predicadoQtal(Dividendo,Divisor,L).

predicadoQtal(A,B,R) :-
		N is A/B,
		T is truncate(N),
		D is N - T,
		D > 0,
		!,
		manager(D,B,R).
predicadoQtal(_,_,R) :- R = [], !.

manager(_,A,R) :- 
		primos(A,P),
		%reduce(P,NP), % Elimina los repetidos, pero no importa.
		check(P), % Se verifica si algun numero distinto a 2 o 5
		R = [0], !.
manager(D,_,R) :-	infiniteMix(D,R), !.

%manager(D,_,R) :- 
% 		nextD(D,A,T),
% 		nextD(T,B,_),
% 		A == B,
% 		R = [A],
% 		!.
%manager(D,_,R) :-	declist(D,R,[]), !.
		
primos(N,X) :-
		O is mod(N,2),
		O == 0,
		X = [2|NX],
		P is div(N,2),
		!,
		primos(P,NX).
primos(N,X) :- primosAux(N,3,X), !.

primosAux(N,D,X) :-
		O is mod(N,D),
		O == 0,
		X = [D|NX],
		P is div(N,D),
		!,
		primosAux(P,D,NX).
primosAux(1,_,X) :- X = [], !.
primosAux(N,D,X) :- ND is D + 2, !, primosAux(N,ND,X).

check([])    :- !.
check([H|T]) :- 
		H == 5,
		!,
		check(T).
check([H|T]) :-
		H == 2,
		!,
		check(T).

infiniteMix(N,R) :-
		mix(N,P,[],S),
		dropWhile(P,\=,S,R),
		!.

mix(N,[H|T],L,S) :-
		nextD(N,H,NN),
		\+ member(H,L),
		NL = [H|L],
		!,
		mix(NN,T,NL,S).
mix(N,T,_,S) :- 
		nextD(N,S,_),
		T = [],
		!.

declist(_,T,L) :-
		T = [],
		itIs(L),
		!.
declist(N,[H|T],L) :-
		nextD(N,H,NH),
		NL = [H|L],
		declist(NH,T,NL).

itIs(L) :-
		fold(L,+,0,N),
		X is truncate(N/10),
		Y is X * 10,
		Z is N - Y,
		9 is Z + X, !.

nextD(N,H,T) :-
		NN is N * 10,
		H is truncate(NN),
		T is NN - H.	

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

filter([],_,_,X) :- X = [], !.
filter([A|TA],FN,E,[X|TX]) :-
		F =.. [FN,E,A],
		F,
		X = A,
		!,
		filter(TA,FN,E,TX).
filter([_|TA],FN,E,X) :-
		!,
		filter(TA,FN,E,X).

reduce([],F) :- F =[], !.
reduce([A|TA],[F|TF]) :-
		filter(TA,\=,A,NT),
		F = A,
		!,
		reduce(NT,TF).

dropWhile([],_,_,SL) :- SL = [], !.
dropWhile([L|T],FN,U,SL) :- 
		F =.. [FN,L,U],
		F,
		!,
		dropWhile(T,FN,U,SL).
dropWhile([H|T],_,_,SL) :-
		cp(T,C),
		SL = [H|C],
		!.

cp([],T)    :- T = [], !.
cp([H|T],[S|ST]) :-
		S = H,
		!,
		cp(T,ST).
