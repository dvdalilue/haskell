:- dynamic([genNumbers/2]).

main :-
		open('sdk.txt',read,Stream),
		read_numbers(Stream,Numbers),
		close(Stream),

		numToM(Numbers,Mat),
		genposible(1,1,9,M),
		fun(Mat,M),
		
		
		sudoku(M,9),
		%norepfila(8/3/6,M),
		drawSudoku(M,9,1),
		nl.

read_numbers(Stream,[]) :- at_end_of_stream(Stream),!.
read_numbers(Stream,[X|Y]) :-
		\+ at_end_of_stream(Stream),
		read(Stream,X),
		read_numbers(Stream,Y),
		!.

numToM([],W) :- W = [], !.
numToM([A|B], [X/Y/Z|W]) :-
		X is truncate(A/100),
		Y is truncate(A/10) - (X*10),
		Z is A - (X*100) - (Y*10),
		numToM(B,W),
		!.

genposible(F,_,L,R) :- F > L,	R = [],	!.
genposible(F,C,L,R) :- C > L,	N is F + 1,
											 genposible(N,1,L,R), !.
genposible(F,C,L,[X/Y/_|R]) :-
		X is F,
		Y is C,
		N is C + 1,
		genposible(F,N,L,R),
		!.

fun([],_) :- !.
fun([X/Y/Z|R],[X1/Y1/Z1|M]) :-
		X == X1,
		Y == Y1,
		Z1 is Z,
		fun(R,M),
		!.
fun(R,[_|M]) :- fun(R,M), !.

sudoku([],_) :- !.
sudoku([X/Y/Z|O],N) :-
		genNumbers(N,R),
		sudoku(O,N),
		member(Z,R),
		norepfila(X/Y/Z,O).
		%norepcolumna(X/Y/Z,O).
sudoku([_|B],N) :-
		sudoku(B,N).

genNumbers(0,R) :- R = [], !.
genNumbers(T,[A|R]) :- A is T, N is T - 1, genNumbers(N,R),
											 asserta(genNumbers(N,R)), !.

norepfila(_,[]) :- !.
norepfila(X/_/Z,[X1/_/Z1|_]) :- X == X1, Z == Z1, !, fail.
norepfila(_/Y/Z,[_/Y1/Z1|_]) :- Y == Y1, Z == Z1, !, fail.
norepfila(X/Y/Z,[_|R]) :- norepfila(X/Y/Z,R). 
%Creo que un solo predicado seria mejor.
norepcolumna(_,[]) :- !.
norepcolumna(X/Y/Z,[_/Y1/_|R]) :- Y =\= Y1, !, norepcolumna(X/Y/Z,R). %Poner X =:= X1 da error(excepcion), X == X1 o X is X1
norepcolumna(X/Y/Z,[_/_/Z1|R]) :- Z =\= Z1, !, norepcolumna(X/Y/Z,R).  %Colocar el cut al final, parece no cortar. Revise el trace.

%--------------------------------------------------------
%
%  Dibuja el sudoku. A partir de una lista de [X/Y/Z|Otros]
%
%--------------------------------------------------------

drawSudoku([],N,_) :- make_line(N), !.
drawSudoku(X,N,I) :-
		make_line(N),
		spacer(X,I,Y,N,1),
		J is I + 1,
		drawSudoku(Y,N,J).
		
spacer([],_,R,N,F) :-	
		L is N - F + 1,
		make_square(L),
		nl,
		R = [], !.
spacer([X/Y/Z|W],I,R,N,F) :-
		I < X,
		L is N - F + 1,
		make_square(L),
		nl,	R = [X/Y/Z|W], !.
spacer([_/Y/Z|W],I,R,N,F) :-
		L is Y - F,
		make_square(L),
		 write(' '), write(Z), write(' '),
		O is Y + 1,
		spacer(W,I,R,N,O),
		!.

tablero(Tam,0) :-	make_line(Tam), !.
tablero(Tam,I) :-
		Tam > 0,
		I > 0,
		make_line(Tam),
		make_square(Tam),
		nl,
		N is I - 1,
		tablero(Tam,N),
		!.

make_line(T) :- T =< 0,	write('+\n'),	!.
make_line(T) :-
		write('+---'),
		N is T - 1,
		make_line(N),
		!.

make_square(T) :- T =< 0,	write('|'),	!.
make_square(T) :-
		write('|   '),
		N is T - 1,
		make_square(N),
		!.
