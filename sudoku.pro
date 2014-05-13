main :-
		open('sdk.txt',read,Stream),
		read_numbers(Stream,Numbers),
		close(Stream),
		% write(Numbers),
		numToM(Numbers,Mat),
		% write(Mat),
		drawSudoku(Mat,9,1),
		nocoinciden(1/2/3,Mat),
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

nocoinciden(_,[]).
nocoinciden(X/Y/Z,[X1/_/Z1|R]) :-
		X == X1,
		Z =\= Z1,
		nocoinciden(X/Y/Z,R).
nocoinciden(X/Y/Z,[_/Y1/Z1|R]) :-
		Y == Y1,
		Z =\= Z1,
		nocoinciden(X/Y/Z,R).
