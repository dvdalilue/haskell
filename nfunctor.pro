:- op(500, fx, [is_dead]).

kill(mario,luigi).
kill(bowser,mario).

is_dead(X) :- kill(_,X).
