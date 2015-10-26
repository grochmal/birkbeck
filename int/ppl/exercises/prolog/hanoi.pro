hanoi(N) :- move(N, a, b, c).

move(1, X, Y, _) :-
        write('Move top disk from '), 
        write(X),
        write(' to '),
        write(Y),
        nl.
move(N, A, B, C) :-
        N > 1,
        N1 is N - 1,
        move(N1, A, C, B),
        move(1,  A, B, _),
        move(N1, C, B, A).

