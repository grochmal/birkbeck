factorial(0, 1).
factorial(N, F) :-
        N > 0,
        N1 is N - 1,
        factorial(N1, F1),
        F is N * F1.

afac(N, R) :- afac(N, 1, R).
afac(0, R, R).
afac(N, A, R) :-
        N > 0,
        N1 is N - 1,
        A1 is A * N,
        afac(N1, A1, R).

% reasoning
% fac [a]   fac(num 1)
% fac [0 n] n
% fac [a n] fac(a-1, n*a)

