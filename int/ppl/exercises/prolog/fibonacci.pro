fib1(1, 1).
fib1(2, 1).
fib1(N, R) :-
        N > 2,
        N1 is N - 1,
        N2 is N - 2,
        fib1(N1, R1),
        fib1(N2, R2),
        R is R1 + R2.

fib2(N, R) :- fib2(N, _, R).
fib2(1, 1, 1).
fib2(2, 1, 1).
fib2(N, LR, R) :-
        N > 2,
        N1 is N - 1,
        fib2(N1, LLR, LR),
        R is LLR + LR.

fib3(N, R) :- fib3(N, 1, 1, R).
fib3(1, _, R, R).
fib3(2, _, R, R).
fib3(N, Tmp, Acc, R) :-
        N > 2,
        N1 is N - 1,
        Tmp1 is Acc,
        Acc1 is Tmp + Acc,
        fib3(N1, Tmp1, Acc1, R).

