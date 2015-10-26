qsort1([], []).
qsort1([Pivot|Rest], S) :-
        partition1(Pivot, Left, Right, Rest),
        qsort(Left, Left1),
        qsort(Right, Right1),
        append(Left1, [Pivot | Right1], S).

partition1(_, [], [], []).
partition1(Pivot, Left, Right, [Head|Tail]) :-
        apppart(Pivot, Left1, Right1, Head),
        partition1(Pivot, Left2, Right2, Tail),
        append(Left1, Left2, Left),
        append(Right1, Right2, Right).

apppart(Pivot, [Var], [],    Var) :- Var @=< Pivot.
apppart(Pivot, [],    [Var], Var) :- Var @>  Pivot.

% reasoning
% qsort [p|rest]
%   left  is rest <= p
%   right is rest >  p
%   append(qsort(left), [p], qsort(right))

qsort2([], []).
qsort2([Pivot|Rest], S) :-
        partition2(Pivot, Left, Right, Rest),
        qsort(Left, Left1),
        qsort(Right, Right1),
        append(Left1, [Pivot | Right1], S).

partition2(_, [], [], []).
partition2(Pivot, [Head|Left], Right, [Head|Tail]) :-
        Head @=< Pivot,
        partition2(Pivot, Left, Right, Tail).
partition2(Pivot, Left, [Head|Right], [Head|Tail]) :-
        Head @>  Pivot,
        partition2(Pivot, Left, Right, Tail).

