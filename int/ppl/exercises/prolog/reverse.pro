rev([], []).
rev([Head|Tail], R) :-
        rev(Tail, RTail),
        append(RTail, [Head], R).

arev(L, R) :- arev(L, [], R).
arev([], R, R).
arev([Head|Tail], Acc, R) :-
        append([Head], Acc, Acc1),
        arev(Tail, Acc1, R).

