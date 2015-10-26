smaller(N1, N2, N1) :- N1 @=< N2.
smaller(N1, N2, N2) :- N1 @>  N2.

small([], []).
small([Head|[]], Head).
small([Head|Tail], S) :-
        Tail \== [],
        small(Tail, STail),
        smaller(Head, STail, S).

