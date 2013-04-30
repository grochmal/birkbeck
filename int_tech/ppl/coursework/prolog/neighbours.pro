
female(alice).
female(eve).
male(bob).
male(charlie).
male(david).
neighbour(alice, bob).
neighbour(bob, charlie).
neighbour(charlie, david).
neighbour(david, eve).

doctor(Who) :- bob \== Who.  % pun intended
teacher(Who) :- male(Who).
firefighter(Who) :- male(Who).
lawyer(Who) :-
	firefighter(Someone),
	neighbour_sym(Who, Someone).
dentist(Who) :-
	female(Someone),
	neighbour_sym(Who, Someone).

% symmetric version for the fact neighbour, such that if A
% is neighbour of B then B is a neighbour of A as well
neighbour_sym(A, B) :- neighbour(A, B).
neighbour_sym(A, B) :- neighbour(B, A).

% verifies truth of a given solution, i.e. checks
% all predictates against the names given
test(Doc, Tea, Den, Law, Fir) :-
	doctor(Doc),
	teacher(Tea),
	dentist(Den),
	lawyer(Law),
	firefighter(Fir).

% inserts an element between each of the elements of a list
% e.g. insert(1, [2,3], Is) -> [1,2,3]; [2,1,3]; [2,3,1].
insert(Elem, List, [Elem|List]).  % this one catches insert(Elem, [], [Elem]).
insert(Elem, [NotElem|Sublist], [NotElem|List]) :- insert(Elem, Sublist, List).

% naive permutation generator, generates all permutations of a list
% but works only if all the elements of the list are different
naive_perm([], []).
naive_perm([Head|Tail], Perm) :-
	naive_perm(Tail, TailPerm),
	insert(Head, TailPerm, Perm).

% generates all possible solutions,
% uses naive_perm to create all possible permutations
generate(Doc, Tea, Den, Law, Fir) :-
	Profs = [Doc, Tea, Den, Law, Fir],
	bagof(F, female(F), Females),
	bagof(M, male(M), Males),
	append(Females, Males, Names),
	naive_perm(Names, Profs).

% solves the puzzle,
% for each possible solution generated tests if it is viable
solve(Doc, Tea, Den, Law, Fir) :-
	generate(Doc, Tea, Den, Law, Fir),
	test(Doc, Tea, Den, Law, Fir).

