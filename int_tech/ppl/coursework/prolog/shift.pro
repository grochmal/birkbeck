
% auxiliary function for readability
give_last(Dropped, Last, List) :-
	append(Dropped, [Last], List).

% rotational left shift i.e. [1,2,3] -> [2,3,1]
lshift([], []).
lshift([Last|Dropped], Shifted) :-
	give_last(Dropped, Last, Shifted).

% rotational right shift i.e. [1,2,3] -> [3,1,2]
rshift([], []).
rshift(List, [Last|Dropped]) :-
	give_last(Dropped, Last, List).

% optionaly one of the functions could be written calling
% the other one (but not both):
% rshift(L1, L2) :- lshift(L2, L1).
% lshift(L1, L2) :- rshift(L2, L1).

