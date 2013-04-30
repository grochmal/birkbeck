
age(homer, 39).
age(marge, 39).
age(bart,  10).
age(lisa,   7).
age(maggie, 1).

% gives a list of all ages from the facts "age"
ages(L) :- findall(B, age(A,B), L).

% average as from "Seven Languages In Seven Weeks" by Bruce Tate
count(0, []).
count(Total, [Head|Tail]) :- count(Cnt, Tail), Total is Cnt + 1.
sum(0, []).
sum(Total, [Head|Tail]) :- sum(Sum, Tail), Total is Sum + Head.
average(Average, List) :- count(Cnt, List), sum(Sum, List), Average is Sum/Cnt.

% all elements at the beginning of the list smaller than Average
% are removed from the resulting list in To
drop_while_smaller([(Num, Name)|Tail], Average, To) :-
	Num < Average,
	drop_while_smaller(Tail, Average, To), !.
drop_while_smaller(List, _, List).

% gives a list of tuples in the form (age, name),
% setof returns the list sorted (in this case by the 1st element)
ages_sorted(L) :- setof((B,A), age(A,B), L).

% given a list of tuples [(a,b)] returns a list of the second element [b]
list_names([], []).
list_names([(Num,Name)|Tail], [Name|NameTail]) :- list_names(Tail,NameTail).

% returns the names of the people with ages above the overall average
above_average(L) :-
	ages(Ages),
	average(Average, Ages),
	ages_sorted(AgesNames),
	drop_while_smaller(AgesNames, Average, Above),
	list_names(Above, L).

