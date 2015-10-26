likes(wallace, cheese).
likes(gromit, cheese).
% friend(X, Y) :- likes(X, Z), likes(Y, Z), \+(X = Y).
friend(X, Y) :- \+(X = Y), likes(X, Z), likes(Y, Z).

