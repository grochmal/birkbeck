valid_queen((Row, Col)) :-
        Range = [1, 2, 3, 4, 5, 6, 7, 8],
        member(Row, Range),
        member(Col, Range).

valid_board([]).
valid_board([Head|Tail]) :-
        valid_queen(Head),
        valid_board(Tail).

rows([], []).
rows([(Row, _)|QueensTail], [Row|RowsTail]) :-
        rows(QueensTail, RowsTail).

cols([], []).
cols([(_, Col)|QueensTail], [Col|ColsTail]) :-
        cols(QueensTail, ColsTail).

diagR([], []).
diagR([(Row, Col)|Tail], [Diag|DiagTail]) :-
        Diag is Row + Col,
        diagR(Tail, DiagTail).

diagL([], []).
diagL([(Row, Col)|Tail], [Diag|DiagTail]) :-
        Diag is Row - Col,
        diagL(Tail, DiagTail).

valid([]).
valid([Head|Tail]) :-
        fd_all_different(Head),
        valid(Tail).

eight_queens(List) :-
        length(List, 8),
        valid_board(List),

        rows(List, Rows),
        cols(List, Cols),
        diagR(List, DiagR),
        diagL(List, DiagL),

        valid([Rows, Cols, DiagR, DiagL]).

opt_queens(Board) :-
        Board = [ (1, _), (2, _), (3, _), (4, _)
                , (5, _), (6, _), (7, _), (8, _) ],
        valid_board(Board),

        cols(Board, Cols),
        diagR(Board, DiagR),
        diagL(Board, DiagL),

        valid([Cols, DiagR, DiagL]).

print_board([]).
print_board([Head|Tail]) :-
        write(Head), nl,
        print_board(Tail).

lst_queens(List) :-
        length(List, 8),
        List = [A, B, C, D, E, F, G, H],
        Board = [ (1, A), (2, B), (3, C), (4, D)
                , (5, E), (6, F), (7, G), (8, H) ],
        Index = [ (a, A), (b, B), (c, C), (d, D)
                , (e, E), (f, F), (g, G), (h, H) ],
        valid_board(Board),

        cols(Board, Cols),
        diagR(Board, DiagR),
        diagL(Board, DiagL),

        valid([Cols, DiagR, DiagL]),
        print_board(Index).

% reasoning (x, y)
% 11 12 13 14 15 16 17 18
% 21 22 23 24 25 26 27 28
% 31 32 33 34 35 36 37 38
% 41 42 43 44 45 46 47 48
% 51 52 53 54 55 56 57 58
% 61 62 63 64 65 66 67 68
% 71 72 73 74 75 76 77 78
% 81 82 83 84 85 86 87 88
%
% sums (x + y)
%  2  3  4  5  6  7  8  9
%  3  4  5  6  7  8  9 10
%  4  5  6  7  8  9 10 11
%  5  6  7  8  9 10 11 12
%  6  7  8  9 10 11 12 13
%  7  8  9 10 11 12 13 14
%  8  9 10 11 12 13 14 15
%  9 10 11 12 13 14 15 16
%
% subtr (x - y)
%  0 -1 -2 -3 -4 -5 -6 -7
%  1  0 -1 -2 -3 -4 -5 -6
%  2  1  0 -1 -2 -3 -4 -5
%  3  2  1  0 -1 -2 -3 -4
%  4  3  2  1  0 -1 -2 -3
%  5  4  3  2  1  0 -1 -2
%  6  5  4  3  2  1  0 -1
%  7  6  5  4  3  2  1  0

% sample solution (1, 4) (2, 5) (3, 8) (4, 6) (5, 3) (6, 7) (7, 2) (8, 4)
%  * - - - - - - -
%  - - - - * - - -
%  - - - - - - - *
%  - - - - - * - -
%  - - * - - - - -
%  - - - - - - * -
%  - * - - - - - -
%  - - - * - - - -

