-module(tictactoe).
-export([game/1]).

game([x, x, x,    _, _, _,    _, _, _]) -> x;
game([_, _, _,    x, x, x,    _, _, _]) -> x;
game([_, _, _,    _, _, _,    x, x, x]) -> x;
game([x, _, _,    x, _, _,    x, _, _]) -> x;
game([_, x, _,    _, x, _,    _, x, _]) -> x;
game([_, _, x,    _, _, x,    _, _, x]) -> x;
game([x, _, _,    _, x, _,    _, _, x]) -> x;
game([_, _, x,    _, x, _,    x, _, _]) -> x;
game([o, o, o,    _, _, _,    _, _, _]) -> o;
game([_, _, _,    o, o, o,    _, _, _]) -> o;
game([_, _, _,    _, _, _,    o, o, o]) -> o;
game([o, _, _,    o, _, _,    o, _, _]) -> o;
game([_, o, _,    _, o, _,    _, o, _]) -> o;
game([_, _, o,    _, _, o,    _, _, o]) -> o;
game([o, _, _,    _, o, _,    _, _, o]) -> o;
game([_, _, o,    _, o, _,    o, _, _]) -> o;
game([A, B, C,    D, E, F,    G, H, I]) ->
	Set = fun(Field) -> if x == Field ; o == Field -> true;
			       true                    -> false
			    end
	      end,
	Pred = lists:all(Set, [A, B, C, D, E, F, G, H, I]),
	if Pred -> cat;
	   true -> no_winner
	end.

