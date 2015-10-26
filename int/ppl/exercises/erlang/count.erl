-module(count).
-export([count10/0]).

count10() -> count10(0).
count10(10)  -> io:format("Finished!  It's ~w~n", [10]);
count10(Num) -> io:format("Currently it's ~w~n", [Num]), count10(Num + 1).

