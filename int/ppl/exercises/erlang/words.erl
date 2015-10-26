-module(words).
-export([wcount/1]).

wcount(Arg) -> wcount(wsplit(Arg, [], []), 0).
wcount([], Count)       -> Count;
wcount([_|Rest], Count) -> wcount(Rest, Count + 1).

appfull(Words, "")   -> Words;
appfull(Words, Word) -> [Word | Words].

wsplit([], Words, ThisWord) ->
	appfull(Words, ThisWord);
wsplit([32|String], Words, ThisWord) ->
	wsplit(String, appfull(Words, ThisWord), []);
wsplit([Char|String], Words, ThisWord) ->
	wsplit(String, Words, [Char | ThisWord]).

