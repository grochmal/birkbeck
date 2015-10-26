-module(errmsg).
-export([maybe/1]).

maybe(success) -> io:format("It's a success (~w)~n", [success]);
maybe({error, Msg}) -> io:format("It's an error :( (~s)~n", [Msg]).

