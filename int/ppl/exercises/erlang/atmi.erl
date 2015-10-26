-module(atmi).
-export([get/2]).

get([], _)               -> [];
get([{Key, Val}|_], Key) -> Val;
get([_|Tail], Key)       -> get(Tail, Key).

