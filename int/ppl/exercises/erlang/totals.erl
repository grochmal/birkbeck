-module(totals).
-export([sum/1]).

sum(List) -> [{Item, Quantity * Price} || {Item, Quantity, Price} <- List].

