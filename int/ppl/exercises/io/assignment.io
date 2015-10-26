#!/usr/bin/env io

Animal := Object clone do (
    legs := nil
    tail := nil
)
"The original prototype (using :=): " print
Animal print

"cat := Animal clone do (legs = 4; tail = 2)\n" print
cat := Animal clone do (legs = 4; tail = 2)
"Cloned (with :=) to cat: " print
cat print
"cat legs: " print
cat legs print
"\n" print
"cat tail: " print
cat tail print
"\n" print

Animal := Object clone do (
    legs ::= nil
    tail ::= nil
)
"Rebuilding the prototype (using ::=): " print
Animal print

"cat := Animal clone setLegs(4) setTail(2)\n" print
cat := Animal clone setLegs(4) setTail(2)
"Cloned (with :=) to cat: " print
cat print
"cat legs: " print
cat legs print
"\n" print
"cat tail: " print
cat tail print
"\n" print

