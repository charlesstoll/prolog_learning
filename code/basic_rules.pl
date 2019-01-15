/* some facts */
likes(mary,james).
likes(john,james).
likes(john,mary).
likes(james,john).

is_father(mark,susan).
is_father(tim,mark).
is_mother(jane,susan).
is_mother(betty,jane).
is_mother(janet,mark).

/* make a rule! */
/* you can type in [love_compatible(john,mary).] to test it. */
/* try typing [love_compatible(X,Y).] and press ';' after the return */
love_compatible(X, Y) :- likes(X, Y), likes(Y, X).

/* you can have multiple rules named the same thing. if prolog doesn't match the first, */
/* it will try to match another rule named the same thing. */
is_parent(X, Y) :- is_father(X, Y) ; is_mother(X, Y).

/* you can cascade this stuff too */
is_grandparent(X, Y) :- is_parent(X, Z), is_parent(Z, Y).

