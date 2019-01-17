/* some facts */
is_male(john).
is_male(charles).
is_male(mark).
is_male(tim).

is_female(jenny).
is_female(susan).
is_female(samantha).
is_female(kate).
is_female(becky).
is_female(mary).

/* some relations */
is_parent(jenny, john).
is_parent(charles, john).

is_parent(susan, samantha).
is_parent(mark,samantha).

is_parent(mary, kate).
is_parent(mark,kate).

is_parent(samantha,becky).
is_parent(john,becky).

is_parent(samantha,tim).
is_parent(john,tim).

p_are_married(jenny, charles).
p_are_married(susan, mark).

/* some rules */
is_mother(X, Y) :- is_female(X), is_parent(X, Y).
is_father(X, Y) :- is_male(X), is_parent(X, Y).
is_not_mother(X, Y) :- is_mother(X, Y).
has_same_mother(X, Y) :- is_mother(Z, X), is_mother(Z, Y).
has_same_father(X, Y) :- is_father(Z, X), is_father(Z, Y).
has_diff_mother(X, Y) :- is_mother(Z, X), is_mother(W, Y), Z \= W.
has_diff_father(X, Y) :- is_father(Z, X), is_father(W, Y), Z \= W.
is_full_sibling(X, Y) :- has_same_father(X, Y), has_same_mother(X, Y), X\= Y.
is_half_sibling(X, Y) :- has_diff_mother(X, Y), has_same_father(X, Y) ; has_diff_father(X, Y), has_same_mother(X, Y).
are_married(X, Y) :- p_are_married(X, Y) ; p_are_married(Y, X).
