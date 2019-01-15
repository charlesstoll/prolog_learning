/* here are simple facts. test them by typing [fact.] */
mary_likes_wine.
it_is_raining.

/* here are simple relational facts. test them by typing [likes(x,y).] */boot
likes(mary,food).
likes(mary,wine).
likes(john,wine).
likes(john,mary).

/* try also try typing [likes(mary,What).] NOTE THE CAPITAL W!! */

/* likes is not a special operator. just a generic relation that we define */
eats(mary,potato).
drinks(mary,juice).
