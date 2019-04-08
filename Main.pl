male(bob).
male(harry).
child(bob,harry).
bird(pengiun).
bird(ostich).
bird(fl).

alpha(not(not(A)), A).

alpha(and(A1, A2), A1, A2).
alpha(not(or(A1, A2)), not(A1), not(A2)).
alpha(not(imp(A1, A2)), A1, not(A2)).
alpha(equ(A1, A2), imp(A1, A2), imp(A2, A1)).

beta(or(A1, A2), A1, A2).
beta(not(and(A1, A2)), not(A1), not(A2)).
beta(imp(A1, A2), not(A1), A2).
beta(not(equ(A1, A2)), not(imp(A1, A2)), not(imp(A2, A1))).

gamma(all(X, A), X, A).
gamma(not(ex(X, A)), X, not(A)).

delta(ex(X, A), X, A).
delta(not(all(X, A)), X, not(A)).


son(X,Y):-
    male(X), child(X,Y).

forAll(X,Y):- terminal(X),terminal(y), connect(x,y).
fly(X, WING):-
    bird(X),
    X \= penguin, 
    X \= ostrich,
    WING \=broken.