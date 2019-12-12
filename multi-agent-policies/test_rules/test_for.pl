:- use_module(library(lists)).

action(N,M):-
        node(N),
        node(M),
        forall(path(N,P), last(M,P)).

last(X,[X]).
last(X,[_|Z]) :- last(X,Z).


node(n1).
node(n2).
node(n3).
node(n4).
path(n1, [n2, n1, n3]).
path(n1, [n3, n1, n4]).

query(action(n1,M)).
