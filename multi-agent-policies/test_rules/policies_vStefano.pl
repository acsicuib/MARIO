:- use_module(library(lists)).

forall(A, B) :- \+(( A, \+B )).

migrate(Si,M):-
        serviceInstance(Si, S, N),
        service(S, HwReqs, MaxReqs, MaxLatency),
        node(N, _, Neighbours),
        member(M,Neighbours),
        node(M, HwM, _),
        HwM >= HwReqs,
        forall(route(Si, path(_, N, P), _, _), isPenultimateHop(M, P)).

isPenultimateHop(M, [M]).
isPenultimateHop(M, [P|Ps]) :-
            P \= M,
            isPenultimateHop(M, Ps).

service(meteo, 1, 200, 50).
serviceInstance(s1, meteo, n1).

node(n1, 3, [n3, n2, n4]). %node(identifier, HW-capacity,neigh)
node(n2, 5, [n1, n5]).
node(n3, 5, [n1]).
node(n4, 5, [n1]).
node(n5, 6, [n2]).

link(n1,n3,40,5). % link(source node, target node, latency, bw)
link(n1,n2,10,5).
link(n1,n4,10,5).
link(n2,n5,10,5).

route(s1, path(n2, n1, []), 45, 100).
route(s1, path(n3, n1, []), 45, 20).
route(s1, path(n5, n1, [n2]), 45, 20).

query(migrate(s1,X)).
query(isPenultimateHop(n1,[n3,n1])).
query(isPenultimateHop(n1,[n2,n1])).
query(isPenultimateHop(n1,[n2,n5])).
query(isPenultimateHop(n5,[n2,n5,n1])).


