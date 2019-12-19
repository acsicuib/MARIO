:- use_module(library(lists)).

nop(Si) :- serviceInstance(Si, S, N),
           service(S, _, _, _),
           node(N, _, _),
           findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),
		   forall((member(route(_,path(_,_,P),_,_),Routes)), foo(P,L)).
		   
foo(P,L):- length(P,L),L==0.

suicide(Si) :- serviceInstance(Si, S, _),
               service(S, _, _, _),
               findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),
               length(Routes,L),
               L is 0.

replicate(Si,M):- serviceInstance(Si, S, N),
				service(S, _, _, _),
                node(N, _, _),
    			route(Si, path(_, N, P), _, _),
			    length(P,L),
    			L > 1,
    			PS is L-2,
    			nth0(PS,P,M).

migrate(Si,M):- serviceInstance(Si, S, N), service(S, HwReqs, _, _),
        node(N, _, Neighbours),
        member(M,Neighbours), node(M, HwM, _), HwM >= HwReqs,
        route(Si, path(_, N, P), _, _),
		isPenultimateHop(M,P).

isPenultimateHop(M, [M|Last]):- length(Last, Len), Len is 1.
isPenultimateHop(M, [P|Ps]) :-
            P \== M,
            isPenultimateHop(M, Ps).


service(meteo, 1, 200, 50).
serviceInstance(s1, meteo, n1).
serviceInstance(s2, meteo, n4).

desiredUser(s1,2).
desiredUser(s2,2).

node(n1, 3, [n3, n2, n4]). %node(identifier, HW-capacity,neigh)
node(n2, 1, [n1]).
node(n3, 5, [n1]).
node(n4, 5, [n1]).
node(n5, 5, [n2]).
node(n6, 5, [n2]).

link(n1,n3,40,5). % link(source node, target node, latency, bw)
link(n1,n2,10,5).
link(n1,n4,10,5).
link(n5,n2,10,5).
link(n6,n2,10,5).

route(s1, path(n5, n1, [n5, n2]), 45, 100).
route(s1, path(n3, n1, [n3]), 45, 20).
route(s1, path(n6, n1, [n6, n2]), 45, 20).
route(s2, path(n4, n4, []), 45, 20).
            
            
forall(A, B) :- \+((A, \+B)).

query(nop(S)).