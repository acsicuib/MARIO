
           :- discontiguous route/4.
           route(xxxxxx, path(xxxx, xxx, []), 10, 10).
           desiredUser(20,2).
        nop(Si) :- serviceInstance(Si, S, N),
           service(S, _, _, _),
           node(N, _, _),
           findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),
		   forall(member(route(_,path(_,_,P),_,_),Routes), (length(P,L),L==0)).

suicide(Si) :- serviceInstance(Si, _, _),
                \+ route(Si, _, _, _).

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


        
service(1,1,200,50).
service(2,1,200,50).
serviceInstance(20,1,4).
node(4,2,[2, 7, 8, 9]).
node(2,1,[1, 3, 4]).
node(7,2,[4]).
node(8,1,[4]).
node(9,2,[4]).
link(2,4,4,1).
link(4,7,6,1).
link(4,8,6,1).
link(4,9,6,1).
