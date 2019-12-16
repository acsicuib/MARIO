
           :- discontiguous route/4.
           route(xxxxxx, path(xxxx, xxx, []), 10, 10).
           desiredUser(1,2).
        nop(Si) :- serviceInstance(Si, S, N),
           service(S, _, _, _),
           node(N, _, _),
           findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),
		   forall(member(route(_,path(_,_,P),_,_),Routes), (length(P,L),L==0)).

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


        
service(1,1,200,50).
service(2,1,200,50).
serviceInstance(1,1,0).
node(0,8,[1]).
node(1,8,[0, 2]).
link(0,1,1,1).
route(1,path(6,0,[6, 3, 2, 1, 0]),13,18).
route(1,path(9,0,[9, 4, 2, 1, 0]),13,18).
