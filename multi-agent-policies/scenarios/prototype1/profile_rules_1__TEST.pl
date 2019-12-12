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
% (fig3) The service receives requests and the requests arrives from more than one edge link.
% → Scale by creating instances in each next hop of each user path and keep an instance
%   in the current node.
%  AND
% (fig4) The service receives requests and the paths for the user requests arrives by an unique
% connection and the path to the user request is longer than 1 hop
% →  Scale by creating an instance in the next hop in the user path and keep a instance in the current node.
replicate(Si,M):- serviceInstance(Si, S, N),
				service(S, _, _, _),
                node(N, _, _),
    			route(Si, path(_, N, P), _, _),
			    length(P,L),
    			L > 0,
    			PS is L-1,
    			nth0(PS,P,M).

migrate(Si,M):- serviceInstance(Si, S, N), service(S, HwReqs, _, _),
        node(N, _, Neighbours),
        member(M,Neighbours), node(M, HwM, _), HwM >= HwReqs,
        route(Si, path(_, N, P), _, _),
		isPenultimateHop(M,P).


% migrate(Si,M):-
%        serviceInstance(Si, S, N), service(S, HwReqs, _, _),
%        node(N, _, Neighbours),
%        member(M,Neighbours), node(M, HwM, _), HwM >= HwReqs,
%        forall(route(Si, path(Source, N, P), Lat, Reqs), isPenultimateHop(M,P)).

isPenultimateHop(M, [M|Last]):- length(Last, Len), Len is 1.
isPenultimateHop(M, [P|Ps]) :-
            P \== M,
            isPenultimateHop(M, Ps).

priority([nop,suicide,replicate,migrate]).