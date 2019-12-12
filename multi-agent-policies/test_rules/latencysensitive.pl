:- use_module(library(lists)).

nop(Si) :- serviceInstance(Si, S, N),
           service(S, HwReqs, MaxReqs, MaxLatency),
           node(N, Hw, Neighbours),
           findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),
           checkLatencies(Routes, MaxLatency, SumReqs),
           SumReqs =< MaxReqs.

migrate(Si,M):-   
        serviceInstance(Si, S, N),
        service(S, HwReqs, MaxReqs, MaxLatency),
        node(N, _, Neighbours),
        member(M,Neighbours),
        node(M, HwM, _),
        HwM >= HwReqs,
        forall(route(Si, path(Source, N, P), Lat, Reqs), isPenultimateHop(P, M)).

isPenultimateHop(M, [M]).
isPenultimateHop(M, [P|Ps]) :- 
            P \== M,
            isPenultimateHop(M, Ps).

replicate(Si,TargetNodes):- 
                serviceInstance(Si, S, N),
				service(S, HwReqs, MaxReqs, MaxLatency),
                node(N, Hw, Neighbours),
                findall(route(Si, path(_, N, P), _, _), route(Si, path(_, N, P), _, _), Routes),
                selectTargets(Si, Routes, Neighbours,TargetNodes).

selectTargets(_, _, [],[]).
selectTargets(Si, Routes, [N|Ns], [N|Ts]):-
                    member(route(Si, path(_, _, P), _, _),Routes),
                    isPenultimateHop(N,P)
                    selectTargets(Ns,Ts).

replicate(Si,[N]):- serviceInstance(Si, S, N),
                service(S, HwReqs, MaxReqs, MaxLatency),
                node(N, Hw, Neighbours),
                route(Si,path(_, N, NodesPath), PathLatency,UReqs),
                UReqs > MaxReqs.

suicide(Si) :- serviceInstance(Si, S, N),
               service(S, HwReqs, MaxReqs, MaxLatency),
               findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),
               length(Routes,L),
               L is 0.



checkLatencies([],_,0).
checkLatencies([route(Si, P, PathLatency, UReqs)|Ps], MaxLatency, SumReqs) :-
                            PathLatency =< MaxLatency,
                            checkLatencies(Ps, MaxLatency, SumReqsOld),
                            SumReqs is SumReqsOld + UReqs.

priority([nop,migrate,replicate,suicide]).

service(meteo, 1, 200, 50).

serviceInstance(s42, meteo, n1).
serviceInstance(s45, meteo, n1).
serviceInstance(s43, meteo, n2).



node(n1, 3, [n3, n2]). %node(identifier, HW-capacity,neigh)
node(n2, 1, [n1]).
node(n3, 5, [n1, n4, n5]).
node(n4, 3, [n3]).
node(n5, 2, [n3]).


link(n1,n3,40,5). % link(source node, target node, latency, bw) 
link(n1,n2,10,5).

0.5::route(s42, path(n4, n1, [n4, n3, n1]), 45, 100); % route(S, Path, Latency, ReqsNo)
0.4::route(s42, path(n4, n1, [n4, n3, n1]), 45, 10). % route(S, Path, Latency, ReqsNo)

route(s45, path(n4, n1, [n4, n3, n1]), 45, 20).
0.20::route(s42, path(n5, n1, [n5, n3, n1]), 45, 20).
%0.80::route(s42, path(n5, n1, [n5, n3, n1]), 45, 0).


query(nop(s42)).
query(nop(s43)).
query(migrate(s42,X,1)).
query(replicate(s42,X)).
query(suicide(s42)).
query(suicide(s43)).
query(fusion(X,Y)).
query(priority(X)).

%writenl(priority([nop,migrate,replicate,suicide])).
