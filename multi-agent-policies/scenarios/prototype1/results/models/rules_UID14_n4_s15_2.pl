
        :- use_module(library(lists)).
           route(xxxxxx, path(xxxx, xxx, []), 10, 10).
        nop(Si) :- serviceInstance(Si, S, N),
           service(S, HwReqs, MaxReqs, MaxLatency),
           node(N, Hw, Neighbours),
           findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),
           checkLatencies(Routes, MaxLatency, SumReqs),
           SumReqs =< MaxReqs.

fusion(Si1,Si2):- serviceInstance(Si1, S, N),
                serviceInstance(Si2, S, N),
                Si1 \== Si2,
                service(S, HwReqs, MaxReqs, MaxLatency),
                node(N, Hw, Neighbours),
                route(Si1,path(_, N1, NodesPath1), PathLatency1,UReqs1),
                route(Si2,path(_, N2, NodesPath2), PathLatency2,UReqs2),
                UReqs1 + UReqs2 < MaxReqs.

migrate(Si,TargetNode,MaxLatency):-
                serviceInstance(Si, S, N),
				service(S, HwReqs, MaxReqs, MaxLatency),
                node(N, Hw, Neighbours),
                node(TargetNode,HwTarget,_),
                member(TargetNode,Neighbours),
                link(N,TargetNode,L,B),
                route(Si,path(_, N, NodesPath), PathLatency,_),
                member(TargetNode,NodesPath),
                L > MaxLatency.

replicate(Si,TargetNodes):- serviceInstance(Si, S, N),
				service(S, HwReqs, MaxReqs, MaxLatency),
                node(N, Hw, Neighbours),
                selectTargets(Neighbours,TargetNodes).

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

selectTargets([],[]).
selectTargets([N|Ns],[N|Ts]):-selectTargets(Ns,Ts).

checkLatencies([],_,0).
checkLatencies([route(Si, P, PathLatency, UReqs)|Ps], MaxLatency, SumReqs) :-
                            PathLatency =< MaxLatency,
                            checkLatencies(Ps, MaxLatency, SumReqsOld),
                            SumReqs is SumReqsOld + UReqs.

priority([replicate,nop,migrate,suicide]).
service(1,1,200,50).
serviceInstance(15,1,4).
node(4,4,[3]).
node(3,6,[1, 4, 0]).
link(3,4,25,1).
route(15,path(4,4,[4]),0,75).

query(nop(15)).
query(migrate(15, X, 4)).
query(replicate(15, X)).
query(suicide(15)).
query(fusion(X, Y)).
query(priority(X)).

        
        