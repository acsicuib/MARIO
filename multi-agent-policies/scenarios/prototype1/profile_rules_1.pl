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

priority([nop,replicate,migrate,suicide]).