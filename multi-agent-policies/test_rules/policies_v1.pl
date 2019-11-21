:- use_module(library(lists)).

0.5::nop(Si) :- serviceInstance(Si, S, N),
           service(S, HwReqs, MaxReqs, MaxLatency),
           node(N, Hw, Neighbours),
           findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),
           checkLatencies(Routes, MaxLatency, SumReqs),
           SumReqs =< MaxReqs.

0.2::migrate(Si,TargetNode):-   serviceInstance(Si, S, N),
				service(S, HwReqs, MaxReqs, MaxLatency),
                node(N, Hw, Neighbours),
                node(TargetNode,HwTarget,_),
                member(TargetNode,Neighbours).
                %findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),

0.3::replicate(Si,TargetNodes):- serviceInstance(Si, S, N),
				service(S, HwReqs, MaxReqs, MaxLatency),
                node(N, Hw, Neighbours),
                selectTargets(Neighbours,TargetNodes).

0.9::suicide(Si) :- serviceInstance(Si, S, N),
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

priority([nop,migrate,replicate,suicide]).

service(meteo, 1, 200, 50).

node(n1, 3, [n3, n2]).
node(n2, 1, [n1]).
node(n3, 5, [n1, n4, n5]).
node(n4, 3, [n3]).
node(n5, 2, [n3]).

0.5::route(s42, path(n4, n1, [n4, n3, n1]), 45, 100); % route(S, Path, Latency, ReqsNo)
0.4::route(s42, path(n4, n1, [n4, n3, n1]), 45, 10). % route(S, Path, Latency, ReqsNo)

0.20::route(s42, path(n5, n1, [n5, n3, n1]), 45, 20).
%0.80::route(s42, path(n5, n1, [n5, n3, n1]), 45, 0).


serviceInstance(s42, meteo, n1).
serviceInstance(s43, meteo, n2).

query(nop(s42)).
query(nop(s43)).
query(migrate(s42,X)).
query(replicate(s42,X)).
query(suicide(s42)).
query(suicide(s43)).
%writenl(priority([nop,migrate,replicate,suicide])).
