from problog.program import PrologString
from problog import get_evaluatable
from problog.tasks import sample


modeltext = """
:- use_module(library(lists)).
nop(Si) :- 
							serviceInstance(Si, S, N),
                            service(S, HwReqs, MaxReqs, MaxLatency),
                            node(N, Hw, Neighbours),
                            findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),
                            writenl(Routes),
                            checkLatencies(Routes, MaxLatency, SumReqs),
                            writenl(SumReqs),
                            SumReqs =< MaxReqs.

migrate(Si,TargetNode):-   serviceInstance(Si, S, N),
				service(S, HwReqs, MaxReqs, MaxLatency),
                node(N, Hw, Neighbours),
                node(TargetNode,HwTarget,_),
                member(TargetNode,Neighbours).
                %findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),

replicate(Si,TargetNodes):- serviceInstance(Si, S, N),
				service(S, HwReqs, MaxReqs, MaxLatency),
                node(N, Hw, Neighbours),
                selectTargets(Neighbours,TargetNodes).

suicide(Si) :- serviceInstance(Si, S, N),
               service(S, HwReqs, MaxReqs, MaxLatency),
               findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),
               length(Routes,L),
               L is 0.

selectTargets([],[]).
selectTargets([N|Ns],[N|Ts]):-selectTargets(Ns,Ts). 


checkLatencies([],_,0).
checkLatencies([route(Si, P, PathLatency, UReqs)|Ps], MaxLatency, SumReqs) :-
                            writenl(PathLatency),
                            PathLatency =< MaxLatency,
                            checkLatencies(Ps, MaxLatency, SumReqsOld),
                            SumReqs is SumReqsOld + UReqs.

service(meteo, 1, 200, 50).

node(n1, 3, [n3, n2]).
node(n2, 1, [n1]).
node(n3, 5, [n1, n4, n5]).
node(n4, 3, [n3]).
node(n5, 2, [n3]).

0.5::route(s42, path(n4, n1, [n4, n3, n1]), 45, 100). % route(S, Path, Latency, ReqsNo)
%0.5::route(s42, path(n4, n1, [n4, n3, n1]), 45, 0). % route(S, Path, Latency, ReqsNo)

0.20::route(s42, path(n5, n1, [n5, n3, n1]), 45, 20).
%0.80::route(s42, path(n5, n1, [n5, n3, n1]), 45, 0).


serviceInstance(s42, meteo, n1).

query(nop(s42)).
query(migrate(s42,X)).
query(replicate(s42,X)).
query(suicide(s42)).

"""

# result = get_evaluatable().create_from(PrologString(model)).evaluate()

model = PrologString(modeltext)
result2 = get_evaluatable().create_from(model).evaluate()
print(result2)

result = sample.sample(model, n=3, format='dict')
print(result)

