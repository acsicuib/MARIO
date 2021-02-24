%% Lib.

sumRequestRates([],0).
sumRequestRates([(_,R,_)|Xs], Tot) :- sumRequestRates(Xs,TotXs), Tot is R+TotXs.

mostRequestsFrom(Requests,M) :-
    msort(Requests,OrderedRequests),
    aggregateRequests(OrderedRequests,AggregatedRequests),
    mostRequestsFrom2(AggregatedRequests,(M,_)).

aggregateRequests([],[]).
aggregateRequests([(H,R,_)|Xs],AggregatedRequests) :- aggregateRequests2((H,R),Xs,AggregatedRequests).

aggregateRequests2((H,R),[(H,R1,_)|Xs],AggregatedRequests) :- NewR is R+R1, aggregateRequests2((H,NewR),Xs,AggregatedRequests).
aggregateRequests2((H,R),[(H1,R1,_)|Xs],[(H,R)|AggregatedRequests]) :- dif(H,H1), aggregateRequests2((H1,R1),Xs,AggregatedRequests).
aggregateRequests2((H,R),[],[(H,R)]).

mostRequestsFrom2([X],X).
mostRequestsFrom2([(Mx,Rx)|Xs],(M,R)) :-
    length(Xs,XsLength), XsLength>0, mostRequestsFrom2(Xs,(MXs,RXs)),
    ( (Rx>RXs, M=Mx, R=Rx); (Rx=<RXs, M=MXs, R=RXs) ).


longestPrefix0fPaths([], []).
longestPrefix0fPaths([P], P).
longestPrefix0fPaths([Path|PathList], LongestPrefix) :-
comparePaths(Path, PathList, LongestPrefix).

comparePaths(Path, [], Path).
comparePaths(Path, [Path2|PathList], LongestPrefix) :-
    longestPrefix(Path, Path2, NewPath),
    comparePaths(NewPath, PathList, LongestPrefix).

longestPrefix([R|Path1],[R|Path2], [R|NewPath]) :-
    longestPrefix(Path1,Path2,NewPath).
longestPrefix([R1|_],[R2|_], []) :-
    dif(R1,R2).
longestPrefix([],[],[]).
