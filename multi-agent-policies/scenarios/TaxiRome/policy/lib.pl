%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% libray of auxiliary predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sumAll([],0).
sumAll([(_,R)|L], Tot) :- sumAll(L,TotL), Tot is R+TotL.

aggregateRequests([],[]).
aggregateRequests([(H,R)|L],AggregatedRequests) :-
    aggregateRequests2((H,R),L,AggregatedRequests).
aggregateRequests2((H,R),[(H,R1)|L],AggregatedRequests) :-
    NewR is R+R1, aggregateRequests2((H,NewR),L,AggregatedRequests).
aggregateRequests2((H,R),[(H1,R1)|L],[(H,R)|AggregatedRequests]) :-
    dif(H,H1), aggregateRequests2((H1,R1),L,AggregatedRequests).
aggregateRequests2((H,R),[],[(H,R)]).

mostRequestsFrom([X],X).
mostRequestsFrom([(Mx,Rx)|L],(M,R)) :- 
    length(L,LLength), LLength>0, mostRequestsFrom(L,(ML,RL)), 
    ( (Rx>RL, M=Mx, R=Rx); (Rx=<RL, M=ML, R=RL) ).
