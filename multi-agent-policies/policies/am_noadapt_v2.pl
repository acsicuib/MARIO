:- discontiguous membrane/4.
:-dynamic refused/3.
:-dynamic requests/4.
:- discontiguous operation/4.
:- discontiguous trigger/4.

% Undeploy %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
operation(undeploy,Si,_,Flavour) :-
    serviceInstance(Si,_,(Flavour,_,_),self),
    \+ requests(Si,_,_,_).


% Migrates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
operation(migrate,Si,M,SiFlavour) :-
    trigger(migrate,Si,M,SiFlavour).

trigger(migrate,Si,M,Id_F) :-
    findall(H,requests(Si,[H|_],_,_),Ms), sort(Ms,[M]), dif(M,self),
    serviceInstance(Si, _, (Id_F,_,_), self).
% Replicates and adapts, if needed %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

operation(replicate,Si,M,Level) :-
    trigger(replicate,Si,TotalRR,M),
    serviceInstance(Si,_,(Level,_,_),self).

trigger(replicate,Si,TotalRR, M) :-
    findall((M,R),requests(Si,[M],R,_),Requests),
    sumList(Requests,TotalRR),
    serviceInstance(Si,_,(_,_,MaxRR_F),self),
    D is TotalRR / MaxRR_F, D>1.1,
    mostRequestsFrom(Requests, M).


% LIB %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sumList([],0).
sumList([(_,X)|Xs], N) :- sumList(Xs,SumXs), N is X+SumXs.

sumIntList([],0).
sumIntList([X|Xs], N) :- sumIntList(Xs,SumXs), N is X+SumXs.

mostRequestsFrom(Requests,M) :- requestsFrom(M,Requests,MR),
    \+ (requestsFrom(N,Requests,MR1), dif(M,N), MR1>MR).

requestsFrom(M,Requests,MR) :-
    member((M,_),Requests),
    findall((M,V),member((M,V),Requests),Vs),
    sumList(Vs,MR).
