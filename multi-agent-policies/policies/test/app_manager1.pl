:- discontiguous operation/4.
:- discontiguous serviceInstance/4.
:- discontiguous operator/3.

operation(undeploy,Si,self,_) :-   \+ requests(Si,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Migrates and adapts, if needed

operation(migrate,Si,M,U) :-
    findall((H,R,L),requests(Si,[H|_],R,L),Requests),
    serviceInstance(Si, S, U, _), service(S,Versions,_), member((U,_,MaxRequestRateU),Versions),
    sumRequestRates(Requests,TotalRequestRate), dif(TotalRequestRate,MaxRequestRateU),
    findall(K,member((K,_,_),Requests),Ms), sort(Ms,[M]), dif(M,self), % all requests come from M =/= self
    migrateMembrane(Versions,M,TotalRequestRate,V).

migrateMembrane(Versions,M,RequestRate,V) :-
    node(M, AvailableHW, _),
    member((V,HWV,MaxReqRateV), Versions), MaxReqRateV >= RequestRate,
    HWV =< AvailableHW,
    % does not exist V2 handling the same amount of requests (or more), and requiring less hardware
    \+ ( member((V2,HWV2,MaxReqRateV2),Versions), dif(V2,V), MaxReqRateV2 >= RequestRate, HWV2 < HWV ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Replicates and adapts, if needed

operation(replicate,Si,M,U) :-
    findall((H,R,L),requests(Si,[H|_],R,L),Requests),
    serviceInstance(Si, S, U, self), service(S,Versions,_), member((U,_,MaxRequestRateU),Versions),
    sumRequestRates(Requests,TotalRequestRate), TotalRequestRate>MaxRequestRateU,      
    mostRequestsFrom(Requests, M),
    replicateMembrane(Versions,M,TotalRequestRate,V).

replicateMembrane(Versions,M,RequestRate,V) :-
    node(M, AvailableHW, _),
    member((V,HWV,MaxReqRateV), Versions), D1 is RequestRate - MaxReqRateV, D1 >= 0,
    HWV =< AvailableHW,
    % does not exist deployable V2 that handles more requests than V
    \+ ( member((V2,HWV2,MaxReqRateV2),Versions), dif(V2,V), HWV2 =< AvailableHW, D2 is RequestRate - MaxReqRateV2, D2 >= 0, D2 < D1 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adapts service instance Si to a different flavour when

operation(adapt,Si,self,V) :-
    node(self, AvailableHW, _), findall((H,R,L),requests(Si,[H|_],R,L),Requests),
    serviceInstance(Si, S, U, self), service(S,Versions,_), member((U,HWU,MaxRequestRateU),Versions),
    sumRequestRates(Requests,TotalRequestRate), dif(TotalRequestRate,MaxRequestRateU),
    adaptMembrane(Versions,AvailableHW,HWU,TotalRequestRate,V), dif(V,U).

adaptMembrane(Versions,AvailableHW,UsedHW,RequestRate,V) :-
    member((V,HWV,MaxReqRateV), Versions), MaxReqRateV >= RequestRate,
    HWV =< AvailableHW + UsedHW,
    % does not exist V2 handling the same amount of requests (or more), and requiring less hardware
    \+ ( member((V2,HWV2,MaxReqRateV2),Versions), dif(V2,V), MaxReqRateV2 >= RequestRate, HWV2 < HWV ).
