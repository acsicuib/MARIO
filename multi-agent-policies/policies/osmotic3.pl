:- discontiguous operation/4.
:- discontiguous serviceInstance/4.
:- discontiguous operator/3.

operation(undeploy,Si,self) :-
    \+ requests(Si,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adapts service instance Si to a different flavour when 
% the current request rate is lower/higher than the one   
% the current instance flavour can handle.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


operation(migrate,Si,M,V) :-
    findall((H,R,L),requests(Si,[H|_],R,L),Requests), 
    serviceInstance(Si, S, U, self), service(S,Versions,_), member((U,_,MaxRequestRateU),Versions),
    sumRequestRates(Requests,TotalRequestRate), TotalRequestRate>MaxRequestRateU,      
    %
    findall(K,member((K,_,_),Requests),Ms), sort(Ms,[C]), dif(C,self),
    (
        % the node from which requests are coming
        (node(C,_,_),M=C)
        ; 
        % a neighbour of the node from which requests are coming
        (node(M,_,Neighbours), dif(M,self), member(C,Neighbours))
        ;
        % a node reachable from self with highest degree, and highest free resources
        node(M, HWM, NeighboursM), dif(M,self), length(NeighboursM,DegM),
            \+( 
              (node(P,HWP,NeighboursP), dif(P,M), length(NeighboursP,DegP), ((DegP > DegM);(DegP=DegM,HWP>HWM)))
            )
    ), migrateMembrane(Versions,M,TotalRequestRate,V).

migrateMembrane(Versions,M,RequestRate,V) :-
    node(M, AvailableHW, _),
    member((V,HWV,MaxReqRateV), Versions), MaxReqRateV >= RequestRate, 
    HWV =< AvailableHW,
    % does not exist V2 handling the same amount of requests (or more), and requiring less hardware
    \+ ( member((V2,HWV2,MaxReqRateV2),Versions), dif(V2,V), MaxReqRateV2 >= RequestRate, HWV2 < HWV ).


operation(replicate,Si,M,V) :-
    findall((H,R,L),requests(Si,[H|_],R,L),Requests), 
    serviceInstance(Si, S, U, self), service(S,Versions,_), member((U,_,MaxRequestRateU),Versions),
    sumRequestRates(Requests,TotalRequestRate), TotalRequestRate>MaxRequestRateU,      %(1a)
    mostRequestsFrom(Requests, C),
      (
        % node from which most requests are coming (can be self)
       (node(C,_,_), M=C)
       ;
        % a neighbour of the node - reachable from self - from which most requests are coming
       (node(M,_,Neighbours), dif(M,self), member(C,Neighbours))
       ;
        % a node reachable from self with highest degree, and highest free resources, can also be self
        node(M, HWM, NeighboursM), length(NeighboursM,DegM),
        \+( 
            (node(P,HWP,NeighboursP), dif(P,M), length(NeighboursP,DegP), ((DegP > DegM);(DegP=DegM,HWP>HWM)))
        )
    ), replicateMembrane(Versions,M,TotalRequestRate,V).

replicateMembrane(Versions,M,RequestRate,V) :-
    node(M, AvailableHW, _),
    member((V,HWV,MaxReqRateV), Versions), D1 is RequestRate - MaxReqRateV, D1 >= 0,
    HWV =< AvailableHW,
    % does not exist deployable V2 that handles more requests than V
    \+ ( member((V2,HWV2,MaxReqRateV2),Versions), dif(V2,V), HWV2 =< AvailableHW, D2 is RequestRate - MaxReqRateV2, D2 >= 0, D2 < D1 ).

