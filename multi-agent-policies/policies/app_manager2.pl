:- discontiguous operation/4.
:- discontiguous serviceInstance/4.
:- discontiguous operator/3.

operation(undeploy,Si,self,_) :- \+ requests(Si,_,_,_).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Replicates onto the node from which most requests come
% (can be self), adapt if needed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
operation(replicate,Si,M,U) :-
    findall((H,R,L),requests(Si,[H|_],R,L),Requests),
    serviceInstance(Si, S, U, self), service(S,Versions,_), member((U,_,MaxRequestRateU),Versions),
    sumRequestRates(Requests,TotalRequestRate), TotalRequestRate>MaxRequestRateU,      %(1a)
    mostRequestsFrom(Requests, M),
    MissedReqs is TotalRequestRate - MaxRequestRateU, MissedReqs > 0,
    replicateMembrane(Versions,M,MissedReqs,V).

replicateMembrane(Versions,M,RequestRate,V) :-
    node(M, AvailableHW, _),
    member((V,HWV,MaxReqRateV), Versions), D1 is RequestRate - MaxReqRateV,
    HWV =< AvailableHW,
    % does not exist deployable V2 that handles more requests than V
    \+ ( member((V2,HWV2,MaxReqRateV2),Versions), dif(V2,V), HWV2 =< AvailableHW, RequestRate - MaxReqRateV2 =< D1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Migrates to the farthest node in the reverse common
% path from which all requests are coming. Adapts, if needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
operation(migrate,Si,M,U) :-
    findall((Path,R,L),requests(Si,Path,R,L),Requests),
    serviceInstance(Si, S, _, self), service(S,Versions,_), member((U,HWU,_),Versions),
    sumRequestRates(Requests,TotalRequestRate), %TotalRequestRate>MaxRequestRateU,      %(1a)
    findall(Path,member((Path,_,_),Requests),Paths),
    sort(Paths,SinglePaths),
    longestPrefix0fPaths(SinglePaths,Prefix),
    dif(M,self),
    migrateMembrane(Prefix,HWU,TotalRequestRate,M,U).

% finds farthest node in the longest prefix of paths
migrateMembrane(Prefix,HWU,RequestRate,M,U) :-
    append(Ps,[X],Prefix),
    ( ( node(X, AvailableHW, _), AvailableHW >= HWU, M=X );
      migrateMembrane(Ps,HWU,RequestRate,M,U)
    ).
