:- discontiguous n_operation/4.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Node Management: Shrink one with less Reqs served x HW unit    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% shrinks one of the service instances Si to version V, choosing 
% Si among the services that allocate more hardware to serve less 
% requests 
n_operation(shrink,Si,_,V) :-
    node(self, AvailableHW, _),
    % SNew asks to replicate or migrate to node self from node M, and hardware of self is not sufficient
    ( operation(replicate,SNew,self,VNew); operation(migrate,SNew,self,VNew) ),
    serviceInstance(SNew, S, _, M), dif(M,self), service(S,SNewVersions,_), member((VNew,HWNew,_),SNewVersions),
    AvailableHW =< HWNew + 0.25,
    % finds all service instances Sj deployed on self
    serviceInstancesWithHW(self,ServicesOnSelf), 
    serviceInstanceWithReqs(ServicesOnSelf, Tmp), 
    % shrinks one of the service with lower rank Si to version V, which allows to accommodate SNew + 0.25 hardware units
    serviceInstancesWithRank(Tmp, RankedServices),
    sort(RankedServices, SRankedServices), % sort by increasing rank
    member((_,_,HWVOld,Si,_,_,Versions), SRankedServices), 
    member((V,HWV,_), Versions), ((AvailableHW + HWVOld - HWV) >= (HWNew + 0.25)). % find suitable version

% force migration of service instance Si with less requests per hardware unit 
% to the neighbour with more free resources, if possible
n_operation(evict,Si,N,_) :-
    node(self, AvailableHW, Neighbours), freestNeighbour(Neighbours,N,HWN),
    %Sk asks to replicate or migrate to node self and hardware is not sufficient
    ( operation(replicate,SNew,self,VNew); operation(migrate,SNew,self,VNew) ),
    serviceInstance(SNew, S, _, M), dif(M,self), service(S,SNewVersions,_), member((VNew,HWNew,_),SNewVersions),
    AvailableHW =< HWNew + 0.25,
    % finds all service instances Sj deployed on self
    serviceInstancesWithHW(self,ServicesOnSelf), 
    serviceInstanceWithReqs(ServicesOnSelf, Tmp), 
    % shrinks one of the service with lower rank Si to version V, which allows to accommodate SNew + 0.25 hardware units
    serviceInstancesWithRank(Tmp, RankedServices),
    sort(RankedServices, SRankedServices), % sort by increasing rank
    member((_,HWV,Si,_,_,_), SRankedServices), HWN > HWV + 0.25, (AvailableHW + HWV) > (HWNew + 0.25).

n_operation(accept,Si,SOp,_) :-
    % always accepts undeploy
    operation(undeploy,Si,self,_) ; 
    % accepts replicate/migrate/adapt if hardware is enough + 0.25 units
    ( operation(SOp,Si,self,V), ( SOp=replicate; SOp=adapt ), node(self, AvailableHW, _), 
      serviceInstance(Si,S,_,_), service(S,Vs,_), member((V,HWV,_),Vs), AvailableHW >= HWV + 0.25 ).

% rejects any operation not handled above
n_operation(reject,_,_,_) :- operation(SOp,_,self,_), \+ ( n_operation(SOp,_,_,_), (SOp=accept; SOp=shrink; SOp=evict) ).



%%%%%%%%%%%%%%%
%%% Library %%%
%%%%%%%%%%%%%%%

% Retrieves all service instances Sj on node N, in tuples like (HWj,Sj,SType,Vj,Versionsj)
% Keeps track of Max and Min hardware allocation
serviceInstancesWithHW(N,ServicesOnN) :-
    findall((HWj,Sj,SType,Vj,Versionsj), deployedService(N,Sj,SType,Vj,HWj,Versionsj), ServicesOnN).

% Checks is service instance Si of type S is deployed onto node N and retrieves 
% the current version V, hardware reqs HWV and all Versions available
deployedService(N,Si,S,V,HWV,Versions) :- 
    serviceInstance(Si, S, V, N), service(S,Versions,_), member((V,HWV,_),Versions).

% Annotates all service instances Sj on node N with TotReqs they receive, 
% in tuples like (ToTReqs,HWj,Sj,S,Vj,Versionsj)
serviceInstanceWithReqs(ServiceInstances, Result) :- 
    serviceInstanceWithReqs2(ServiceInstances, Result).

serviceInstanceWithReqs2([], []).
serviceInstanceWithReqs2([(HWj,Sj,S,Vj,Versionsj)|Ss], [(ToTReqs,HWj,Sj,S,Vj,Versionsj)|Rest]) :-
    findall( (Sj,R,H), requests(Sj,H,R,_), Requests),
    sumRequestRates(Requests, ToTReqs),
    serviceInstanceWithReqs2(Ss, Rest).

sumRequestRates([],0).
sumRequestRates([(_,R,_)|Xs], Tot) :- sumRequestRates(Xs,TotXs), Tot is R+TotXs.

serviceInstancesWithRank([],[]).
serviceInstancesWithRank([(Reqs,HW,Si,S,V,Versions)|Ss],[(Score,Reqs,HW,Si,S,V,Versions)|Rest]) :-
    dif(HW,0), Score is Reqs/HW,
    serviceInstancesWithRank(Ss, Rest).

freestNeighbour(Neighbours, N, HWN) :- 
    member(N, Neighbours), node(N, HWN,_),
    \+ ( member(M, Neighbours), dif(M,N), node(M, HWM,_), HWM > HWN ).
