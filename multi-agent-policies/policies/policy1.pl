%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Policy 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Service instance Si asks to UNDEPLOY itself
% if there are no requests for S_i.
operation(undeploy,Si,self) :-
    \+ requests(Si,_,_,_).

% Service instance Si asks to MIGRATE to node M (different from self) if
% (1a) it cannot handle the total request rate of incoming requests
%  AND
% (2) all the requests for Si have the same neighbour M as last hop
operation(migrate,Si,M) :-
    findall((H,R,L),requests(Si,H,R,L),Requests),
    serviceInstance(Si, S, _), service(S,_,MaxRequestRate,_),
    sumRequestRates(Requests,TotalRequestRate), TotalRequestRate>MaxRequestRate,      %(1a)
    %
    findall(K,member((K,_,_),Requests),Ms), sort(Ms,[M]), dif(M,self).

% Service instance Si asks to REPLICATE in one node M if
% (1a) it cannot handle the total request rate of incoming requests
%
%   - M is the node from which most requests to Si are arriving
%   - M is chosen among self and the neighbours of self
%   - M can be self only if there is enough HW in the node
operation(replicate,Si,M) :-
    findall((H,R,L),requests(Si,H,R,L),Requests),
    serviceInstance(Si, S, N), service(S,RequiredHW,MaxRequestRate,_),
    sumRequestRates(Requests,TotalRequestRate), TotalRequestRate>MaxRequestRate,      %(1a)
    mostRequestsFrom(Requests, M),
      (
       (dif(M,self)
       ;
       (M=self, node(N,AvailableHW,_),AvailableHW>=RequiredHW))
      ).

