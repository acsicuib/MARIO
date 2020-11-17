%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Policy 12 (=1&2 combined)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Service instance Si asks to UNDEPLOY itself
% if there are no requests for S_i.
operation(undeploy,Si,self) :-
    \+ requests(Si,_,_,_).

% Service instance Si asks to MIGRATE to node M (different from self) if
% (
% (1a) it cannot handle the total request rate of incoming requests
%  OR
% (1b) it does not satisfy the maximum latency constraint for all requests arriving from M
% )
%  AND
% (2) all the requests for Si have the same neighbour M as last hop
operation(migrate,Si,M) :-
    findall((H,R,L),requests(Si,H,R,L),Requests),
    serviceInstance(Si, S, _), service(S,_,MaxRequestRate,MaxLatencyToClient),
    (
     (sumRequestRates(Requests,TotalRequestRate), TotalRequestRate>MaxRequestRate)      %(1a)
     ;
     (member((_,_,Lat),Requests), Lat>MaxLatencyToClient)                               %(1b)
    ),
    %
    findall(K,member((K,_,_),Requests),Ms), sort(Ms,[M]), dif(M,self).                  %(2)

% Service instance Si asks to REPLICATE in one node M if
% (1a) it cannot handle the total request rate of incoming requests
%  OR
% (1b) it does not satisfy the maximum latency constraint for all requests
%
% If (1b):
%   - M is the neighbour of self from which most requests to Si not satisfying the maximum latency constraint are arriving
% If (1a) (and not(1b))
%   - M is the node from which most requests to Si are arriving
%   - M is chosen among self and the neighbours of self
%   - M can be self only if there is enough HW in the node
operation(replicate,Si,M) :-
    findall((H,R,L),requests(Si,H,R,L),Requests),
    serviceInstance(Si, S, N), service(S,RequiredHW,MaxRequestRate,MaxLatencyToClient),
    (
     (sumRequestRates(Requests,TotalRequestRate), TotalRequestRate>MaxRequestRate)      %(1a)
     ;
     (member((_,_,Lat),Requests), Lat>MaxLatencyToClient)                               %(1b)
    ),
    %
    findall((K,Rk,Lk),(member((K,Rk,Lk),Requests),Lk>MaxLatencyToClient),FilteredRequests),
    (
     (FilteredRequests=[], mostRequestsFrom(Requests, M),
      (
       (dif(M,self)
       ;
       (M=self, node(N,AvailableHW,_),AvailableHW>=RequiredHW))
      )
     )
    ;
     (dif(FilteredRequests,[]), mostRequestsFrom(FilteredRequests, M))
    ).
