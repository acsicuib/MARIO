%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Policy 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Service instance Si asks to UNDEPLOY itself 
% if there are no requests for S_i
operation(undeploy,Si,self) :- 
    \+ requests(Si,_,_,_).

% Service instance Si asks to MIGRATE to node M
% if there are no requests for Si from clients in its node and 
% all the requests for Si have neighbour M as last hop
operation(migrate,Si,M) :- 
    findall(H,requests(Si,H,_,_),Ms), sort(Ms,[M]), dif(M,self).

% Service instance Si asks to REPLICATE in node M 
% M is the node from which most requests to Si are arriving.
% M is chosen among the node where Si is running and the neighbous of that node.
operation(replicate,Si,M) :- 
    findall((H,R),requests(Si,H,R,_),Requests),
    msort(Requests,OrderedRequests), aggregateRequests(OrderedRequests,AggregatedRequests),
    mostRequestsFrom(AggregatedRequests,(M,_)).

:-dynamic requests/4.

