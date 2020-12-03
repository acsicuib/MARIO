
operation(undeploy,Si,self) :-
    \+ requests(Si,_,_,_).

operation(migrate,Si,M) :-
    findall((H,R,L),requests(Si,[H|_],R,L),Requests),
    serviceInstance(Si, S, N), service(S,RequiredHW,MaxRequestRate,_),
    sumRequestRates(Requests,TotalRequestRate), TotalRequestRate>MaxRequestRate,
    %
    findall(K,member((K,_,_),Requests),Ms), sort(Ms,[C]), dif(C,self),
    (
        % the node from which requests are coming
        (node(C,AvailableHW,_),AvailableHW>=RequiredHW,M=C)
        ;
        % a neighbour of the node from which requests are coming
        (node(M,AvailableHW,Neighbours), dif(M,N), member(C,Neighbours), AvailableHW>=RequiredHW)
        ;
        % a node reachable from self with highest degree, and highest free resources
        node(M, HWM, NeighboursM), dif(M,N), HWM>=RequiredHW, length(NeighboursM,DegM),
            \+(
              (node(P,HWP,NeighboursP), dif(P,N), HWP>=RequiredHW, length(NeighboursP,DegP), ((DegP > DegM);(DegP=DegM,HWP>HWM)))
            )
    ).


operation(replicate,Si,M) :-
    findall((H,R,L),requests(Si,[H|_],R,L),Requests), %SF: changed 1/12
    serviceInstance(Si, S, N), service(S,RequiredHW,MaxRequestRate,_),
    sumRequestRates(Requests,TotalRequestRate), TotalRequestRate>MaxRequestRate,      %(1a)
    mostRequestsFrom(Requests, C),
      (
          % node from which most requests are coming
       (dif(C,self), node(C,CHW,_), CHW >= RequiredHW, M=C)
       ;
          % self
       (C=self, node(N,NHw,_), NHw>=RequiredHW)
       ;
        % a neighbour of the node from which requests are coming
       (node(M,AvailableHW,Neighbours), dif(M,N), member(C,Neighbours), AvailableHW>=RequiredHW)
       ;
        % a node reachable from self with highest degree, and highest free resources, can also be self
        node(M, HWM, NeighboursM), HWM>=RequiredHW, length(NeighboursM,DegM),
        \+(
            (node(P,HWP,NeighboursP), dif(P,N), HWP>=RequiredHW, length(NeighboursP,DegP), ((DegP > DegM);(DegP=DegM,HWP>HWM)))
        )
    ).




