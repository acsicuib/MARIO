
operation(undeploy,Si,self) :-
    \+ requests(Si,_,_,_).

operation(migrate,Si,M) :-
    findall((H,R,L),requests(Si,[H|_],R,L),Requests), 
    serviceInstance(Si, S, NodeS), service(S,RequiredHW,MaxRequestRate,_),
    sumRequestRates(Requests,TotalRequestRate), TotalRequestRate>MaxRequestRate,      %(1a)
    %
    findall(K,member((K,_,_),Requests),Ms), sort(Ms,[M]), dif(M,NodeS),
    node(M,AvailableHW, _),
    (AvailableHW>=RequiredHW;
        (
            node(NodeS, _, V), findNewNode(V,NodeS,RequiredHW)
        )
    ).
    

findNewNode([P|Ps],Y,ReqHW):-
    (node(P, AvailableHWNew, _), 
    dif(P,NodeS),
    AvailableHWNew>=ReqHW);
    findNewNode(Ps,Y,ReqHW).


operation(replicate,Si,M) :-  
    %prendi tutte le richieste al servizio fatte dal nodo H
    findall((H,R,L),requests(Si,[H|_],R,L),Requests), 
    %prendi il servizio
    serviceInstance(Si, S, N), service(S,RequiredHW,MaxRequestRate,_),
    sumRequestRates(Requests,TotalRequestRate), TotalRequestRate>MaxRequestRate,      %(1a)
    mostRequestsFrom(Requests, M),
      (
       (dif(M,N), node(M,MHW,_), MHW >= RequiredHW)
       ;
       (M=N,node(N,AvailableHW,_),AvailableHW>=RequiredHW)
       ;
        (dif(M,N), node(N,AHW,V), findNewNode(V,N,RequiredHW))    
      ).

