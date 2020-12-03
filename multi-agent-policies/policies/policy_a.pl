operation(undeploy,Si,self) :-
    \+ requests(Si,_,_,_).

operation(migrate,Si,P) :-
    findall((H,R,L),(requests(Si,[H|_],R,L)),Requests),
    serviceInstance(Si, S, _), service(S,RequiredHW,MaxRequestRate,_),
    sumRequestRates(Requests,TotalRequestRate), TotalRequestRate>MaxRequestRate,      %(1a)
    %
    findall(Ls,(requests(Si,Ls,R,L)),Ms), sort(Ms, SMs), 
    intersect(SMs, NewMs), member(M,NewMs), 
    dif(M,self),
    serviceInstance(Si, _, Start),
    getPath(Start, M, [P|Path]), 
    checkPath([P|Path], RequiredHW).

operation(replicate,Si,M) :-  
    findall((H,R,L),(requests(Si,[H|_],R,L)),Requests),
    serviceInstance(Si, S, N), service(S,RequiredHW,MaxRequestRate,_),
    sumRequestRates(Requests,TotalRequestRate), TotalRequestRate>MaxRequestRate,      %(1a)
    
    findall(H,(requests(Si,Ls,R,L), member(H,Ls)),Ms), sort([self|Ms], SMs), 
    (mostRequestsFrom(Requests, M); member(M,SMs)),
      (
       (dif(M,self), node(M,MHW,_), MHW >= RequiredHW)
       ;
       (M=self,node(N,AvailableHW,_),AvailableHW>=RequiredHW)
      ).



intersect([], []).
intersect([L|Ls], Int) :-
    intersect(Ls, L, Int).

intersect([], Acc, Acc).
intersect([L|Ls], Acc, Int) :-
    intersection(L, Acc, NewAcc),
    intersect(Ls, NewAcc, Int).

checkPath([],_).
checkPath([Node|Ls], RHw) :-
    node(Node, Hw, _),
    Hw >= RHw,
    checkPath(Ls, RHw).

getPath(Start, End, Path) :-
    getPath(Start, End, [], Path).
    %reverse(Path, RPath).

getPath(End, End, Ls, Ls).
getPath(Start, End, Acc, Path) :-
    node(Start, _, NLs),
    member(Node, NLs),
    \+ member(Node, Acc),
    getPath(Node, End, [Node|Acc], Path).

