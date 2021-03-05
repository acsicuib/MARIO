
:- discontiguous n_operation/3.
:- discontiguous requests/4.

% accept %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
n_operation(accept,(undeploy,Si,_),_) :-
    operation(undeploy,Si,_).

n_operation(accept,(adapt,Si,NewSiFlavour),_) :-
    operation(adapt,Si,NewSiFlavour),
    serviceInstance(Si,_,(_,HW_F,_),self),
    NewSiFlavour=(_,HW_NewF,_),
    node(self, AvailableHW, _),
    AvailableHW + HW_F >= HW_NewF.

n_operation(accept,(Op,Si,SiFlavour),_) :-
    operation(Op,Si,SiFlavour), (Op=migrate; Op=replicate), 
    SiFlavour=(_,HW_F,_), 
    node(self, AvailableHW, _),
    AvailableHW >= HW_F.

% shringANDaccept %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
n_operation(shrinkANDaccept,(Op,Si,SiFlavour),(Sj,SjFlavour,NewSjFlavour)) :-
    operation(Op,Si,SiFlavour), (Op=migrate; Op=replicate),
    SiFlavour=(_,HW_F,_),
    node(self, AvailableHW, _),
    NeededHW is HW_F - AvailableHW,            % (HWNeeded>0 if preceding defs of n_operation failed)
    lessProductive(Sj,SjFlavour),
    dif((Si,SiFlavour),(Sj,SjFlavour)),
    lighterFlavour(Sj,SjFlavour,NeededHW,NewSjFlavour).

% evictANDaccept %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
n_operation(evictANDaccept,(Op,Si,SiFlavour),(Sj,SjFlavour,M)) :-
    operation(Op,Si,SiFlavour), (Op=migrate; Op=replicate),
    lessProductive(Sj,SjFlavour),
    dif((Si,SiFlavour),(Sj,SjFlavour)),        % perhaps not needed here
    node(self, _, Neighbours),
    freestNeighbour(Neighbours,M,HW_M),
    SiFlavour=(_,HW_F,_),
    HW_M >= HW_F.

n_operation(reject,(SOp,Si,SiFlavour),_) :-
    operation(SOp,Si,SiFlavour).

% LIB %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sumList([],0).
sumList([X|Xs], N) :- sumList(Xs,SumXs), N is X+SumXs.

lessProductive(Sj,SjFlavour) :-
    productivity(Sj,SjFlavour,Pj),
    \+ (productivity(Sk,_,Pk),Pk<Pj,dif(Sk,Sj)).

productivity(Sj,SjFlavour,Pj) :-
    serviceInstance(Sj,_,SjFlavour,self), 
    SjFlavour=(_,HW_F,_),
    findall(R,requests(Sj,_,R,_),Requests), 
    sumList(Requests,TotalRR), 
    Pj is HW_F / TotalRR.

lighterFlavour(Sj,SjFlavour,NeededHW,(Id_Sh,HW_Sh,MRR_Sh)) :-
    SjFlavour=(_,HW_F,_),
    serviceInstance(Sj, S, _, self), service(S,SVersions,_), 
    member((Id_Sh,HW_Sh,MRR_Sh),SVersions), HW_F-HW_Sh >= NeededHW, 
    \+ (member((_,HW_F3,_), SVersions), HW_F-HW_F3 >= NeededHW, HW_F3>HW_Sh).

freestNeighbour(Neighbours, M, HW_M) :- 
    member(M, Neighbours), node(M, HW_M,_),
    \+ ( member(M2, Neighbours), dif(M2,M), node(M2, HW_M2,_), HW_M2 > HW_M ).