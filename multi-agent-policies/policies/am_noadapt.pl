:- discontiguous membrane/4.
:-dynamic refused/3.
:-dynamic requests/4.
:- discontiguous operation/4.
:- discontiguous trigger/4.

% Undeploy %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
operation(undeploy,Si,_,Flavour) :-
    serviceInstance(Si,_,(Flavour,_,_),self),
    \+ requests(Si,_,_,_).

% Adapt Si into different flavour when current request rate differs from what current flavour can handle %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%operation(adapt,Si,_,NewSiFlavour) :-
%    trigger(adapt,Si,TotalRR),
%    membrane(adapt,Si,TotalRR,NewSiFlavour).
%
%
%trigger(adapt,Si,TotalRR) :-
%    findall(R,requests(Si,_,R,_),Requests),
%    sumIntList(Requests,TotalRR),
%    serviceInstance(Si,_,(_,_,MaxRR_F),self),
%    D is TotalRR / MaxRR_F, (D>1.1;D<0.9).

%membrane(adapt,Si,_,NewSiFlavour) :-
%    serviceInstance(Si, S, (Id_F,HW_F,_), self), service(S,SVersions,_),
%    node(self, AvailableHW, _),
%    member(NewSiFlavour, SVersions), NewSiFlavour=(Id_F2,HW_F2,MRR_F2), dif(Id_F2,Id_F),
%    HW_F2 =< AvailableHW + HW_F,
%    \+ (member((_,HW_F3,MRR_F3), SVersions),  MRR_F3 > MRR_F2, HW_F3 =< AvailableHW + HW_F).

% Migrates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
operation(migrate,Si,M,SiFlavour) :-
    trigger(migrate,Si,M,SiFlavour).

trigger(migrate,Si,M,Id_F) :-
    findall(H,requests(Si,[H|_],_,_),Ms), sort(Ms,[M]), dif(M,self),
    serviceInstance(Si, _, (Id_F,_,_), self).

%    node(M, _, _).
    %HW_F =< AvailableHW.

% Replicates and adapts, if needed %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
operation(replicate,Si,M,Level) :-
    trigger(replicate,Si,TotalRR,M),
    membrane(replicate,Si,TotalRR,NewSiFlavour),
    NewSiFlavour=(Level,_,_).


trigger(replicate,Si,TotalRR, M) :-
    findall((M,R),requests(Si,[M],R,_),Requests),
    sumList(Requests,TotalRR),
    serviceInstance(Si,_,(_,_,MaxRR_F),self),
    D is TotalRR / MaxRR_F, D>1.1,
    mostRequestsFrom(Requests, M).


membrane(replicate,Si,TotalRR,NewSiFlavour) :-
	serviceInstance(Si, S, (_,_,MaxRR_F), self), service(S,SVersions,_),
	RRdifference is TotalRR- MaxRR_F,
	member(NewSiFlavour, SVersions), NewSiFlavour=(_,HW_F2,_),
	\+ (member((_,HW_F3,MRR_F3), SVersions), MRR_F3 >= RRdifference, HW_F3 < HW_F2).

%	ISAAC modification
%membrane(trigger,Si,TotalRR,NewSiFlavour) :-
%	serviceInstance(Si, S, (_,_,MaxRR_F), self), service(S,SVersions,_),
%	RRdifference is TotalRR- MaxRR_F,
%	member(NewSiFlavour, SVersions), NewSiFlavour=(_,HW_F2,MRR_F2),
%    MRR_F2 =< RRdifference,
%	\+ (member((_,HW_F3,MRR_F3), SVersions), (  MRR_F3 >= RRdifference; HW_F3 > HW_F2)).


% OLD ORIGINAL
%	MRR_F2 >= RRdifference,% HW_F2 =< AvailableHW,
%	\+ (member((_,HW_F3,MRR_F3), SVersions), MRR_F3 >= RRdifference, HW_F3 < HW_F2).



% LIB %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sumList([],0).
sumList([(_,X)|Xs], N) :- sumList(Xs,SumXs), N is X+SumXs.

sumIntList([],0).
sumIntList([X|Xs], N) :- sumIntList(Xs,SumXs), N is X+SumXs.

mostRequestsFrom(Requests,M) :- requestsFrom(M,Requests,MR),
    \+ (requestsFrom(N,Requests,MR1), dif(M,N), MR1>MR).

requestsFrom(M,Requests,MR) :-
    member((M,_),Requests),
    findall((M,V),member((M,V),Requests),Vs),
    sumList(Vs,MR).
