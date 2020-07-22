action(Si,suicide,_):- suicide(Si).
action(Si,migrate,M):- migrate(Si,M).
action(Si,replicate,M):- replicate(Si,M).
action(_,nop,_).

%%suicide of service instance Si
% if Si is a service instance and there are no requests routed towards Si then Si suicides
suicide(Si) :-
  serviceInstance(Si, _, _),
  \+ route(Si, _, _, _).

%%migration of service instance Si from node N to node M
% if Si is a service instance running on node N and node M is the next hop of N in all request routes for Si
% and M features the HW required by Si then Si is migrated to M
migrate(Si,M) :-
  serviceInstance(Si, S, N),
  service(S, RequiredHW, _, _),
  findall(X, ( route(Si, path([N,X|_]), _, _), node(X, FeaturedHW, _), FeaturedHW >= RequiredHW ), Ms),
  sort(Ms,[M]).

%%replication of service instance Si on the list of nodes [F1,F2|Fs]
% if Si is a service instance running on node N and there is more than one request route
% then Si is replicated on each first hop of such routes that feature HW required by si
%replicate(Si,[F1,F2|Fs]) :-
%   serviceInstance(Si, S, N),
%   service(S, RequiredHW, MaxUserRequests, _),
%   findall((M,UserReqs), ( route(Si,path([N,M|_]),_,UserReqs), node(M,FeatHW,_), FeatHW >= RequiredHW ), Ms),
%   sort(Ms,NewMs),
%   Threshold is MaxUserRequests*0,
%   filter(NewMs,Threshold,[F1,F2|Fs]).

filter([],_,[]).
filter([(N,R)|L],T,NewL) :- filter2(L,T,(N,R),NewL).
%filter2([],T,(N,R),[N]) :- R > T.
%filter2([],T,(_,R),[]) :- R =< T.
%filter2([(N,R1)|L],T,(N,R),NewL) :- NewR is R+R1, filter2(L,T,(N,NewR),NewL).
%filter2([(N1,R1)|L],T,(N,R),[N|NewL]) :- N1 \== N, R>T, filter2(L,T,(N1,R1),NewL).
%filter2([(N1,R1)|L],T,(N,R),NewL) :- N1 \== N, R=<T, filter2(L,T,(N1,R1),NewL).


replicate(Si,[F1,F2|Fs]) :-
   serviceInstance(Si, S, N),
   service(S, RequiredHW, MaxUserRequests, _),
   findall((M,UserReqs), ( route(Si,path([N,M|_]),_,UserReqs), node(M,FeatHW,_), FeatHW >= RequiredHW ), Ms),
   sort(Ms,NewMs),
   replicateThreshold(MaxUserRequests,Threshold),
   filter(NewMs,Threshold,[F1,F2|Fs]).


replicateThreshold(MaxUserRequests,Threshold) :- Threshold is MaxUserRequests*0.2. 	%threshold definition

filter2([],T,(N,R),[N]) :- R > T.
filter2([],T,(_,R),[]) :- R =< T.
filter2([(N,R1)|L],T,(N,R),NewL) :- NewR is R+R1, filter2(L,T,(N,NewR),NewL).
filter2([(N1,R1)|L],T,(N,R),[N|NewL]) :- N1 \== N, R>T, filter2(L,T,(N1,R1),NewL).
filter2([(N1,R1)|L],T,(N,R),NewL) :- N1 \== N, R=<T, filter2(L,T,(N1,R1),NewL).


%”self-referential” KPI: measuring how many instances are “getting closer”


