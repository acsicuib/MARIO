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
%migrate(Si,M) :-
%  serviceInstance(Si, S, N),
%  service(S, RequiredHW, _, _),
%  findall(Ms, route(Si, path([N|Ms]), _, _), LL),
%  farthest(LL, RequiredHW, M).

%farthest(LL, RequiredHW, M) :- longestPrefix(LL,P), farthestwithHW(P, RequiredHW, M).

%longestPrefix([P],P).
%longestPrefix([P|LL],P) :- prefixOfAll(P,LL).
%longestPrefix([P|LL],Q) :- append(Q,[_],P), prefixOfAll(Q,LL).

%prefixOfAll(_,[]).
%prefixOfAll(P,[P2|LL]) :- append(P,_,P2), prefixOfAll(P,LL).

%farthestwithHW(P, RequiredHW, M) :- append(_,[M],P), node(M, FeaturedHW, _), FeaturedHW >= RequiredHW.
%farthestwithHW(P, RequiredHW, M) :- append(Ms,[M],P), node(M, FeaturedHW, _), FeaturedHW < RequiredHW, farthestwithHW(Ms, RequiredHW, M).


migrate(Si,M) :-
  serviceInstance(Si, S, N),
  service(S, RequiredHW, _, _),
  findall(Ms, route(Si, path([N|Ms]), _, _), LL),]
  farthest(LL, RequiredHW, M).

farthest(LL, RequiredHW, M) :-
	longestPrefix(LL, P), migrateThreshold(T), chop(P, T, NewP), farthestwithHW(NewP, RequiredHW, M).

migrateThreshold(3).  	%threshold definition

chop([],_,[]).
chop(_, 0, []).
chop([X|P], T, [X|NewP]) :- T>0, T1 is T-1, chop(P, T1, NewP).

longestPrefix([P], P).
longestPrefix([P|LL], P) :- prefixOfAll(P, LL).
longestPrefix([P|LL], Q) :- append(Q, [_], P), prefixOfAll(Q, LL).
longestPrefix([P|LL], X) :- append(Q, [_], P), \+ prefixOfAll(Q, LL), append(R,[_], Q), longestPrefix([R|LL], X).

prefixOfAll(_, []).
prefixOfAll(P, [P2|LL]) :- append(P, _, P2), prefixOfAll(P, LL).

farthestwithHW(P, RequiredHW, M) :-
	append(_,[M],P), node(M, FeaturedHW, _), FeaturedHW >= RequiredHW.
farthestwithHW(P, RequiredHW, M) :-
	append(Ms,[M],P), node(M, FeaturedHW, _), FeaturedHW < RequiredHW, farthestwithHW(Ms, RequiredHW, M).



%%replication of service instance Si on the list of nodes [F1,F2|Fs]
% if Si is a service instance running on node N and there is more than one request route
% then Si is replicated on each first hop of such routes that feature HW required by si
replicate(Si,[F1,F2|Fs]) :-
  serviceInstance(Si, S, N),
  service(S, RequiredHW, _, _),
  findall(M, ( route(Si, path([N,M|_]), _, _), node(M, FeaturedHW, _), FeaturedHW >= RequiredHW ), Ms),
  sort(Ms,[F1,F2|Fs]).

%”self-referential” KPI: measuring how many instances are “getting closer”


