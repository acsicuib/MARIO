priority(["suicide","nop","migrate","replicate"]).

%%suicide of service instance Si
% if Si is a service instance and there are no requests routed towards Si then Si suicides
suicide(Si) :-
  serviceInstance(Si, _, _),
  \+ route(Si, _, _, _).

%%nop for service instance Si
% if Si is a service instance and there are no request routes longer then 1 then Si performs nop
nop(Si) :-
  serviceInstance(Si, _, M),
  foreach(route(Si, path([M|L]), _,_),L==[]).

%  same as “get closer” but migrating service instance Si to the farthest hop common to all request routes for Si
migrate(Si,M) :-
  serviceInstance(Si, S, N),
  service(S, RequiredHW, _, _),
  findall(Ms, route(Si, path([N|Ms]), _, _), LL),
  farthest(LL, RequiredHW, M).

farthest(LL, RequiredHW, M) :- longestPrefix(LL,P), farthestwithHW(P, RequiredHW, M).

longestPrefix([P],P).
longestPrefix([P|LL],P) :- prefixOfAll(P,LL).
longestPrefix([P|LL],Q) :- append(Q,[_],P), prefixOfAll(Q,LL).
prefixOfAll(_,[]).
prefixOfAll(P,[P2|LL]) :- append(P,_,P2), prefixOfAll(P,LL).

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


