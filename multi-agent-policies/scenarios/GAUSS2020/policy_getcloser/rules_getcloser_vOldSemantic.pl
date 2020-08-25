%priority(["suicide","nop","migrate","replicate"]).



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
replicate(Si,[F1,F2|Fs]) :-
  serviceInstance(Si, S, N),
  service(S, RequiredHW, _, _),
  findall(M, ( route(Si, path([N,M|_]), _, _), node(M, FeaturedHW, _), FeaturedHW >= RequiredHW ), Ms),
  sort(Ms,[F1,F2|Fs]).

%”self-referential” KPI: measuring how many instances are “getting closer”


