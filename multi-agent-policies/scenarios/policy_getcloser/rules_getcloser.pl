priority(["suicide","nop","migrate","replicate"]).


%suicide of service instance Si: if Si is a service instance and there are no requests currently routed towards Si then Si suicides
suicide(Si) :-
   serviceInstance(Si, _, _),
   \+ route(Si, _, _, _).

%nop for service instance Si: if Si is a service instance and there are no request routes longer then 1 then Si performs nop
nop(Si) :-
  serviceInstance(Si, _, _),
  \+( route(Si, path([_|_]), _,_)).

%migration of service instance Si from node N to node M: if Si is a service instance running on node N and
%node M is the predecessor of N in a request route and M features the HW required by Si then Si is migrated to M
migrate(Si,Ms) :-
   serviceInstance(Si, S, N),
   service(S, RequiredHW, _, _),
   findall(M,
          (route(Si, path([N, M| _]), _, _), node(M, FeaturedHW, _), FeaturedHW >= RequiredHW),
           Ms),
   sort(Ms, [_]).


%replication of service instance Si on node M: if Si is a service instance running on node N and there is more than one request route
%then Si is replicated on each first hop of such routes that feature HW required by si
replicate(Si,MsO) :-
   serviceInstance(Si, S, N),
   service(S, RequiredHW, _, _),
   findall(M,
          (route(Si, path([N, M|_]), _, _), node(M, FeaturedHW, _), FeaturedHW >= RequiredHW),
           Ms),
   length(Ms,Length), Length>1,
   sort(Ms, MsO).



%”self-referential” KPI: measuring how many instances are “getting closer”


