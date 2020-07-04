priority(["suicide","replicate","migrate","nop"]).

suicide(Si) :-
   serviceInstance(Si, S, N),
   service(S, _, MaxUserRequests, _),
   findall(Sj, serviceInstance(Sj, S, N), L),
   length(L,NumberOfSj),
   findall(UserRequests, route(Si, _, _, UserRequests), AllUserRequests),
   sumAll(AllUserRequests,TotalUserRequests),
   TotalUserRequests =< (MaxUserRequests * (NumberOfSj-1)).

replicate(Si, Ms):-
   serviceInstance(Si, S, N),
   service(S, RequiredHW, MaxUserRequests, _),
   findall(UserRequests, route(Si, path([N|_]), _, UserRequests), L1),
   sumAll(L1,TotalUserRequests),
   TotalUserRequests > MaxUserRequests,
   findall(M,
          (route(Si, path([N, M| _]), _, _),
           node(M, FeaturedHW, _), FeaturedHW >= RequiredHW),
           LMs),
   sort(LMs,Ms).

migrate(Si, [M]):-
   serviceInstance(Si, S, N),
   service(S, RequiredHW, _, MaxLatency),
   route(Si, path([N,M|_]), PathLatency, _),
   PathLatency > MaxLatency,
   node(M, FeaturedHW, _),
   FeaturedHW >= RequiredHW.

migrate(Si, [M]):-
   serviceInstance(Si, S, N),
   service(S, RequiredHW, _, MaxLatency),
   route(Si, path([N| _]), PathLatency, _),
   PathLatency =< MaxLatency/2,
   link(N, M, LatencyMN, _),
   LatencyMN+PathLatency =< MaxLatency,
   node(M, FeaturedHW, _),
   FeaturedHW >= RequiredHW.

nop(Si):-
   serviceInstance(Si, S, _),
   service(S, _, MaxUserRequests, _),
   findall(UserRequests, route(Si, _, _, UserRequests), L),
   sumAll(L,TotalUserRequests),
   TotalUserRequests =< MaxUserRequests.

sumAll([],0).
sumAll([X|Xs], Tot) :- sumAll(Xs, T), Tot is X+T.


%”self-referential” KPIs:
% number of users that are provided and not provided with the service
% number of user requests that are served and not served by respecting the required latency







