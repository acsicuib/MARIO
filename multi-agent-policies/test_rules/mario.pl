


service(meteo, 1, 200, 50).

node(n1, 3, [n3, n2]).
node(n2, 1, [n1]).
node(n3, 5, [n1, n4, n5]).
node(n4, 3, [n3]).
node(n5, 2, [n3]).


link(n1,n3,40,5). % link(source node, target node, latency, bw) 
link(n1,n2,10,5).

0.5::route(s42, path(n4, n1, [n4, n3, n1]), 45, 100); % route(S, Path, Latency, ReqsNo)
0.4::route(s42, path(n4, n1, [n4, n3, n1]), 45, 10). % route(S, Path, Latency, ReqsNo)

route(s45, path(n4, n1, [n4, n3, n1]), 45, 20).
0.20::route(s42, path(n5, n1, [n5, n3, n1]), 45, 20).
%0.80::route(s42, path(n5, n1, [n5, n3, n1]), 45, 0).


serviceInstance(s42, meteo, n1).
serviceInstance(s45, meteo, n1).
serviceInstance(s43, meteo, n2).

query(nop(s42)).
query(nop(s43)).
query(migrate(s42,X,1)).
query(replicate(s42,X)).
query(suicide(s42)).
query(suicide(s43)).
query(fusion(X,Y)).

%writenl(priority([nop,migrate,replicate,suicide])).

