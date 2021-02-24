% shrink facts

service(app1,[(small,1,10),(medium,2,20),(large,5,50)],10).
service(app2,[(small,1,10),(medium,2,20),(large,3,30)],10).

node(self,4,[n0,n2,n3]).
node(n0,10,[n1]).
node(n2,10,[n1, n3, n4, n5]).
node(n3,10,[n1, n2, n11, n12, n13]).

serviceInstance(s1,app1,large,n0).
serviceInstance(s2,0,large,self).

requests(s1,[n1],40,6).

operation(migrate,s1,self,large).

% evict facts ?