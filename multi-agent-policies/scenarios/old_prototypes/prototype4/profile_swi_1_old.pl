
migrate(Si,M):- serviceInstance(Si, S, N), service(S, _, _, _),
        node(N, _, _),
        findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),
		foreach(member(route(_,path(_,_,P1),_,_),Routes),
                (member(route(_,path(_,_,P2),_,_),Routes),match(P1,P2,M))).

match(L1,L2,M) :- length(L1,S1),
    			length(L2,S2),
      		    PS1 is S1-2,
			    PS2 is S2-2,
    			nth0(PS1,L1,M),
    			nth0(PS2,L2,M).

nop(Si):- serviceInstance(Si, S, N), service(S, _, _, _),
          node(N, _, _),
          findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),
          length(Routes,M),
          desiredUser(Si,M).


replicate(Si,L):- serviceInstance(Si, S, N),service(S, _, _, _),
                node(N, _, _),desiredUser(Si,MaxUsers),
                findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),
        		length(Routes,Len),
                (    Len > MaxUsers ->
	    			findall(Dist,(
                              route(Si,path(_,_,L1),_,_),
                              length(L1,Dist)),Distances),
	                max_l(Distances,Max),
                    member(route(_,path(_,_,P1),_,_),Routes),
                    length(P1,Max),
                    Max1 is Max - 2,
                    nth0(Max1,P1,L);

                    findall(Node,(
                              route(Si,path(_,_,L1),_,_),
                              length(L1,S1),
                              PS1 is S1-2,
                              nth0(PS1,L1,Node)),Nodes),
	    		    list_to_set(Nodes, Set),
					length(Routes,Len),
    				length(Set,Len),
    				Len > 1, % TODO pensar hasta aqui
    				desiredUser(Si,M), %it returns the max. number of nodes that it supports
	    			M1 is M-1,
    				take(Set,M1,L)
                ).

max_l([X],X) :- !, true.
max_l([X|Xs], M):- max_l(Xs, M), M >= X.
max_l([X|Xs], X):- max_l(Xs, M), X >  M.

take(Src,N,L) :- findall(E, (nth1(I,Src,E), I =< N), L).

suicide(Si) :- serviceInstance(Si, S, _),
               service(S, _, _, _),
               findall(route(Si, P, PathLatency, UReqs), route(Si, P, PathLatency, UReqs), Routes),
               length(Routes,L),
               L is 0.
    
priority([migrate,nop,replicate,suicide]).