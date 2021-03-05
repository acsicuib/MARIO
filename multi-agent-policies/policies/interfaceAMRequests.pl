% AgentRequests.pl

operations(Si,RequestedActions) :-
    findall((Op,Si,M,V),operation(Op,Si,M,V),RequestedActions).

:-dynamic refused/3.
:-dynamic requests/4.
