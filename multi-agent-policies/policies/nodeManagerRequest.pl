% NodeManagerRequest.pl

n_operations(RequestedActions) :-
    findall((Op,Si,M,V),n_operation(Op,Si,M,V),RequestedActions).