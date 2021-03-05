% NodeManagerRequest.pl

n_operations(RequestedActions) :-
    findall((Action,(Op,Si,M),V),n_operation(Action,(Op,Si,M),V),RequestedActions).