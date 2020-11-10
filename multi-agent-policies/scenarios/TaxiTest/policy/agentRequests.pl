%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Service instance Si asks MARIO to execute a list of operations
% (determined according to the loaded policy)
operations(Si,RequestedActions) :- 
    \+ lastOutcome((_,Si,_),rejected),
    findall((Op,Si,M),operation(Op,Si,M),RequestedActions).

% Service instance Si asks MARIO to execute a list of operations
% (determined according to the loaded policy), excluding from 
% such list the last operation that was requested and rejected
operations(Si,RequestedActions) :- 
    lastOutcome((Op_r,Si,M_r),rejected),
    findall((Op,Si,M),(operation(Op,Si,M),(dif(Op,Op_r);dif(M_r,M))),RequestedActions).

:-dynamic lastOutcome/2.

