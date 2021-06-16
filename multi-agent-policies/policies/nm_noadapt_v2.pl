%:- ['nm_example'].
:- discontiguous n_operation/3.

% accept %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
n_operation(accept,(undeploy,Si,_),_) :-
    operation(undeploy,Si,_).

n_operation(accept,(adapt,Si,NewSiFlavour),_) :-
    operation(adapt,Si,NewSiFlavour),
    serviceInstance(Si,_,(_,HW_F,_),self),
    NewSiFlavour=(_,HW_NewF,_),
    node(self, AvailableHW, _),
    AvailableHW + HW_F >= HW_NewF.

n_operation(accept,(Op,Si,SiFlavour),_) :-
    operation(Op,Si,SiFlavour), (Op=migrate; Op=replicate), 
    SiFlavour=(_,HW_F,_), 
    node(self, AvailableHW, _),
    AvailableHW >= HW_F.

n_operation(reject,(Op,Si,SiFlavour),_) :-
    operation(Op,Si,SiFlavour).

