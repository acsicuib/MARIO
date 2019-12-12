from pyswip import Prolog
prolog = Prolog()
prolog.consult("model.pl")
print(list(prolog.query("nop(X)")))
print(list(prolog.query("suicide(X)")))
print(list(prolog.query("replicate(s1,M)")))
print(list(prolog.query("replicate(s2,M)")))
print(list(prolog.query("migrate(X,H)")))
# for soln in prolog.query("nop(s1)"):
#     print(soln)