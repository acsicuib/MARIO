from pyswip import Prolog
prolog = Prolog()
prolog.consult("model2.pl")
nop = list(prolog.query("nop(s1)"))
if len(nop)==0: print("nop is false")
nop = list(prolog.query("nop(s1)"))
if len(nop)==0: print("nop is false")

print(list(prolog.query("suicide(s1)")))

replicate = list(prolog.query("replicate(s1,M)"))
# replicate(1,[3, 2])
print(replicate)
tonodes = set()
for rep in replicate:
    tonodes.add(rep["M"])
if len(tonodes) > 0:
    print('replicate(s%i,%s)' % (1, list(tonodes)))


print(list(prolog.query("migrate(s1,H)")))
# print(list(prolog.query("priority(X)"))) DOESTNOT WORK

# for soln in prolog.query("nop(s1)"):
#     print(soln)