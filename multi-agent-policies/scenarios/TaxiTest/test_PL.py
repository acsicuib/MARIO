from subprocess import Popen, PIPE


path = "results_20201028/models/"
file = "rules_swi_UID4_nn0lt0ln0_s2_0_500.pl"
service_name = 2

cmd = ["swipl", path+file, str(service_name)]
# cmd = ["swipl","%s/%s"%(gwc,model_file),service_name]
p = Popen(cmd, stdout=PIPE, stderr=PIPE)
stdout, stderr = p.communicate()

output = stdout.decode("utf-8")
action = output.split(",")[0]
params = output.split(",")[1:]

print("output: ",output)
print("action: ",action)
print("params: ",params)
