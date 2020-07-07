import pandas as pd
import matplotlib.pyplot as plt

experiment_path = "scenarios/FOCLASA2020/policy_getcloser/"
experiment_path = "scenarios/FOCLASA2020/policy_getcloserII/"
# experiment_path = "scenarios/FOCLASA2020/policy_getcloserIII/"
df = pd.read_csv(experiment_path+"results/action_stats.txt")
del df["time"]

df[:8].plot.bar(stacked=True)
plt.title('Type of actions by instance service')
plt.xlabel(r"Activation")
plt.ylabel(r"Number of instance service")
#plt.legend(loc='lower right')

plt.savefig(experiment_path+"results/action.png", dpi=300)