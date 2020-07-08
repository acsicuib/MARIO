import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator

experiment_path = "scenarios/FOCLASA2020/policy_getcloser/"
#experiment_path = "scenarios/FOCLASA2020/policy_getcloserII/"
#experiment_path = "scenarios/FOCLASA2020/policy_getcloserIII/"
fig, ax = plt.subplots()
df = pd.read_csv(experiment_path+"results/action_stats.txt")
del df["time"]
df = df.rename(columns={"none":"blocked action"})
df[:8].plot.bar(stacked=True,ax=ax)
plt.title('Total number of actions by type')
plt.xlabel(r"Execution steps")
plt.ylabel(r"Number of service instances")
plt.legend(loc='lower right')

#ax.grid(which='major', axis='y',linestyle=':',linewidth='0.5')
ax.yaxis.set_major_locator(MaxNLocator(integer=True))
ax.grid(linestyle='-', axis='y',linewidth='0.5',)
ax.grid('on', which='minor', axis="y",linewidth='0.5')

for tick in ax.get_xticklabels():
    tick.set_rotation(0)
    
fig.savefig(experiment_path+"results/action.png", dpi=300)