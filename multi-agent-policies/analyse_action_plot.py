import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator

# experiment_path = "scenarios/FOCLASA2020/policy_getcloser/"
# experiment_path = "scenarios/FOCLASA2020/policy_eco_getcloser/"
experiment_path = "scenarios/FOCLASA2020/policy_turbo_getcloser/"
# experiment_path = "scenarios/FOCLASA2020/policy_getclosers_I_II_III/"

fig, ax = plt.subplots()
df = pd.read_csv(experiment_path+"results/action_stats.txt")
del df["time"]
df = df.rename(columns={"none":"blocked action"})
df.index += 1
df[:7].plot.bar(stacked=True,ax=ax)
plt.title('Total number of actions by type', fontsize=18)
plt.xlabel(r"Execution steps", fontsize=18)
plt.ylabel(r"Number of instances", fontsize=18)
# plt.legend(loc='upper left', fontsize=12)
plt.legend(loc='lower right', fontsize=12)


#ax.grid(which='major', axis='y',linestyle=':',linewidth='0.5')
ax.yaxis.set_major_locator(MaxNLocator(integer=True))

ax.tick_params(axis='both', which='major', labelsize=12)
ax.tick_params(axis='both', which='minor', labelsize=8)

ax.grid(linestyle='-', axis='y',linewidth='0.5',)
ax.grid('on', which='minor', axis="y",linewidth='0.5')

for tick in ax.get_xticklabels():
    tick.set_rotation(0)
    
fig.savefig(experiment_path+"results/action.pdf", dpi=400)