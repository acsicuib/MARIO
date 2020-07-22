import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
from matplotlib import cm
# cmap = cm.get_cmap('Spectral')

exps = [
    "scenarios/FOCLASA2020/policy_getcloser/",
    "scenarios/FOCLASA2020/policy_eco_getcloser/",
    "scenarios/FOCLASA2020/policy_turbo_getcloser/",
    "scenarios/FOCLASA2020/policy_getclosers_I_II_III/"
]



for lg,experiment_path in enumerate(exps):
    print("Scenario definition: ", experiment_path)

    fig, ax = plt.subplots()
    df = pd.read_csv(experiment_path+"results/action_stats.txt")
    del df["time"]
    df = df.rename(columns={"none":"inhibited action"})
    df.index += 1
    # df[:7].plot.bar(stacked=True,ax=ax,cmap=cmap)
    df[:7].plot.bar(stacked=True,ax=ax)
    plt.title('Total number of actions by type', fontsize=18)
    plt.xlabel(r"Execution steps", fontsize=18)
    plt.ylabel(r"Number of instances", fontsize=18)

    if lg == 0 or lg ==3:
        plt.legend(loc='upper left', fontsize=13) # policy_getcloser # policy_getclosers_I_II_III
    elif lg == 1:
        plt.legend(loc='lower right', fontsize=16) # policy_eco_getcloser
    else:
        plt.legend(loc='lower right', fontsize=13) #policy_turbo_getcloser


    #ax.grid(which='major', axis='y',linestyle=':',linewidth='0.5')
    ax.yaxis.set_major_locator(MaxNLocator(integer=True))

    ax.tick_params(axis='both', which='major', labelsize=12)
    ax.tick_params(axis='both', which='minor', labelsize=8)

    ax.grid(linestyle='-', axis='y',linewidth='0.5',)
    ax.grid('on', which='minor', axis="y",linewidth='0.5')

    for tick in ax.get_xticklabels():
        tick.set_rotation(0)

    fig.savefig(experiment_path+"results/actions_.pdf", dpi=400)

print("Done")