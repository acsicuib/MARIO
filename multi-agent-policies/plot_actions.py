import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
from collections import defaultdict
import numpy as np


def showActionsbyGroup(ncase,num,groupname,df0,dfmov0):
    df0 = df0.groupby(["time", "action"]).agg({"action": "count"})
    df0.columns = ["freq"]
    df0.reset_index(level=["time", "action"], inplace=True)
    df0 = df0.pivot_table('freq', ['time'], 'action')
    df0 = df0.fillna(0)

    mv = {}
    pos = 0
    for idx, time in enumerate(dfmov0.index):
        while pos < len(df0.index) and time > df0.index[pos]:
            pos += 1
        mv[pos] = dfmov0.taxi.iloc[idx]

    mv[0] = 0

    dfa = df0
    dfa.index = pd.RangeIndex(start=1, stop=len(dfa) + 1, step=1)
    if "none" not in dfa.columns:
        dfa["none"] = np.zeros(len(dfa))
    if "replicate" not in dfa.columns:
        dfa["replicate"] = np.zeros(len(dfa))
    if "migrate" not in dfa.columns:
        dfa["migrate"] = np.zeros(len(dfa))
    if "undeploy" not in dfa.columns:
        dfa["undeploy"] = np.zeros(len(dfa))

    dfa = dfa[["undeploy", "nop", "migrate", "replicate", "none"]]
    dfa = dfa.rename(columns={"none": "inhibited action"})
    color = ['blue', 'orange', 'green', 'red', "black"]

    fig, ax = plt.subplots(figsize=(20, 12))
    dfa.plot.bar(stacked=True, ax=ax, width=0.9, color=color)
    plt.title('Total number of actions (%s Apps)'%groupname, fontsize=38)
    plt.xlabel(r"MARIO Activations", fontsize=30)
    plt.ylabel(r"Number of instances", fontsize=30)
    plt.legend(loc='upper left', fontsize=18, ncol=5, handleheight=2.4, labelspacing=0.05)
    ax.xaxis.set_major_locator(plt.MaxNLocator(10))
    ax.grid(linestyle='-', axis='y', linewidth='0.5', )
    ax.grid('on', which='minor', axis="y", linewidth='0.5')

    xmin, xmax = ax.get_xlim()
    spticks = np.linspace(0, xmax, 10)
    ax.xaxis.set_ticks(spticks)
    ax.set_xticklabels(np.round(spticks).astype(int))

    for tick in ax.get_yticklabels():
        tick.set_fontsize(24)
    for tick in ax.get_xticklabels():
        tick.set_rotation(0)
        tick.set_fontsize(24)
    ax2 = ax.twinx()  # instantiate a second axes that shares the same x-axis
    color = 'gray'
    ax2.set_ylabel('Number of handovers', color=color, fontsize=30)  # we already handled the x-label with ax1
    ax2.plot(list(mv.keys()), list(mv.values()), '-ok', color=color,
             markersize=10, linewidth=4,
             markerfacecolor='white',
             markeredgecolor='gray',
             markeredgewidth=2)
    ax2.tick_params(axis='y', labelcolor=color)
    for tick in ax2.get_yticklabels():
        tick.set_fontsize(24)
    fig.savefig(pathcommon + "actions_%s_G%i.pdf" % (ncase,num), dpi=400)


experiments = [
    # ("squad3", "results_R_CaseA_r2_X/"),
    ("Instructors", "results_R_instructors_r2/"),
    ("squad3Path", "results_R_squad6_path/"),
    ("squad3Neigh", "results_R_squad6_neigh/"),
]

for ncase, pathcommon in experiments:

    actions = pathcommon+"action_stats.txt"
    mov = pathcommon+"movements.csv"
    res = pathcommon+"Results_Rome_0.csv"
    detailedMoves = pathcommon+"moves_stats.txt"


    ## GLOBAL ACTIONS
    dfa = pd.read_csv(actions)

    df = pd.read_csv(mov)
    dm = df.groupby("time").count()

    mv = {}
    pos = 0
    for idx, time in enumerate(dm.index):
        while pos < len(dfa.time) and time > dfa.time.iloc[pos]:
            pos += 1
        mv[pos] = dm.taxi.iloc[idx]

    mv[0] = 0

    del dfa["time"]
    dfa = dfa.rename(columns={"none": "inhibited action"})
    dfa.index += 1
    # df[:7].plot.bar(stacked=True,ax=ax,cmap=cmap)
    color = ['blue', 'orange', 'green', 'red', "black"]

    fig, ax = plt.subplots(figsize=(20, 12))

    dfa.plot.bar(stacked=True, ax=ax, width=0.9, color=color)

    plt.title('Total number of actions', fontsize=38)
    plt.xlabel(r"MARIO activations", fontsize=30)
    plt.ylabel(r"Number of instances", fontsize=30)

    plt.legend(loc='upper left', fontsize=18, ncol=5,handleheight=2.4, labelspacing=0.05)

    # ax.locator_params(nbins=10, axis='x')

    xmin, xmax = ax.get_xlim()
    spticks = np.linspace(0, xmax, 10)
    ax.xaxis.set_ticks(spticks)
    ax.set_xticklabels(np.round(spticks).astype(int))

    ax.grid(linestyle='-', axis='y', linewidth='0.5', )
    ax.grid('on', which='minor', axis="y", linewidth='0.5')

    for tick in ax.get_yticklabels():
        tick.set_fontsize(24)

    for tick in ax.get_xticklabels():
        tick.set_rotation(0)
        tick.set_fontsize(24)

    ax2 = ax.twinx()  # instantiate a second axes that shares the same x-axis

    color = 'gray'
    ax2.set_ylabel('Number of handovers', color=color, fontsize=30)  # we already handled the x-label with ax1
    ax2.plot(list(mv.keys()), list(mv.values()), '-ok', color=color,
             markersize=10, linewidth=4,
             markerfacecolor='white',
             markeredgecolor='gray',
             markeredgewidth=2)

    ax2.tick_params(axis='y', labelcolor=color)
    for tick in ax2.get_yticklabels():
        tick.set_fontsize(24)

    fig.savefig(pathcommon+"actions_%s.pdf"%ncase, dpi=400)

    ### FOR EACH APP

    # =============================================================================
    # Analysing the type of actions FOR EACH APP of each users
    # =============================================================================
    df = pd.read_csv(mov)

    # Getting users
    users_raw = df.groupby("taxi")["DES"].apply(list)  # DES=ID, len(DES)=Number of changes
    usersCode = {}  # key: DES, value: (taxiCode,user_changes)
    usersMovs = {}
    for p in range(len(users_raw)):
        usersCode[users_raw[p][0]] = users_raw.index[p]
        usersMovs[users_raw[p][0]] = len(users_raw[p])

    dfc = pd.read_csv(res)
    # Getting the app of each user
    users_app = {}
    for sample in dfc.iterrows():
        users_app[sample[1]["DES.src"]] = sample[1]["app"]
        if len(users_app) == len(usersCode):
            break

    dfapp = pd.DataFrame([users_app, usersCode, usersMovs]).T
    dfapp.columns = ["app", "code", "movs"]

    ## apps 1,2,3 => Workload senstivie
    ## apps 4,5,6 => Latency sensitive
    mapapps = {1: 0, 2: 0, 3: 0, 4: 1, 5: 1, 6: 1}
    dfapp["type"] = dfapp.app.map(mapapps)
    ### Geting the movements for each group of app
    dfmov = pd.read_csv(mov)  # all movements
    # get mov from users Type0
    codes = dfapp[dfapp.type == 0].code.values
    dfmov0 = dfmov[dfmov.taxi.isin(codes)]
    dfmov0 = dfmov0.groupby("time").count()

    # get mov from users Type1
    codes = dfapp[dfapp.type == 1].code.values
    dfmov1 = dfmov[dfmov.taxi.isin(codes)]
    dfmov1 = dfmov1.groupby("time").count()

    ### Geting the operations for each group of app
    dem = pd.read_csv(detailedMoves)
    df0 = dem.loc[dem.app < 4]
    df1 = dem.loc[dem.app > 3]


    showActionsbyGroup(ncase,1,"WS",df0, dfmov0)
    showActionsbyGroup(ncase,2,"LS", df1, dfmov1)


print("Done")