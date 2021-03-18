import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import scipy.stats as stats
import json

def normalizeOperations(operations, movements):
    noperations = {}
    pos = 1
    acc = 0
    for p, idx in enumerate(operations.index):
        acc += operations.loc[idx]
        if pos < len(movements):
            if idx > movements.index[pos]:
                noperations[movements.index[pos - 1]] = acc
                pos += 1
                acc = 0
        else:
            pass

    noperations[movements.index[pos - 1]] = acc
    return pd.Series(noperations)


def computeAndPlot(movements, nopers, name):
    print(name)
    r, p = stats.pearsonr(movements, nopers)
    print(f"Scipy computed Pearson r: {r} and p-value: {p}")
    x = range(len(nopers))
    fig, ax = plt.subplots(figsize=(12, 5))
    ax.plot(x, nopers.values, linewidth=4, label="Operations")
    ax.plot(x, movements.values, linewidth=3, label="User Movements")

    if "%f"%r == "nan":
        plt.title("Pearson correlation coefficient r = --", fontsize=28)
    else:
        plt.title("Pearson correlation coefficient r = %0.3f" % r, fontsize=28)
    plt.xlabel(r"MARIO activations", fontsize=20)
    plt.ylabel(r"Frequency", fontsize=20)
    plt.legend(loc='upper right', fontsize=18)  # policy_getcloser # policy_getclosers_I_II_III
    fig.savefig(name, dpi=400)


def run(datestamp):
    with open("experiments.json") as f:
        experiments = json.load(f)

    for item in experiments:
        try:
            ncase = item["code"]
            name = item["scenario"]
            experiment_path = item["path"]
            policy_folder = item["pathPolicy"]
            projection = item["coords"]
            policy_file = item["policyName"]
            pathcommon = experiment_path+"results_%s_%s/"%(ncase,datestamp)


            actions = pathcommon+"action_stats.txt"
            mov = pathcommon+"movements.csv"
            res = pathcommon+"Results_Rome_0.csv"
            detailedMoves = pathcommon+"moves_stats.txt"


            dfmov = pd.read_csv(mov)  # all movements
            # dfmov = dfmov[dfmov.time!=0]
            dfmov = dfmov[dfmov.nodeSRC != dfmov.nodeDST]
            dfmov["count"] = np.ones(len(dfmov))

            # time - moves
            movements = dfmov.groupby("time")["count"].sum()

            dem = pd.read_csv(detailedMoves)

            dem = dem.groupby(["time", "action"]).agg({"action": "count"})
            dem.columns = ["freq"]
            dem.reset_index(level=["time", "action"], inplace=True)
            dem = dem.pivot_table('freq', ['time'], 'action')
            dem = dem.fillna(0)

            dem = dem.drop(["nop"], axis=1)
            if ["none"] in list(dem.columns):
                dem = dem.drop(["none"], axis=1)
            # time - operations
            operations = dem.sum(axis=1)

            # We need two series with the same length
            # We normalize the operation serie aggregating the sum of operations for each to movements.time
            nopers = normalizeOperations(operations, movements)

            ##movements
            ##nopers
            ### Compute the co-relations between both
            computeAndPlot(movements, nopers,pathcommon +"pearson_%s.pdf" % (ncase))

            # =============================================================================
            # BY GROUP APP
            # =============================================================================

            ### GET TAXI -> APP

            dfmov = pd.read_csv(mov)
            users_raw = dfmov.groupby("taxi")["DES"].apply(list)  # DES=ID, len(DES)=Number of changes
            usersCode = {}  # key: DES, value: (taxiCode,user_changes)
            usersMovs = {}
            for p in range(len(users_raw)):
                usersCode[users_raw[p][0]] = users_raw.index[p]
                usersMovs[users_raw[p][0]] = len(users_raw[p])

            dfresults = pd.read_csv(res)
            # Getting the app of each user
            users_app = {}
            for sample in dfresults.iterrows():
                users_app[sample[1]["DES.src"]] = sample[1]["app"]
                if len(users_app) == len(usersCode):
                    break

            dfapp = pd.DataFrame([users_app, usersCode, usersMovs]).T
            dfapp.columns = ["app", "code", "movs"]

            ## 0: apps 1,2,3 => Workload senstivie
            ## 1: apps 4,5,6 => Latency sensitive
            mapapps = {1: 0, 2: 0, 3: 0, 4: 1, 5: 1, 6: 1}
            dfapp["type"] = dfapp.app.map(mapapps)

            ######
            # dfapp = [app, code, movs, type ].columns
            #####

            ### SAME PREVIOUS
            # get mov from users Type0
            codes = dfapp[dfapp.type == 0].code.values
            dfmov0 = dfmov[dfmov.taxi.isin(codes)]
            dfmov0["count"] = np.ones(len(dfmov0))

            ### TIME - MOVES
            # time - moves0
            movements0 = dfmov0.groupby("time")["count"].sum()

            ### OPERATIONS - 0
            dem = pd.read_csv(detailedMoves)
            dem["type"] = dem.app.map(mapapps)

            dem0 = dem[dem.type == 0]

            dem0 = dem0.groupby(["time", "action"]).agg({"action": "count"})
            dem0.columns = ["freq"]
            dem0.reset_index(level=["time", "action"], inplace=True)
            dem0 = dem0.pivot_table('freq', ['time'], 'action')
            dem0 = dem0.fillna(0)

            dem0 = dem0.drop(["nop"], axis=1)
            if ["none"] in list(dem0.columns):
                dem0 = dem0.drop(["none"], axis=1)
            # time - operations
            operations0 = dem0.sum(axis=1)
            nopers0 = normalizeOperations(operations0, movements0)

            computeAndPlot(movements0, nopers0, pathcommon +"pearson_%s_WL.pdf" % (ncase))

            ### SAME PREVIOUS
            # get mov from users Type1
            codes = dfapp[dfapp.type == 1].code.values
            dfmov1 = dfmov[dfmov.taxi.isin(codes)]
            dfmov1["count"] = np.ones(len(dfmov1))

            ### TIME - MOVES
            # time - moves0
            movements1 = dfmov1.groupby("time")["count"].sum()

            ### OPERATIONS - 0
            dem = pd.read_csv(detailedMoves)
            dem["type"] = dem.app.map(mapapps)

            dem1 = dem[dem.type == 1]

            dem1 = dem1.groupby(["time", "action"]).agg({"action": "count"})
            dem1.columns = ["freq"]
            dem1.reset_index(level=["time", "action"], inplace=True)
            dem1 = dem1.pivot_table('freq', ['time'], 'action')
            dem1 = dem1.fillna(0)

            dem1 = dem1.drop(["nop"], axis=1)
            if ["none"] in list(dem1.columns):
                dem1 = dem1.drop(["none"], axis=1)
            # time - operations
            operations1 = dem1.sum(axis=1)
            nopers1 = normalizeOperations(operations1, movements1)

            computeAndPlot(movements1, nopers1, pathcommon +"pearson_%s_LS.pdf"%(ncase))

        except:
            print("Some error in plot pearson coefficient: " + ncase)

if __name__ == '__main__':
    run()
    print("Done")