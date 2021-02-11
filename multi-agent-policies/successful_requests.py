import os
import json
import collections
import numpy as np
import pandas as pd
from scipy import stats

#
# Compute the % of successful request
#

#fake deadline for each app
apps = 6
deadline = [100]*apps

usecols = [0,2,7,9,12,13]

if __name__ == '__main__':

    with open("experiment.json") as f:
        experiments = json.load(f)

    for item in experiments:
        print("Experiment %s with code %s "%(item["scenario"],item["code"]))
        pathFolder = "results/results_%s_%s/" %(item["code"],item["scenario"])
        path = pathFolder + "Results_%s_%i.csv" % (item["scenario"], 0) # 0 - iterations

        df = pd.read_csv(path,usecols=usecols)
        groupsAppUserRequests = df[df["module.src"] == "None"].groupby(['app', 'TOPO.src'])['id'].apply(list)

        # fake deadline for each app
        apps = np.unique(df.app)
        deadline = [100] * len(apps)

        global_satis = []
        for (app, user) in groupsAppUserRequests.index:
            print("App: ", app - 1)
            print("\t user: ", user)
            ids = groupsAppUserRequests[(app, user)]
            print("\t total requests: ", len(ids))
            dtmp = df[df["id"].isin(ids)]
            dtmp["response"] = dtmp["time_out"] - dtmp["time_emit"]
            print("\t average response: ", dtmp["response"].mean())
            satis = np.sum(dtmp["response"] < deadline[app - 1])
            print("\t total successful requests: ", satis)
            satis = satis / float(len(dtmp))
            global_satis.append(satis)
            print("\t     %% successful requests: %.2f" % (satis))

        global_satis = np.array(global_satis)

        print("Global %% of successful requests: %.2f" % global_satis.mean())