import os
import json
import collections
import numpy as np
import pandas as pd
from scipy import stats
import matplotlib.pyplot as plt

pd.options.mode.chained_assignment = None  # default='warn'

#
# Compute the % of successful request
#

#fake deadline for each app
apps = 6
deadline = [50]*apps

usecols = [0,2,5,6,7,9,12,13]

def myprint(msg,fileStats):
    fileStats.write("\n"+msg)
    print(msg)

def run():
    with open("experiment.json") as f:
        experiments = json.load(f)

    for item in experiments:
        try:
            # item = experiments[0]
            # print(item)
            code = item["code"]
            name = item["scenario"]
            policy = item["policy"]
            nm_policy = item["n_policy"]
            radius = item["radius"]
            reversepath = item["reversepath"]

            experiment_path = "scenarios/%s/"%name

            # datestamp = time.strftime('%Y%m%d_%H%M')
            datestamp = "X"
            pathcommon = "results/results_%s_%s" % (code, datestamp) + "/"

            pathfile_agg_actions = pathcommon+"agg_operations_NM.csv"
            pathfile_mov = pathcommon+"movements.csv"
            res = pathcommon+"Results_%s_0.csv"%name
            spc_actions = pathcommon+"specific_actions.csv"

            fileStats = open(pathcommon + "response_stats_%s.txt" % code, "w")

            # print("Experiment %s with code %s "%(item["scenario"],item["code"]))
            # pathFolder = "results/results_%s_%s/" %(item["code"],item["scenario"])
            # path = pathFolder + "Results_%s_%i.csv" % (item["scenario"], 0) # 0 - iterations

            # df = pd.read_csv(res,usecols=usecols)
            # groupsAppUserRequests = df[df["module.src"] == "None"].groupby(['app', 'TOPO.src'])['id'].apply(list)

            # fake deadline for each app
            # apps = np.unique(df.app)
            # deadline = [100] * len(apps)
            #
            # global_satis = []
            # for (app, node) in groupsAppUserRequests.index:
            #
            #     myprint( "App: %i"%app,fileStats)
            #     myprint("\t on node: %i"%node,fileStats)
            #     ids = groupsAppUserRequests[(app, node)]
            #     myprint("\t total requests: %i"%len(ids),fileStats)
            #     dtmp = df[df["id"].isin(ids)]
            #     dtmp["response"] = dtmp["time_out"] - dtmp["time_emit"]
            #     myprint("\t average response: %f"%dtmp["response"].mean(),fileStats)
            #
            #     satis = np.sum(dtmp["response"] < deadline[app - 1])
            #     myprint("\t total successful requests: %f"%satis,fileStats)
            #     satis = satis / float(len(dtmp))
            #     global_satis.append(satis)
            #     myprint("\t     %% successful requests: %.2f" % (satis),fileStats)
            #
            #     fig, ax = plt.subplots(figsize=(20, 12))
            #     dtmp.response.plot(ax=ax)
            #     plt.title('Response .  User %i on APP: %i' % (node, app), fontsize=38)
            #     # plt.show()
            #     fig.savefig(pathcommon +"response_on_node%i.pdf" % node, dpi=400)
            #     plt.close()
            #
            # global_satis = np.array(global_satis)
            #
            # myprint("Global %% of successful requests: %.2f" % global_satis.mean(),fileStats)
            #

            df = pd.read_csv(res,usecols=usecols)
            groupsAppUserRequests = df[df["module.src"] == "None"].groupby(['app','DES.src',])['id'].apply(list)

            # fake deadline for each app
            apps = np.unique(df.app)
            deadline = [100] * len(apps)

            global_satis = []

            avg_per_app_r = collections.defaultdict(list)
            avg_per_app_s = collections.defaultdict(list)
            avg_per_app_ps = collections.defaultdict(list)

            for (app, DESsrc) in groupsAppUserRequests.index:

                myprint("App: %i"%app,fileStats)
                myprint("\t on service: %i"%DESsrc,fileStats)
                myprint("\t on node: %s"%np.unique(df[df["DES.src"] == DESsrc]["TOPO.src"]),fileStats)
                ids = groupsAppUserRequests[(app, DESsrc)]
                myprint("\t total requests: %i"%len(ids),fileStats)

                print("\t n.messages %i"%len(ids))


                dtmp = df[df["id"].isin(ids)]
                dtmp["response"] = dtmp["time_out"] - dtmp["time_emit"]
                avg_value = dtmp["response"].mean()
                myprint("AVG %f"%dtmp["response"].mean(),fileStats)
                myprint("MIN %f"%dtmp["response"].min(),fileStats)
                myprint("MAX %f"%dtmp["response"].max(),fileStats)
                myprint("STD %f"%dtmp["response"].std(),fileStats)


                myprint("\t average response: %d"%dtmp["response"].mean(),fileStats)
                avg_per_app_r[app].append(dtmp["response"].mean())

                satis = np.sum(dtmp["response"] < deadline[app - 1])
                myprint("\t total successful requests: %f"%satis,fileStats)
                avg_per_app_s[app].append(satis)

                satis = satis / float(len(dtmp))
                avg_per_app_ps[app].append(satis)

                global_satis.append(satis)
                myprint("\t     %% successful requests: %.2f" % (satis),fileStats)

                fig, ax = plt.subplots(figsize=(20, 12))
                dtmp.response.plot(ax=ax)
                plt.title('Response .  User %i on APP: %i' % (DESsrc, app), fontsize=38)
                # plt.show()
                fig.savefig(pathcommon +"response_user%i.pdf" % DESsrc, dpi=400)
                plt.close()

            global_satis = np.array(global_satis)

            myprint("Global %% of successful requests: %.2f" % global_satis.mean(),fileStats)

            for app in  avg_per_app_r:
                myprint("Averages by app: %i" % app, fileStats)
                avg = np.array(avg_per_app_r[app]).mean()
                myprint("\t avg. response time: %f" % avg, fileStats)
                avg = np.array(avg_per_app_s[app]).mean()
                myprint("\t avg. successful requests: %f" % avg, fileStats)
                avg = np.array(avg_per_app_ps[app]).mean()
                myprint("\t avg. %% successful requests: %f" % avg, fileStats)

        except:
            print("Eror in one response time")

if __name__ == '__main__':
    run()
    print("Done")