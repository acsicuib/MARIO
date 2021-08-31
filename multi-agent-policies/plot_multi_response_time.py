import os
import json
import collections
from configparser import ConfigParser

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
    # print(msg)

def run(fileName,renderPlot=False):
    with open(fileName) as f:
        experiments = json.load(f)



    for item in experiments:
            response_results = collections.defaultdict(list)
        # try:
            # item = experiments[0]
            # print(item)
            code = item["code"]
            name = item["scenario"]
            policy = item["policy"]
            nm_policy = item["n_policy"]
            radius = item["radius"]
            reversepath = item["reversepath"]

            experiment_path = "scenarios/%s/"%name

            config = ConfigParser()
            config.read(experiment_path + 'config.ini')
            nSimulations = int(config.get('simulation', 'nSimulations'))

            # datestamp = time.strftime('%Y%m%d_%H%M')
            datestamp = "X"
            pathcommon = "results/results_%s_%s" % (code, datestamp) + "/"

            for iteration in range(nSimulations):

                pathfile_agg_actions = pathcommon+"agg_operations_NM_%i.csv"%iteration
                pathfile_mov = pathcommon+"movements_%i.csv"%iteration
                res = pathcommon+"Results_%s_%i.csv"%(name,iteration)
                spc_actions = pathcommon+"specific_actions_%i.csv"%iteration

                fileStats = open(pathcommon + "response_stats_%s_%i.txt" % (code,iteration), "w")


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

                    # print("\t n.messages %i"%len(ids))


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

                    if renderPlot:
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

                    response_results[app].append(avg)

                    avg = np.array(avg_per_app_s[app]).mean()
                    myprint("\t avg. successful requests: %f" % avg, fileStats)
                    avg = np.array(avg_per_app_ps[app]).mean()
                    myprint("\t avg. %% successful requests: %f" % avg, fileStats)

            #end experiment
            finalstats = open(pathcommon + "final_response_stats_%s.txt" % (code), "w")
            for app in response_results.keys():
                finalstats.write(str(app)+",")
                for value in response_results[app]:
                    finalstats.write("%f,"%value)

                avg = np.array(response_results[app]).mean()
                std = np.array(response_results[app]).std()
                finalstats.write("%f,%f\n"%(avg,std))

            finalstats.flush()
            finalstats.close()

        # except:
        #     print("Error in one response time")

if __name__ == '__main__':
    run(fileName="experiment_MSeed.json")
    print("Done")