import json
import collections
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

pd.options.mode.chained_assignment = None  # default='warn'

#
# Compute the % of successful request
#

usecols = [0,2,5,6,7,9,12,13]

def myprint(msg,fileStats):
    fileStats.write("\n"+msg)
    print(msg)

def run(datestamp):
    with open("experiments.json") as f:
        experiments = json.load(f)

    # datestamp = time.strftime('%Y%m%d')
    # datestamp = "20210318" # fixed for testing

    for item in experiments:
        try:
            code = item["code"]
            experiment_path = item["path"]
            pathcommon = experiment_path + "results_%s_%s/" % (code, datestamp)

            res = pathcommon + "Results_Rome_0.csv"

            fileStats = open(pathcommon + "response_stats_%s.txt" % code, "w")


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
            print("Error in one response time")

if __name__ == '__main__':
    run()
    print("Done")