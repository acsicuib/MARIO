import json
from configparser import ConfigParser
import collections
import pandas as pd

import numpy as np
import glob

pd.options.mode.chained_assignment = None  # default='warn'



def run(fileName):

    with open(fileName) as f:
        experiments = json.load(f)


    for item in experiments:

        usage_results = collections.defaultdict(list)
        #         try:
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
            print("Iteration %i"%iteration)
            pathfile_agg_actions = pathcommon+"agg_operations_NM_%i.csv"%iteration
            pathfile_mov = pathcommon+"movements_%i.csv"%iteration
            res = pathcommon+"Results_%s_%i.csv"%(name,iteration)
            spc_actions = pathcommon+"specific_actions_%i.csv"%iteration

            fileStats = open(pathcommon + "requests_stats_%s_%i.txt" %(code,iteration), "w")
            fileStats.write("time,app,des,tr,ltr,suc\n")
            plfiles = list(glob.glob(pathcommon+"models/*.pl"))
            plfiles = sorted(plfiles,key=lambda name:int(name.split("_")[-1].replace(".pl","")),reverse=False)

            print("Number of files: ",len(plfiles))
            for pathfile in plfiles:
                time = int(pathfile.split("_")[-1].replace(".pl",""))
                des = int(pathfile.split("_")[-3].replace("DES",""))
                app = -1
                with open(pathfile,"r") as f:
                    tr = 0
                    for line in f:
                        if line.startswith("requests("):
                            r = int(line.split(",")[2]) ## requests
                            tr += r

                        if line.startswith("serviceInstance("):
                            tok = line.split(",")
                            if "self" not in tok[-1]:
                                print("Warning")
                            else:
                                app = int(tok[1].replace("app",""))
                                ltr = int(tok[4].replace(")",""))

                if app>=0:
                    per = tr/ltr
                    fileStats.write("%i,%i,%i,%i,%i,%f\n"%(time,app,des,tr,ltr,per))

            fileStats.flush()
            fileStats.close()


            df = pd.read_csv(pathcommon + "requests_stats_%s_%i.txt" % (code,iteration))
            file2 = open(pathcommon + "requests_info_stats_%s_%i.txt" % (code,iteration), "w")
            file2.write(str(df.groupby("app").agg({"suc":["mean","std","max","sum"]})))
            file2.flush()
            file2.close()


            dg = df.groupby("app").agg({"suc": ["mean"]})
            for item in dg.iterrows():
                app = item[0]
                value = item[1].values[0]
                usage_results[app].append(value)

        #end experiment
        finalstats = open(pathcommon + "final_serviceusage_stats_%s.txt" % (code), "w")
        for app in usage_results.keys():
            finalstats.write(str(app) + ",")
            for value in usage_results[app]:
                finalstats.write("%f," % value)
            avg = np.array(usage_results[app]).mean()
            std = np.array(usage_results[app]).std()
            finalstats.write("%f,%f\n" % (avg, std))

        finalstats.flush()
        finalstats.close()


        # except:
        #     print("Error in one totalrequests")
if __name__ == '__main__':
    run(fileName="experiment_MSeed.json")
    print("Done")