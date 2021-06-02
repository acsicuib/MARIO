import json
import pandas as pd


import glob

pd.options.mode.chained_assignment = None  # default='warn'



def run(fileName):

    with open(fileName) as f:
        experiments = json.load(f)
    
    for item in experiments:
# item = experiments[0]
        try:
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

            fileStats = open(pathcommon + "requests_stats_%s.txt" % code, "w")
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


            df = pd.read_csv(pathcommon + "requests_stats_%s.txt" % code)
            file2 = open(pathcommon + "requests_info_stats_%s.txt" % code, "w")
            file2.write(str(df.groupby("app").agg({"suc":["mean","std","max","sum"]})))
            file2.flush()
            file2.close()
        except:
            print("Error in one totalrequests")
if __name__ == '__main__':
    run()
    print("Done")