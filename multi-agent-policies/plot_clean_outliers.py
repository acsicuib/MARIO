import pandas as pd
import json
import collections
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.gridspec as gridspec

def is_outlier(points, thresh=3.5):
    """
    Returns a boolean array with True if points are outliers and False
    otherwise.

    Parameters:
    -----------
        points : An numobservations by numdimensions array of observations
        thresh : The modified z-score to use as a threshold. Observations with
            a modified z-score (based on the median absolute deviation) greater
            than this value will be classified as outliers.

    Returns:
    --------
        mask : A numobservations-length boolean array.

    References:
    ----------
        Boris Iglewicz and David Hoaglin (1993), "Volume 16: How to Detect and
        Handle Outliers", The ASQC Basic References in Quality Control:
        Statistical Techniques, Edward F. Mykytka, Ph.D., Editor.
    """
    if len(points.shape) == 1:
        points = points[:,None]
    median = np.median(points, axis=0)
    diff = np.sum((points - median)**2, axis=-1)
    diff = np.sqrt(diff)
    med_abs_deviation = np.median(diff)

    if med_abs_deviation == 0: return False
    modified_z_score = 0.6745 * diff / med_abs_deviation

    return modified_z_score > thresh

def run(fileName,threshold):
    name=["All-in-Cloud","All-in-Edge","Fog-based","Osmotic-based","Profiled osmotic-based"]
    with open(fileName) as f:
        experiments = json.load(f)
        for item in experiments:
            # item = experiments[0]
            # try:
                code = item["code"]
                print("Code %s"%code)

                if "Cloud" in code or "Edge" in code: continue

                name = item["scenario"]
                policy = item["policy"]
                nm_policy = item["n_policy"]
                radius = item["radius"]
                reversepath = item["reversepath"]

                experiment_path = "scenarios/%s/" % name

                # datestamp = time.strftime('%Y%m%d_%H%M')
                datestamp = "X"
                pathcommon = "results/results_%s_%s" % (code, datestamp) + "/"

                pathfile_agg_actions = pathcommon + "agg_operations_NM.csv"
                pathfile_mov = pathcommon + "movements.csv"
                res = pathcommon + "Results_%s_0.csv" % name
                spc_actions = pathcommon + "specific_actions.csv"

                finalStats_response = pathcommon + "final_response_stats_%s.txt"%code
                fileStats = open(pathcommon + "clean_outliers_response__%s.txt" % code, "w")
                df = pd.read_csv(finalStats_response,header=None)
                for app,row in enumerate(df.iterrows()):
                    values = np.array(row[1])
                    values = values[1:-2]
                    print("\tTotal number of values:",len(values))
                    filtered = values[~is_outlier(values,threshold)]
                    try:
                        print("\tTotal number of filtered items:",len(values)-len(filtered))
                    except TypeError:
                        print("\t WARNING: Median 0 in Response time ",filtered)
                        print(values)
                        filtered = values

                    fileStats.write("%i," %(app+1))
                    for v in filtered:
                        fileStats.write("%f,"%v)
                    fileStats.write("%f,%f\n"%(np.mean(filtered),np.std(filtered)))

                fileStats.flush()
                fileStats.close()

                finalStats_response = pathcommon + "final_serviceusage_stats_%s.txt" % code
                fileStats = open(pathcommon + "clean_outliers_serviceusage_%s.txt" % code, "w")
                df = pd.read_csv(finalStats_response, header=None)
                for app, row in enumerate(df.iterrows()):
                    values = np.array(row[1])
                    values = values[1:-2]
                    print("\tTotal number of values:", len(values))
                    filtered = values[~is_outlier(values, threshold)]
                    try:
                        print("\tTotal number of filtered items:", len(values) - len(filtered))

                    except TypeError:
                        print("\t WARNING: Median 0 in Service Usage ",filtered)
                        print(values)
                        filtered = values

                    fileStats.write("%i," % (app + 1))
                    for v in filtered:
                        fileStats.write("%f," % v)
                    fileStats.write("%f,%f\n" % (np.mean(filtered), np.std(filtered)))

                fileStats.flush()
                fileStats.close()

            #
            # except:
            #     print("Somethings goes wrong")

if __name__ == '__main__':
    # run(fileName="experiment_LSeed_v2Fog.json",threshold=5.0)
    # run(fileName="experiment_ML.json",threshold=5.0)
    run(fileName="experiment_SSeed.json",threshold=5.0)
    print("Done")