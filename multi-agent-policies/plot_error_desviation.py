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
# Compute the % of the desviation vs simuland mean value 
#

def run(fileName,renderPlot=False):
    with open(fileName) as f:
        experiments = json.load(f)



    allErrorValues = []
    for item in experiments:
            response_results = collections.defaultdict(list)
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
                final_res = pathcommon+"final_response_stats_%s.txt"%code

                df = pd.read_csv(final_res,header=None)
                
                print(df)
                error = (df[df.columns[-1]]/df[df.columns[-2]])
                error = error*100
                print(error.values)
                allErrorValues.append(error.values)

    val = np.array(allErrorValues).flatten()
    print("Min value %f"%val.min())
    print("Max value %f"%val.max())
    print("Mean value %f"%val.mean())    
    print("STD value %f"%val.std())    
    
if __name__ == '__main__':
    run(fileName="experiment_MSeed.json")
    print("Done")