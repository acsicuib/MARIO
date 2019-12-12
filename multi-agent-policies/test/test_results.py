import pandas as pd
import numpy as np

import json

pathCSV = "/Users/isaaclera/PycharmProjects/DistributedPolicies/multi-agent-policies/scenarios/prototype1/results/Results_prot1_100000_0.csv"
previous_number_samples = 0
df = pd.read_csv(pathCSV, skiprows=range(1, previous_number_samples))  # include header
df = df[df["DES.dst"] == 21]
if len(df) > 0:
    # print("Number of samples: %i (from: %i)" % (len(df.index)-1, self.previous_number_samples))
    previous_number_samples += len(df.index) - 1


