#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct 26 12:04:35 2020

@author: isaaclera
"""

import pandas as pd
import numpy as np
from tiledTopology import TiledTopology
import networkx as nx

df = pd.read_csv("scenarios/TaxiRome/data/raw_trajectories.csv", ",")

df.index = pd.DatetimeIndex(df[df.columns[1]])

df = df.drop(columns=['date time'])
df = df.rename(columns={"taxi id": "taxiid"})

(unique, counts) = np.unique(df.taxiid, return_counts=True)

print("Taxis: %i" % (len(unique)))
print("Average number of movements: %f", np.mean(counts))
print("Time range: ", df.index.max() - df.index.min())  # 14 minutes


rcoord = [df.latitude.min(), df.longitude.max()]
lcoord = [df.latitude.max(), df.longitude.min()]

projection = [rcoord,lcoord]
print(projection)

tiledTopo = TiledTopology(10)
G = tiledTopo.TiledGraph(projection)

nx.write_gexf(G,"graph_grid_%i.gexf"%tiledTopo.size)
