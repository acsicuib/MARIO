#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct 26 12:04:35 2020

It checks information about the Taxi Rome traces (.csv)

@author: isaaclera
"""

import pandas as pd
import numpy as np
from matplotlib import cm

import gpxpy
import gpxpy.gpx
import datetime


df = pd.read_csv("data/raw_trajectories.csv", ",")

df.index = pd.DatetimeIndex(df[df.columns[1]])

df = df.drop(columns=['date time'])
df = df.rename(columns={"taxi id": "taxiid"})

(unique, counts) = np.unique(df.taxiid, return_counts=True)

print("Taxis: %i"%(len(unique)))
print("Average number of movements: %f",np.mean(counts))
print("Time range: ", df.index.max()-df.index.min()) # 14 minutes

nipy = cm.get_cmap('nipy_spectral',len(unique))
newcolors = nipy(np.linspace(0, 1, len(unique)))

# Generating a JSON file to represent it on Deck.gl
f = open("traces.json","w")
mapcolors = {}
counter= 0

dfg = df.groupby(["taxiid"])

for idx,(group_name,df_group) in enumerate(dfg):
    color = newcolors[idx]
    weight = 4
    
    coordSRC = [df_group.iloc[0].longitude,df_group.iloc[0].latitude]
    for irow in range(1,len(df_group)):
        coordTRG= [df_group.iloc[irow].longitude,df_group.iloc[irow].latitude]
        f.write("{path:[%s,%s],name:\"%s\",color:[%f,%f,%f],scale:%i},\n"%
            (coordSRC,coordTRG,group_name,
             color[0]*256,color[1]*256,color[2]*256,weight))
        coordSRC = coordTRG
f.close()

# ================
rcoord = [df.latitude.min(),df.longitude.max()]
lcoord = [df.latitude.max(),df.longitude.min()]





# print(gpx.to_xml())


df = pd.read_csv("data/raw_trajectories.csv", ",")
df = df.rename(columns={"taxi id": "taxi"})
dfg = df.groupby(["taxi"])


for idx, (group_name, df_group) in enumerate(dfg):
    #df routes are sorted by time
    
    gpx = gpxpy.gpx.GPX()
    gpx_track = gpxpy.gpx.GPXTrack()
    gpx.tracks.append(gpx_track)
    gpx_segment = gpxpy.gpx.GPXTrackSegment()
    gpx_track.segments.append(gpx_segment)
    
    # Create points:
    for idx in df_group.index:
        date_time_obj = datetime.datetime.strptime(df_group.loc[idx]["date time"], '%Y-%m-%d %H:%M:%S')
        
        gpx_segment.points.append(gpxpy.gpx.GPXTrackPoint(df_group.loc[idx].latitude, df_group.loc[idx].longitude,
                                                          time = date_time_obj,
                                                          elevation = 0
                                                          ))


    with open('taxi_%i.gpx'%group_name, 'w') as f:
        f.write(gpx.to_xml())
    
    




