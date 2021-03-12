#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 11 12:13:11 2021

@author: isaaclera
"""

import os
import json
from  collections import defaultdict
import numpy as np
import pandas as pd
from scipy import stats
import matplotlib.pyplot as plt

pd.options.mode.chained_assignment = None  # default='warn'


apps = 6


usecols = [0,2,5,6,7,9,12,13]


with open("experiments.json") as f:
    experiments = json.load(f)

expResponses = dict()
maxNR=-1
    
for item in experiments:
    # item = experiments[0]
    # print(item)
    code = item["code"]
    name = item["scenario"]
    policy = item["policy"]
    nm_policy = item["n_policy"]
    radius = item["radius"]
    reversepath = item["reversepath"]
    
    # experiment_path = "scenarios/%s/"%name
    
    # datestamp = time.strftime('%Y%m%d_%H%M')
    datestamp = "X"
    pathcommon = "fromServer/results_%s_%s" % (code, datestamp) + "/"
    
    pathfile_agg_actions = pathcommon+"agg_operations_NM.csv"
    pathfile_mov = pathcommon+"movements.csv"
    res = pathcommon+"Results_%s_0.csv"%name
    spc_actions = pathcommon+"specific_actions.csv"
    
    
    df = pd.read_csv(res,usecols=usecols)
    groupsAppUserRequests = df[df["module.src"] == "None"].groupby(['app','DES.src',])['id'].apply(list)
    
    appResponses = defaultdict(dict)
    # maxNR=-1
    
    expResponses[code]=defaultdict(dict)
    
    for (app, DESsrc) in groupsAppUserRequests.index:
    
        # print("App: %i"%app)
        # print("\t on service: %i"%DESsrc)
        # print("\t on node: %s"%np.unique(df[df["DES.src"] == DESsrc]["TOPO.src"]))
        ids = groupsAppUserRequests[(app, DESsrc)]
        # print("\t total requests: %i"%len(ids))
        # print("\t n.messages %i"%len(ids))
    
        
        dtmp = df[df["id"].isin(ids)]
        dtmp["response"] = dtmp["time_out"] - dtmp["time_emit"]
        if len(dtmp["response"])>maxNR:
            maxNR=len(dtmp["response"])
        expResponses[code][app][DESsrc]=list(dtmp["response"].values)
        
        
    
    # fig, ax = plt.subplots(figsize=(20, 12))
    # color = {1:"orange",2:"green",3:"purple"}
    # x = range(maxNR)
    # for app in appResponses:
    #     print(app)
    #     reset = True
    #     for des in appResponses[app]:
    #         values = appResponses[app][des]
    #         ns = len(values)
    #         ad = maxNR-ns
    #         values = values + [0]*ad
    #         c = color[app]
    #         label = None
    #         if reset:
    #             label="App %i"%app
    #             reset = False
    #         plt.plot(x,values,color=c,label=label)
            
    # plt.title("User response time", fontsize=38)
    # plt.xlabel(r"requests", fontsize=30)
    # plt.ylabel(r"ms", fontsize=30)
    # plt.legend(prop={'size': 30})
    # plt.show()
    # fig.savefig(pathcommon +"responses_%s.pdf"%code, dpi=400)
    # plt.close()

del expResponses['Model_NoAdapt_Profile_r1_pr5_nout_s10']



name={'Model_NoAdapt_r1_pr5_nout_s10':"Fog-based",'Model_Adapt_r1_pr5_nout_s10':"Osmotic-based",
      'Model_Cloud_r1_pr5_nout_s10':"All-in-Cloud",
      'Model_Edge_r1_pr5_nout_s10':"All-in-Edge",
      'Model_Adapt_Profile_r1_pr5_nout_s10':"Profiled osmotic-based"}



# fig, ax = plt.subplots(figsize=(20, 12))
# color = {0:"#08F7FE",1:"orange",2:"green",3:"purple",4:"gray",5:"pink",7:"red"}
# x = range(maxNR)
# for exp,code in enumerate(expResponses):
#     print(code)
#     reset = True
#     for app in expResponses[code]:
#         if app==1 or app==2: continue
#         avg = []
#         std = []
#         for des in expResponses[code][app]:
#             values = expResponses[code][app][des]
#             ns = len(values)
#             ad = maxNR-ns
#             values = values + [0]*ad
#             c = color[exp]
#             label = None
#             if reset:
#                 label=name[code]
#                 reset = False
#             plt.plot(x,values,color=c,label=label,alpha=.4,linewidth=8)
        
# plt.title("User response time", fontsize=38)
# plt.xlabel(r"requests", fontsize=30)
# plt.ylabel(r"ms", fontsize=30)
# plt.legend(prop={'size': 30})
# plt.show()
# fig.savefig(pathcommon +"responses_%s.pdf"%code, dpi=400)
# plt.close()

namesCols = ["code","app"]+list(range(maxNR))
df = pd.DataFrame([],columns=namesCols)
i=0
for exp,code in enumerate(expResponses):
    for app in expResponses[code]:
        for des in expResponses[code][app]:
            values = expResponses[code][app][des]
            ns = len(values)
            ad = maxNR-ns
            values = values + [0]*ad
            df.loc[i]= [code,app]+values
            i+=1
mn = df.groupby(["code","app"]).mean()
st = df.groupby(["code","app"]).std()

for apix in [0,1,2]:
    fig, ax = plt.subplots(figsize=(20, 12))
    plt.title("Average response time of App%i"%(apix+1), fontsize=38)
    for i in range(apix,len(mn),3):
        print(i)
        (model,app) = mn.index[i]
        print("model, app",model,app)
        plt.plot(mn.iloc[i].values,label=name[model],linewidth=8)
        lowlevel = mn.iloc[i]-st.iloc[i] 
        lowlevel[lowlevel<0]=0
        plt.fill_between(range(maxNR),lowlevel,mn.iloc[i]+st.iloc[i],alpha=.1)
    plt.xlabel(r"requests", fontsize=30)
    plt.ylabel(r"ms", fontsize=30)
    plt.legend(ncol=3,prop={'size': 20})
    plt.show()
    fig.savefig("responses_avg_%s.pdf"%app, dpi=400)
    plt.close()