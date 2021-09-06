#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar 12 11:15:04 2021

@author: isaaclera
"""
import matplotlib
import matplotlib.pyplot as plt
import numpy as np


name=["All-in-Cloud","All-in-Edge","Fog-based","Osmotic-based","Profiled osmotic-based"]
color = {0:"#08F7FE",1:"Carrot",2:"green",3:"purple",4:"carmine",5:"pink",7:"red"}



color = ["#7BD5F5","#DC8665","#138086","#534666","#FFCAD4"]      
# 86E3CE

color = ["#7BD5F5","#D0E6A5","#FFDD94","#FA897B","#CCABD8"]      



# SMALL SCENARIO - 10 simulations
trApp1 = [53.529113,2.220000,  4.705485,    17.747394,17.748576]
trApp2 = [98.336873,2.461616,  5.769192,    10.649520,8.708434]
trApp3 = [114.589747,7.000000, 13.981364,   15.518364,15.364091]
errorApp1 = [1.609905,0.071661,0.253253,    0.135108,0.134552]
errorApp2 = [1.425300,0.128331,0.082834,    0.948607,0.912645]
errorApp3 = [1.168209,0,0.296874,           0.541637,0.512973]

# MEDIUM SCENARIO - 10 simulations
# trApp1 = [30.07,2.08,20.2678,24.9722,24.6578]
# trApp2 = [30.17,2.0265,8.626045,17.08994,17.1744]
# trApp3 = [30.55,2,19.287314,17.923587,17.710178]
# errorApp1 = [0.398566,0.216255,4.648492,1.4458,1.4084]
# errorApp2 = [0.571435,0.024007,0.207795,1.044,1.0881]
# errorApp3 = [0.243134,0,0.472720,0.303,0.098316]

#original
# trApp1 = [55.76,23.81,17.37,20.01,17.94]
# trApp2 = [100.20,131.43,70.01,41.58,50.40]
# trApp3 = [117.94,10.00,16.34,14.41,12.90]

trs=[trApp1,trApp2,trApp3]
trerror =[errorApp1,errorApp2,errorApp3]

#original
# rrApp1 = [0.312,0.707,0.734,0.872,0.919]
# rrApp2 = [0.214,1.702,1.351,1.266,1.352]
# rrApp3 = [0.250,0.624,1.203,0.638,0.640]


#SMALL SCENARIO - 10 simulations
rrApp1 = [0.312490,0.666105,  0.638904, 1.880233,1.880233]
rrApp2 = [0.214352,1.651686,   1.331715,0.736599,1.182850]
rrApp3 = [0.124988,0.312500,      0.625000,0.628824,0.625000]

rorApp1 = [0.000029,0.033231,   0.024208,0.000000,0.000000]
rorApp2 = [0.000022,0.038647,   0.104000,0.004965,0.099937]
rorApp3 = [0.000035,0,          0.0,0.000000,0]

#MEDIUM SCENARIO - 10 simulations

# rrApp1 = [0.312762,0.284672,  0.396978,0.437564,0.452089]
# rrApp2 = [0.231019,1.418658,   0.809885,0.72423,0.717388]
# rrApp3 = [0.250233,0.028,      0.2610,0.25151,0.251526]
# rorApp1 = [0.000029,0 ,0.0579,0.07498,0.082531]
# rorApp2 = [0.001362,0.00835,0.01110,0.050888,0.044204]
# rorApp3 = [0,0, 0.01605,0.000186,0]

rrs=[rrApp1,rrApp2,rrApp3]
trrr = [rorApp1,rorApp2,rorApp3]

x = np.array([0,1,2])
labels = ["App1","App2","App3"]
width = 0.55
shiftA = 4
xticks = [width*2,shiftA+width*2,shiftA*2+width*2]

fig, ax = plt.subplots(figsize=(20, 12))
shiftx = 0
for ap in x:
    shiftx = shiftA *ap
    for i in range(len(trs[ap])):
        acc = shiftx+(width*i)
        label = name[i]
        if ap>=1: label = None
        ax.bar(acc, trs[ap][i], yerr=trerror[ap][i], width=width, label=label,color=color[i])

        ax.annotate('%0.2f'%trs[ap][i],
              xy=(acc, trs[ap][i]),
              xytext=(1,2),  # 3 points vertical offset
              textcoords="offset points",
              ha='center', va='bottom',
              fontsize=20)


# Add some text for labels, title and custom x-axis tick labels, etc.
ax.set_ylabel('Response time',fontsize=32)
# ax.set_title('Response time by Experiment and App',fontsize=35)
ax.set_xticks(xticks)
ax.set_xticklabels(labels)
for tick in ax.get_xticklabels():
    tick.set_fontsize(28)
for tick in ax.get_yticklabels():
    tick.set_fontsize(24)
ax.legend(ncol=1,loc=0,prop={'size': 30})
fig.tight_layout()
plt.show()
fig.savefig("bar_response_time.pdf", dpi=400)




fig, ax = plt.subplots(figsize=(20, 12))
shiftx = 0
for ap in x:
    shiftx = shiftA *ap
    for i in range(len(rrs[ap])):
        acc = shiftx+(width*i)
        label = name[i]
        if ap>=1: label = None
        ax.bar(acc, rrs[ap][i], yerr=trrr[ap][i],width = width, label=label,color=color[i])

        ax.annotate('%0.2f'%rrs[ap][i],
              xy=(acc, rrs[ap][i]),
              xytext=(1,2),  # 3 points vertical offset
              textcoords="offset points",
              ha='center', va='bottom',
              fontsize=20)

ax.hlines(y=1,xmin=0.5,xmax=9.5,color="gray",linewidth=5)
ax.hlines(y=1,xmin=0,xmax=0.5,color="gray",linewidth=5,linestyles="--")
ax.hlines(y=1,xmin=9.5,xmax=10,color="gray",linewidth=5,linestyles="--")

ax.text(7.5,1.05,"under provisioned",color="gray",fontsize=24)
ax.text(7.5,0.92,"over provisioned",color="gray",fontsize=24)

# Add some text for labels, title and custom x-axis tick labels, etc.
ax.set_ylabel('Service Usage',fontsize=32)
# ax.set_title('Response time by Experiment and App',fontsize=35)
ax.set_xticks(xticks)
ax.set_xticklabels(labels)
for tick in ax.get_xticklabels():
    tick.set_fontsize(28)
for tick in ax.get_yticklabels():
    tick.set_fontsize(24)    
ax.legend(ncol=1,loc="upper left",prop={'size': 30})
fig.tight_layout()
plt.show()
fig.savefig("bar_service_usage.pdf", dpi=400)
