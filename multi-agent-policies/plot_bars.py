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

trApp1 = [55.76,23.81,17.37,20.01,17.94]
trApp2 = [100.20,131.43,70.01,41.58,50.40]
trApp3 = [117.94,10.00,16.34,14.41,12.90]

trs=[trApp1,trApp2,trApp3]

rrApp1 = [0.312,0.707,0.734,0.872,0.919]
rrApp2 = [0.214,1.702,1.351,1.266,1.352]
rrApp3 = [0.250,0.624,1.203,0.638,0.640]


rrs=[rrApp1,rrApp2,rrApp3]

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
        ax.bar(acc, trs[ap][i], width, label=label,color=color[i])

        ax.annotate('%0.0f'%trs[ap][i],
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
        ax.bar(acc, rrs[ap][i], width, label=label,color=color[i])

        ax.annotate('%0.2f'%rrs[ap][i],
              xy=(acc, rrs[ap][i]),
              xytext=(1,2),  # 3 points vertical offset
              textcoords="offset points",
              ha='center', va='bottom',
              fontsize=20)

ax.hlines(y=1,xmin=0,xmax=10,color="gray")
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
