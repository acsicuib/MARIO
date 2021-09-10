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

#MEDIUM SNCEARIO - v2 of the rules
# typescale = "medium"
# trApp1 =[30.072263,2,27.923563,27.641829,27.116772]
# trApp2 =[30.175586,2.020003,22.607502,18.071586,16.622579]
# trApp3 =[30.546664,2,17.784587,21.262433,21.138951]
#
# errorApp1 = [0.398566,0,0.450775,0.580998,0.475383]
# errorApp2 = [0.571435,0.014552,0.356399,0.435624,0.45256]
# errorApp3 = [0.243134,0,0,0.198566,0.138243]


# # SMALL SCENARIO - 10 simulations
typescale = "small"
trApp1 =[14.613333,2.22,8.182424,8.188788,8.188788]
trApp2 =[20.407071,2.461616,4.170278,5.935404,5.935404]
trApp3 =[24.094545,4,7.074545,7.428364,7.428364]

errorApp1 = [0.10942,0.071661,0.014545,0.014545,0.014545]
errorApp2 = [0.10378,0.128331,0.273025,0.110986,0.110986]
errorApp3 = [0.335434,0,0.129385,0.317049,0.317049]

trs=[trApp1,trApp2,trApp3]
trerror =[errorApp1,errorApp2,errorApp3]


#MEDIUM SNCEARIO - v2 of the rules
# typescale = "medium"
# rrApp1 = [0.312762,0.284672,0.312791,0.74328,0.742653]
# rrApp2 = [0.231697,1.422833,0.462703,0.634548,0.639365]
# rrApp3 = [0.250233,0.028436,0.251526,0.310549,0.309942]
#
# rorApp1 = [0.000029,0,0,0.005048,0.006656]
# rorApp2 = [0,0,0.00003,0.021252,0.05647]
# rorApp3 = [0,0,0,0,0]

# #SMALL SCENARIO - 10 simulations
typescale = "small"

rrApp1 = [0.125078,0.333052,0.753721,0.753721,0.753721]
rrApp2 = [0.214341,0.369055,0.65358,0.68841,0.68841]
rrApp3 = [0.125,0.3125,0.625,0.625,0.625]

rorApp1 = [0,0.016615,0,0,0]
rorApp2 = [0,0.006441,0.076739,0.064936,0.064936]
rorApp3 = [0,0,0,0,0]


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
fig.savefig("bar_response_time_%s.pdf"%typescale, dpi=400)




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
fig.savefig("bar_service_usage_%s.pdf"%typescale, dpi=400)
