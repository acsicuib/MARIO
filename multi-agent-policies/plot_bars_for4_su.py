#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul  9 08:39:30 2021

@author: isaaclera
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar 12 11:15:04 2021

@author: isaaclera
"""
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.gridspec as gridspec

name=["All-in-Cloud","All-in-Edge","Fog-based","Osmotic-based","Profiled osmotic-based"]
color = {0:"#08F7FE",1:"Carrot",2:"green",3:"purple",4:"carmine",5:"pink",7:"red"}



color = ["#7BD5F5","#DC8665","#138086","#534666","#FFCAD4"]      
# 86E3CE

color = ["#7BD5F5","#D0E6A5","#FFDD94","#FA897B","#CCABD8"]      

#LArge for v2 rules version

rrApp1 = [0.752105,0.417,0.67035,0.690944,0.808213]
rrApp2 = [1.100947,4.169832,0.570582,0.819347,0.852051]
rrApp3 = [0.588953,0.041699,0.539425,0.627986,0.751112]
rrApp4 = [1.210558,0.083424,0.661174,0.552618,0.481432]

# rrApp1 = [0.752105,0.417,0.697111,0.695595,0.82344]
# rrApp2 = [1.100947,4.169832,0.579293,0.856645,0.920153]
# rrApp3 = [0.588953,0.041699,0.546622,0.633757,0.753256]
# rrApp4 = [1.210558,0.083424,0.648039,0.564457,0.501058]


rrErrorApp1 = [0.002096,0,0.002357,0.005767,0.007945]
rrErrorApp2 = [0.003327,0,0.008121,0.010333,0.035005]
rrErrorApp3 = [0.000147,0,0,0.000013,0.000016]
rrErrorApp4 = [0.018952,0,0.030054,0.015963,0.00938]

# rrErrorApp1 = [0.002096,0,0.020623,0.016385,0.004608]
# rrErrorApp2 = [0.003327,0,0.006885,0.017315,0.060802]
# rrErrorApp3 = [0.000147,0,0,0,0]
# rrErrorApp4 = [0.018952,0,0.054024,0.022856,0.016079]


# LARGE RESULTS v1
# rrApp1 = [0.750,0.417,0.684,0.710,0.819]
# rrApp2 = [1.100,4.170,0.579,0.855,0.834]
#
# rrApp3 = [0.589,0.042,0.547,0.634,0.753]
# rrApp4 = [1.225,0.083,0.691,0.549,0.484]
# rrs=[rrApp1,rrApp2,rrApp3,rrApp4]

# LArge restults 5

# rrApp1 = [0.752105,0.417,0.614511,0.940581,0.940581]
# rrApp2 = [1.100947,4.169832,1.094396,1.057151,1.134020]
# rrApp3 = [0.588953,0.041699,0.481525,0.630469,0.749304]
# rrApp4 = [1.210558,0.083424,0.523047,0.530954,0.566843]
#
# rrErrorApp1 = [0.002096,0,0.006495,0,0]
# rrErrorApp2 = [0.003327,0,0.007779,0.040220,0.036544]
# rrErrorApp3 = [0.000147,0,0.000406,0,0]
# rrErrorApp4 = [0.018952,0,0.016124,0.039219,0.031071]

rrs=[rrApp1,rrApp2,rrApp3,rrApp4]
errs=[rrErrorApp1,rrErrorApp2,rrErrorApp3,rrErrorApp4]


x = np.array([0,1,2,3])
labels = ["App1","App2","App3","App4"]
width = 0.55
shiftA = 4
xticks = [width*2,shiftA+width*2,shiftA*2+width*2,shiftA*3+width*2]


ylim = [4, 4.3]
ylim2 = [0.0, 1.5]
ylimratio = (ylim[1] - ylim[0]) / (ylim2[1] - ylim2[0] + ylim[1] - ylim[0])
ylim2ratio = (ylim2[1] - ylim2[0]) / (ylim2[1] - ylim2[0] + ylim[1] - ylim[0])
gs = gridspec.GridSpec(2, 1, height_ratios=[ylimratio, ylim2ratio])

fig = plt.figure(figsize=(20, 12))
ax = fig.add_subplot(gs[0])
ax2 = fig.add_subplot(gs[1])

shiftx = 0
for ap in x:
    shiftx = shiftA *ap
    for i in range(len(rrs[ap])):
        acc = shiftx+(width*i)
        label = name[i]
        if ap>=1: label = None
        ax.bar(acc, rrs[ap][i],yerr=errs[ap][i] ,width=width, label=label,color=color[i])
        ax2.bar(acc, rrs[ap][i], yerr=errs[ap][i] ,width=width, label=label,color=color[i])
        ax.annotate('%0.2f'%rrs[ap][i],
              xy=(acc, rrs[ap][i]),
              xytext=(1,2),  # 3 points vertical offset
              textcoords="offset points",
              ha='center', va='bottom',
              fontsize=18)

        ax2.annotate('%0.2f'%rrs[ap][i],
              xy=(acc, rrs[ap][i]),
              xytext=(1,2),  # 3 points vertical offset
              textcoords="offset points",
              ha='center', va='bottom',
              fontsize=18)

ax.set_ylim(ylim)
ax2.set_ylim(ylim2)

ax.spines['bottom'].set_visible(False)
ax2.spines['top'].set_visible(False)
ax2.xaxis.tick_bottom()
ax.set_xticks([])

# Add some text for labels, title and custom x-axis tick labels, etc.
# ax2.set_ylabel('Response time',fontsize=32)




# ax.set_title('Response time by Experiment and App',fontsize=35)
ax2.set_xticks(xticks)
ax2.set_xticklabels(labels,fontsize=30)


ax2.hlines(y=1,xmin=0.5,xmax=13.5,color="gray",linewidth=5)
ax2.hlines(y=1,xmin=0,xmax=0.5,color="gray",linewidth=5,linestyles="--")
ax2.hlines(y=1,xmin=13.5,xmax=14,color="gray",linewidth=5,linestyles="--")

ax2.text(7.5,1.05,"under provisioned",color="gray",fontsize=24)
ax2.text(7.5,0.92,"over provisioned",color="gray",fontsize=24)


for tick in ax2.get_xticklabels():
    tick.set_fontsize(28)
for tick in ax2.get_yticklabels():
    tick.set_fontsize(24)

for tick in ax.get_yticklabels():
    tick.set_fontsize(24)

ax.legend(ncol=1,loc=1,prop={'size': 20},framealpha=0.6)

kwargs = dict(color='k', clip_on=False)
xlim = ax.get_xlim()
dx = .02 * (xlim[1] - xlim[0])
dy = .01 * (ylim[1] - ylim[0]) / ylimratio
ax.plot((xlim[0] - dx, xlim[0] + dx), (ylim[0] - dy, ylim[0] + dy), **kwargs)
ax.plot((xlim[1] - dx, xlim[1] + dx), (ylim[0] - dy, ylim[0] + dy), **kwargs)
dy = .01 * (ylim2[1] - ylim2[0]) / ylim2ratio
ax2.plot((xlim[0] - dx, xlim[0] + dx), (ylim2[1] - dy, ylim2[1] + dy), **kwargs)
ax2.plot((xlim[1] - dx, xlim[1] + dx), (ylim2[1] - dy, ylim2[1] + dy), **kwargs)

ax.set_xlim(xlim)
ax2.set_xlim(xlim)



#ax.set_ylim(0,400)
#plt.ylabel('Response time', multialignment='center',fontsize=32)
fig.text(0.04, 0.5, r"Service Usage", va="center", rotation="vertical", fontsize=32)
fig.tight_layout()
plt.show()
fig.savefig("bar_service_usage_large.pdf", dpi=400)

