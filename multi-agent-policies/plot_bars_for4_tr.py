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



# LARGE RESULTS - v2_rules
trApp1 = [4383.879456,2.026667,615.572265,25.258852,24.580037]
trApp2 = [4572.884368,2.061214,943.224955,12.370833,12.861402]
trApp3 = [4740.899852,2,31.03172,30.638151,31.533847]
trApp4 = [4328.183981,2,28.225854,20.309216,19.183897]

# trApp1 = [4383.879456,2.026667,	134.160276,	34.18038,	33.246728]
# trApp2 = [4572.884368,2.061214,	178.73121,	19.131202,	19.385418]
# trApp3 = [4740.899852,2,	40.459626,	40.231319,	41.078351]
# trApp4 = [4328.183981,2,	67.107806,	29.459574,	28.579461]
trs=[trApp1,trApp2,trApp3,trApp4]


rrApp1 =[23.006929,0,120.596243,0.195365,0.181481]
rrApp2 =[35.754721,0.002259,175.140657,0.246434,0.275673]
rrApp3 =[12.499409,0,0.22024,0.168769,0.163437]
rrApp4 =[196.930872,0,1.30831,0.844212,0.729197]


# rrApp1 =[23.006929,	0,	104.241085,	0.206933,	0.510009]
# rrApp2 =[35.754721,	0.002259,	178.842321,	0.239252,	0.20849]
# rrApp3 =[12.499409,	0,	0.102611,0.134708,0.132616]
# rrApp4=[196.930872,	0,	62.095822,0.385926,0.697008]

# LARGE RESULTS - v0
# trApp1 = [4400.87,2.03,234.05,34.29,33.49]
# trApp2 = [4595.39,2.06,339.79,19.30,19.56]
# trApp3 = [4719.99,2,40.48,40.18,41.04]
# trApp4 = [4267.15,2,36.96,28.81,27.82]
#
# trs=[trApp1,trApp2,trApp3,trApp4]
#
# rrApp1 = [0.750,0.417,0.684,0.710,0.819]
# rrApp2 = [1.100,4.170,0.579,0.855,0.834]
# rrApp3 = [0.589,0.042,0.547,0.634,0.753]
# rrApp4 = [1.225,0.083,0.691,0.549,0.484]

etrs=[rrApp1,rrApp2,rrApp3,rrApp4]

# # LARGE RESULTS - 5 simulations
# trApp1 = [4383.879456,2.026667,28.135372, 45.632445,43.312256]
# trApp2 = [4572.884368,2.061214,12.374072, 24.438898,21.912164]
# trApp3 = [4740.899852,2,35.831297,  44.570983,41.716388]
# trApp4 = [4328.183981,2,29.978552,  30.568537,32.568585]
#
#
# trErrorApp1 = [23.006929,0,0.494086 ,0.257047,0.146649]
# trErrorApp2 = [35.754721,0.002259, 0.006424, 0.539357,0.169494]
# trErrorApp3 = [12.499409,0,0.083027, 0.178034,0.221560]
# trErrorApp4 = [196.930872,0,0.905966,1.813870,0.902954]


x = np.array([0,1,2,3])
labels = ["App1","App2","App3","App4"]
width = 0.55
shiftA = 4
xticks = [width*2,shiftA+width*2,shiftA*2+width*2,shiftA*3+width*2]

ylim = [500, 5300]
ylim2 = [0, 40]
ylimratio = (ylim[1] - ylim[0]) / (ylim2[1] - ylim2[0] + ylim[1] - ylim[0])

ylim2ratio = (ylim2[1] - ylim2[0]) / (ylim2[1] - ylim2[0] + ylim[1] - ylim[0])
print(ylimratio)
print(ylim2ratio)
gs = gridspec.GridSpec(2, 1, height_ratios=[ylim2ratio,ylim2ratio]) #TODO I change the order to have more ratio in the other axis

fig = plt.figure(figsize=(20, 12))
ax = fig.add_subplot(gs[0])
ax2 = fig.add_subplot(gs[1])


shiftx = 0
for ap in x:
    shiftx = shiftA *ap
    for i in range(len(trs[ap])):
        acc = shiftx+(width*i)
        label = name[i]
        if ap>=1: label = None
        ax.bar(acc, trs[ap][i], yerr=etrs[ap][i] ,width=width, label=label,color=color[i])
        ax2.bar(acc, trs[ap][i],  yerr=etrs[ap][i], width=width, label=label,color=color[i])
        ax.annotate('%0.0f'%trs[ap][i],
              xy=(acc, trs[ap][i]),
              xytext=(1,2),  # 3 points vertical offset
              textcoords="offset points",
              ha='center', va='bottom',
              fontsize=20)

        ax2.annotate('%0.0f'%trs[ap][i],
              xy=(acc, trs[ap][i]),
              xytext=(1,2),  # 3 points vertical offset
              textcoords="offset points",
              ha='center', va='bottom',
              fontsize=20)


ax.set_ylim(ylim)
ax2.set_ylim(ylim2)

ax.spines['bottom'].set_visible(False)
ax2.spines['top'].set_visible(False)
ax2.xaxis.tick_bottom()
ax.set_xticks([])

# Add some text for labels, title and custom x-axis tick labels, etc.
# ax2.set_ylabel('Response time',fontsize=32)




ax2.set_xticks(xticks)
ax2.set_xticklabels(labels,fontsize=30)


for tick in ax2.get_xticklabels():
    tick.set_fontsize(28)
for tick in ax2.get_yticklabels():
    tick.set_fontsize(24)

for tick in ax.get_yticklabels():
    tick.set_fontsize(24)


ax.legend(ncol=1,loc=4,prop={'size': 20},framealpha=0.6)

kwargs = dict(color='k', clip_on=False)
xlim = ax.get_xlim()

# dx = .02 * (xlim[1] - xlim[0])
# dy = .01 * (ylim[1] - ylim[0]) / ylimratio
# ax.plot((xlim[0] - dx, xlim[0] + dx), (ylim[0] - dy, ylim[0] + dy), **kwargs)
# ax.plot((xlim[1] - dx, xlim[1] + dx), (ylim[0] - dy, ylim[0] + dy), **kwargs)
# dy = .01 * (ylim2[1] - ylim2[0]) / ylim2ratio
# ax2.plot((xlim[0] - dx, xlim[0] + dx), (ylim2[1] - dy, ylim2[1] + dy), **kwargs)
# ax2.plot((xlim[1] - dx, xlim[1] + dx), (ylim2[1] - dy, ylim2[1] + dy), **kwargs)

ax.set_xlim(xlim)
ax2.set_xlim(xlim)


#ax.set_ylim(0,400)
#plt.ylabel('Response time', multialignment='center',fontsize=32)
fig.text(0.04, 0.5, r"Response time", va="center", rotation="vertical", fontsize=32)
fig.tight_layout()
plt.show()
fig.savefig("bar_response_time_large.pdf", dpi=400)

