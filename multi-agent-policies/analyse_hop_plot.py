import numpy as np
import pandas as pd
from scipy import stats
import networkx as nx
from yafs.topology import Topology
import json
from  collections import defaultdict
from matplotlib import colors
import matplotlib as mpl
import matplotlib.pyplot as plt


# Color palette from MARIO render style
total_apps = 6
tab20 = plt.cm.get_cmap('tab20',total_apps+5)
bounds = range(total_apps+5)
newcolors = tab20(np.linspace(0, 1, total_apps+5))
newcolors[0] = np.array([250.0 / 256.0, 250. / 256., 250. / 256., 1])
newcmp = mpl.colors.ListedColormap(newcolors)
##

# Parameters
timeSimulation = 20000

sampling = 1000 # aggregation periods of messages

experiment_path = "scenarios/FOCLASA2020/policy_getcloser/"
experiment_path = "scenarios/FOCLASA2020/policy_eco_getcloser/"
# experiment_path = "scenarios/FOCLASA2020/policy_turbo_getcloser/"
# experiment_path = "scenarios/FOCLASA2020/policy_getclosers_I_II_III/"


# Load the simulation trace
df = pd.read_csv(experiment_path + "results/Results_%s_%s_%i.csv" % ("prot1", timeSimulation, 0))
df.index = df["time_out"].astype('datetime64[s]')

# =============================================================================
# VERSION 1
# =============================================================================
# =============================================================================
# #Get the node.src and node.dst resampling the samples and we get the most frequent values.
# =============================================================================

df2 = df.groupby('app')["TOPO.src","TOPO.dst"].resample("%ss"%sampling).agg(lambda x:x.value_counts().index[0])
df2.reset_index(inplace=True)


# We need the infrastructure to compute the path between the nodes
t = Topology()
dataNetwork = json.load(open(experiment_path + 'networkDefinition.json'))
t.load_all_node_attr(dataNetwork)

app_hop = defaultdict(list)
for row in df2.iterrows():
    idApp = row[1]["app"]
    src = row[1]["TOPO.src"]
    dst = row[1]["TOPO.dst"]
    path = list(nx.shortest_path(t.G, source=src, target=dst))
    app_hop[idApp].append(len(path)-1)

print(app_hop)

fig, ax = plt.subplots()
for k in app_hop:
    plt.plot(app_hop[k],color=newcmp(k))
plt.title('Hops')
plt.xlabel(r"Simulation periods (%i time units)"%sampling)
plt.ylabel(r"Hop")
# plt.xticks(np.arange(0, len(app_hop[k]), 1))
# plt.yticks(np.arange(0, 7), 1)
fig.savefig(experiment_path+"results/hops.png", dpi=300)        


app_hop=np.array(list(app_hop.values()))
print(app_hop)
fig, ax = plt.subplots()
ax.boxplot(app_hop)
plt.title('Hops')
plt.xlabel(r"Simulation periods (%i time units)"%sampling)
plt.ylabel(r"Hop")
# plt.xticks(np.arange(0, len(app_hop[k]), 1))
# plt.yticks(np.arange(0, 7), 1)
fig.savefig(experiment_path+"results/boxhops.png", dpi=300)        




# =============================================================================
# VERSION 2
# =============================================================================
#col_hops = []
#app_hop = defaultdict(dict)
#for row in df.iterrows():
#    idApp = row[1]["app"]
#    sid = row[1]["DES.dst"]
#    src = row[1]["TOPO.src"]
#    dst = row[1]["TOPO.dst"]
#    path = list(nx.shortest_path(t.G, source=src, target=dst))
#    col_hops.append(len(path))
#
#
#print(col_hops)
#df["hops"] = col_hops
#
#
##sampling = 1000 #,"DES.dst":"mean" .resample("%ss"%sampling)
##DES.dst is the ID of the service instance
##df2 = df.groupby('app').resample("%ss"%sampling).agg({"hops":['mean','std'],"DES.dst": lambda x: ''.join(x)})
#df2 = df.groupby(['app','DES.dst', pd.Grouper(freq = '1000s')]).agg({'hops':[np.mean, np.std]})
#df2.reset_index(inplace=True)
