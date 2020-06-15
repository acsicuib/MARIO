# -*- coding: utf-8 -*-
import networkx as nx
import matplotlib.pyplot as plt
import numpy as np
import json
import sys
from pathlib import Path
from matplotlib import colors
import matplotlib as mpl
from PIL import Image

# 1..4 Steps to create an Infrastructure
number_nodes = 100
# 5    Configure App TODO
# 6    Create users
number_users = 30


def get_nodes_with_services(G):
    """
       Based on the render of app_operator.py for visual inspection


    :param sim:
    :return:
    """
    HwReqs = nx.get_node_attributes(G, name="HwReqs")
    currentOccupation = {}
    for n in G.nodes:
        currentOccupation[n] = np.zeros(int(HwReqs[n])).astype(int)

    # for app in sim.alloc_module:
    #     dict_module_node = sim.alloc_module[app]  # modules deployed
    #     for module in dict_module_node:
    #         for des in dict_module_node[module]:
    #             indx = list(currentOccupation[sim.alloc_DES[des]]).index(0)
    #             currentOccupation[sim.alloc_DES[des]][indx] = app

    shape = nx.get_node_attributes(G, name="shape")
    for node in currentOccupation:
        try:
            currentOccupation[node] = np.array(currentOccupation[node]).reshape(eval(shape[node]))
        except ValueError:
            raise "Network node: %i defined with a bad shape " % node
            # print("Network node: %i defined with a bad shape "%node)
            # currentOccupation[node] = np.zeros(shape(1,1))

    return currentOccupation

def render(G,posNorm,total_services=2):
    """
    Based on the render of app_operator.py for visual inspection

    :param G:
    :return:
    """
    pos = posNorm


    tab20 = plt.cm.get_cmap('tab20', total_services + 5)
    bounds = range(total_services + 5)
    newcolors = tab20(np.linspace(0, 1, total_services + 5))
    newcolors[0] = np.array([250.0 / 256.0, 250. / 256., 250. / 256., 1])
    newcmp = mpl.colors.ListedColormap(newcolors)
    norm = mpl.colors.BoundaryNorm(bounds, newcmp.N)

    fig, ax = plt.subplots(figsize=(16.0, 10.0))

    nx.draw(G, pos, with_labels=False, node_size=1, node_color="#1260A0", edge_color="gray",
            node_shape="o",
            font_size=7, font_color="white", ax=ax)

    width = ax.get_xlim()[1]
    top = ax.get_ylim()[1]

    # Labels on nodes
    for x in G.nodes:
        ax.text(pos[x][0] + (width / 45), pos[x][1] + (width / 35), "N%i" % (x), fontsize=10)

    piesize = .02
    p2 = piesize / 4.5

    # LAST step:
    # Displaying capacity, changing node shape
    trans = ax.transData.transform
    trans2 = fig.transFigure.inverted().transform
    data_occupation = get_nodes_with_services(G)
    for n in G.nodes():
        xx, yy = trans(pos[n])  # figure coordinates
        xa, ya = trans2((xx, yy))  # axes coordinates
        a = plt.axes([xa - p2, ya - p2, piesize, piesize])
        a.set_aspect('equal')
        a.imshow(data_occupation[n], cmap=newcmp, interpolation='none', norm=norm)
        a.axes.get_yaxis().set_visible(False)
        a.axes.get_xaxis().set_visible(False)

    # plt.text(2, 1000, "Step: %i" % self.activation, {'color': 'C0', 'fontsize': 16})

    canvas = plt.get_current_fig_manager().canvas
    canvas.draw()
    pil_image = Image.frombytes('RGB', canvas.get_width_height(), canvas.tostring_rgb())
    pil_image.save("network_test.png")
    plt.close(fig)


# =============================================================================
#  1
#  THE NETWORK MODEL
#  Note: Which model?
# 
# =============================================================================

# =============================================================================
# Returns a 𝐺𝑛,𝑝 random graph, also known as an Erdős-Rényi graph or a binomial graph.
# =============================================================================
# G = nx.gnp_random_graph(10, 0.5, seed=10, directed=False)
# pos = nx.kamada_kawai_layout(G)
# nx.draw(G, pos)
# plt.show()
# =============================================================================
# Returns a random graph according to the Barabási–Albert preferential attachment model.
# =============================================================================
# G = nx.barabasi_albert_graph(10, 2, seed=10)
# pos = nx.kamada_kawai_layout(G)
# nx.draw(G, pos)
# plt.show()

# =============================================================================
# Returns an random graph based on the specified kernel.
# =============================================================================
# def integral(u, w, z):
#     return c * (z - w)
# def root(u, w, r):
#     return r / c + w
# c = 4
# G = nx.random_kernel_graph(10, integral, root)
# pos = nx.kamada_kawai_layout(G)
# nx.draw(G, pos)
# plt.show()

# =============================================================================
# Returns a Watts–Strogatz small-world graph.
# =============================================================================
# G = nx.watts_strogatz_graph(10, 5, 0.2, seed=None)
# pos = nx.kamada_kawai_layout(G)
# nx.draw(G, pos)
# plt.show()

# =============================================================================
# Generate a Gaussian random partition graph.
# =============================================================================
# A Gaussian random partition graph is created by creating k partitions each with a size drawn from a normal distribution with mean s and variance s/v. Nodes are connected within clusters with probability p_in and between clusters with probability p_out[1]
# n (int) – Number of nodes in the graph
# s (float) – Mean cluster size
# v (float) – Shape parameter. The variance of cluster size distribution is s/v.
# p_in (float) – Probabilty of intra cluster connection.
# p_out (float) – Probability of inter cluster connection.
n = number_nodes
s = 60
v = 0.3
p_in = 0.02
p_out = 0.01
G = nx.gaussian_random_partition_graph(n, s, v, p_in, p_out, directed=False, seed=10)
# pos = nx.kamada_kawai_layout(G,scale=210)

pos1 = nx.kamada_kawai_layout(G)
pos = nx.spring_layout(G,k=1.2,pos=pos1,scale=2.0)
nx.draw(G, pos)
plt.show()


# Better vis. with Gephi
nx.write_gexf(G,"sample.gexf")



# =============================================================================
#  2
#  Network/Infrastructure attributes
#  - Node capacity
#  - Node speed
#  - Links latency and bandwidth
# =============================================================================


# - Links latency and bandwidth
valuesOne = dict(zip(G.edges(), np.ones(len(G.edges()))))
print(valuesOne)
nx.set_edge_attributes(G, name='BW', values=valuesOne)
nx.set_edge_attributes(G, name='PR', values=valuesOne)

# "HwReqs" node attribute,
capacity_size = 6
capacity = dict(zip(G.nodes(), np.ones(len(G.nodes()))*capacity_size))
print(capacity)
nx.set_node_attributes(G, name='HwReqs', values=capacity)


# "IPT": 1000,
ipt = 1000
iptvalues= dict(zip(G.nodes(), np.ones(len(G.nodes()))*ipt))
nx.set_node_attributes(G, name='IPT', values=iptvalues)

# SHAPE node attribute "shape": "(4,2)",
#if capacity is 6, then (3,3) > current version: manual
values = (("(2,3)|"*len(G.nodes())).split("|"))[:-1]
shapes = dict(zip(G.nodes(),values))
nx.set_node_attributes(G, name='shape', values=shapes)

# POS attribute - "pos": "(4,4)"
loc = np.array(list(pos.values()))
minXvalue = loc[:,0].min()
maxXvalue = loc[:,0].max()
minYvalue = loc[:,1].min()
maxYvalue = loc[:,1].max()
future_draw_dim = 150
normalizedX = (future_draw_dim * (loc[:, 0] - minXvalue) / (maxXvalue - minXvalue)).astype(int)
normalizedY = (future_draw_dim * (loc[:, 1] - minYvalue) / (maxYvalue - minYvalue)).astype(int)

inlabels = ["(%i,%i)"%(normalizedX[x],normalizedY[x]) for x in range(len(normalizedX))]
posTopo = dict(zip(G.nodes(),inlabels))
posNorm = dict(zip(G.nodes(),zip(normalizedX,normalizedY)))
nx.set_node_attributes(G, name='pos', values=posTopo)


# =============================================================================
# 3
# JSON GENERATOR
# Note: YAFS support directaly the use of "G": t.G = G
# =============================================================================

net = {}
net['entity']=nx.node_link_data(G)["nodes"]
net['link']=nx.node_link_data(G)["links"]

#Rename some keys

for link in net['link']:
    link["s"] = link["source"]
    link["d"] = link["target"]
    del link["source"]
    del link["target"]

with open("networkDefinition.json","w") as f:
    f.write(json.dumps(net))


print("HERE")
print(posNorm)
render(G,posNorm)

# =============================================================================
# 5
# Application generator
# =============================================================================

# TODO


# =============================================================================
# 6
# User generator
# =============================================================================
degrees = sorted(G.degree, key=lambda x: x[1], reverse=False)
# degreeCount = collections.Counter([x[1] for x in degrees])
# deg, cnt = zip(*degreeCount.items())
# assert deg[0]!=0 #Nodes with 0-edges

users = {}
users["sources"] = []

# Users will randomly assign to nodes with lower degree values
assert number_users<len(degrees)
for idx in range(number_users):
    print("User_%i on node %i" % (idx, degrees[idx][0]))
    users["sources"].append({
        "id_resource": degrees[idx][0],
        "app": 1,
        "message": "M.USER.APP.1",
        "lambda": 100,
        "start": 20,
        "constraint": {"deadline": 500, "cost": 300}
    })
with open("usersDefinition.json","w") as f:
    f.write(json.dumps(users))