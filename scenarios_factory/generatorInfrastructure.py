# -*- coding: utf-8 -*-
import networkx as nx
import matplotlib.pyplot as plt
import numpy as np
import json

# 1..4 Steps to create an Infrastructure
number_nodes = 100
# 5    Configure App TODO
# 6    Create users
number_users = 30



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
pos = nx.kamada_kawai_layout(G)
# pos = nx.spectral_layout(G)
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
values = (("(3,3)|"*len(G.nodes())).split("|"))[:-1]
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
normalizedY = (future_draw_dim * (loc[:, 0] - minYvalue) / (maxYvalue - minYvalue)).astype(int)
inlabels = ["(%i,%i)"%(normalizedX[x],normalizedY[x]) for x in range(len(normalizedX))]
posTopo = dict(zip(G.nodes(),inlabels))
nx.set_node_attributes(G, name='pos', values=posTopo)


# =============================================================================
# 3
# JSON GENERATOR
# Note: YAFS support directaly the use of "G": t.G = G
# =============================================================================

net = {}
net['entity']=nx.node_link_data(G)["nodes"]
net['link']=nx.node_link_data(G)["links"]
with open("networkDefinition.json","w") as f:
    f.write(json.dumps(net))

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


# {
#     "id_resource": 6,
#     "app": 1,
#     "message": "M.USER.APP.1",
#     "lambda": 20,
#     "start": 20,
#     "constraint": {"deadline": 500, "cost": 300}
# },