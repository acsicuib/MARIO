
import networkx as nx
import json
from yafs.topology import *

# G = nx.cycle_graph(10)
# print(G.nodes)
# print(G.edges(0))
# nadj = [e for e in G.edges(0)]
# print(nadj)

experiment_path = "../scenarios/prototype1/"
t = Topology()
# dataNetwork = json.load(open(path + 'networkDefinition.json'))
dataNetwork = json.load(open(experiment_path + 'networkDefinition.json'))
t.load_all_node_attr(dataNetwork)
nx.draw(t.G,with_labels=True)
plt.show()

src = 0
dst = 1
print(nx.shortest_path(t.G,src,dst))

print(t.G.edges(2))
for e in t.G.edges(2):
    print("EDGE:",e)


lat = nx.get_edge_attributes(t.G,"PR")
print(lat)
# data = json.load(open('exp1/networkDefinition.json'))
#
# G = nx.Graph()
# for edge in data["link"]:
#     G.add_edge(edge["s"], edge["d"],BW=edge["BW"],PR=edge["PR"])
#
# # ok
# print len(G.nodes)
#
# minPath = nx.shortest_path(G, source=0, target=3)
# print "Min path %s"%minPath
#
#
# for path in nx.all_simple_paths(G, source=0, target=9,cutoff=len(minPath)):
#     print(path)
#
# # 0 4 7 9
#
# for path in nx.all_simple_paths(G, source=0, target=3,cutoff=len(minPath)):
#     print(path)
#
# # 0 4 7 3
#
# print G[0][15]
#
# from itertools import islice
# def k_shortest_paths(G, source, target, k, weight=None):
#     return list(islice(nx.shortest_simple_paths(G, source, target, weight=weight), k))
#
# # DE PM! LO Ordena de menor a menos segun el weight
#
# for path in k_shortest_paths(G, 0, 9, 10,"BW"):
#     bw = 0
#     pr = 0
#     for i in range(len(path)-1):
# #        print path[i],path[i+1]
# #        print G[path[i]][path[i+1]]
#         bw +=G[path[i]][path[i+1]]["BW"]
#         pr +=G[path[i]][path[i+1]]["PR"]
#     print path,"  BW:%i   PR:%i"%(bw,pr)
#
#
# #    it = iter(path)
# #    for x in it:
# #        print x,next(it)