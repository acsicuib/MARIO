import networkx as nx
import matplotlib.pyplot as plt

# Returns a 𝐺𝑛,𝑝 random graph, also known as an Erdős-Rényi graph or a binomial graph.
G = nx.gnp_random_graph(10, 0.5, seed=10, directed=False)
pos = nx.kamada_kawai_layout(G)
nx.draw(G, pos)
plt.show()

#Returns a random graph according to the Barabási–Albert preferential attachment model.

G = nx.barabasi_albert_graph(10, 2, seed=10)
pos = nx.kamada_kawai_layout(G)
nx.draw(G, pos)
plt.show()


# Returns an random graph based on the specified kernel.
def integral(u, w, z):
    return c * (z - w)
def root(u, w, r):
    return r / c + w
c = 4
G = nx.random_kernel_graph(10, integral, root)
pos = nx.kamada_kawai_layout(G)
nx.draw(G, pos)
plt.show()

# Returns a Watts–Strogatz small-world graph.
G = nx.watts_strogatz_graph(10, 5, 0.2, seed=None)
pos = nx.kamada_kawai_layout(G)
nx.draw(G, pos)
plt.show()


# Generate a Gaussian random partition graph.
# A Gaussian random partition graph is created by creating k partitions each with a size drawn from a normal distribution with mean s and variance s/v. Nodes are connected within clusters with probability p_in and between clusters with probability p_out[1]
# n (int) – Number of nodes in the graph
# s (float) – Mean cluster size
# v (float) – Shape parameter. The variance of cluster size distribution is s/v.
# p_in (float) – Probabilty of intra cluster connection.
# p_out (float) – Probability of inter cluster connection.
n = 100
s = 60
v = 0.3
p_in = 0.02
p_out = 0.01
G = nx.gaussian_random_partition_graph(n, s, v, p_in, p_out, directed=False, seed=10)
pos = nx.kamada_kawai_layout(G)
# pos = nx.spectral_layout(G)
nx.draw(G, pos)
plt.show()
