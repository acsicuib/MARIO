""""

+ It tests the topology

+ it generates a json data to visualize on CESIUM (./Cesium_viewer/index.html).
    CESIUM DATA:
    - Generate topology nodes and edges
    - Generate users according with its movement
    - Generate links between users and nodes
    - Copy and paste "prints" in the html

+ It computes user mobility indicators in the topology according with the projection

"""

import numpy as np
import networkx as nx
from matplotlib import pyplot as plt
import trackanimation
import collections

def generateAllLevels(size):
    ls = [size]
    v = size // 2
    while v >= 2:
        ls.append(v)
        v = v // 2
    ls.append(0)
    return ls


def isInHier2D(value, levels):
    for level in levels[:-1]:
        for posX, x in enumerate(np.linspace(0, 1, int(level) + 1)):
            if x > value[0]:
                break
        for posY, x in enumerate(np.linspace(0, 1, int(level) + 1)):
            if x > value[1]:
                break
        yield (level, posX - 1, posY - 1)
    yield (0, 0, 0)


def getListEdges(levels):
    levels = np.array(levels)
    edges = set()
    levelNode = {}

    for idx in np.linspace(0, 1, levels[0] + 1):
        for idy in np.linspace(0, 1, levels[0] + 1):
            ed = list(isInHier2D([idx, idy], ls))
            # print(ed)
            p = 0
            levelC, nodeCX, nodeCY = ed[p]
            levelC = np.where(levelC == levels[::-1])[0][0]
            while p < len(ed) - 1:
                p += 1
                levelD, nodeDX, nodeDY = ed[p]
                levelD = np.where(levelD == levels[::-1])[0][0]
                nameC = "%i#%i-%i" % (levelC, nodeCX, nodeCY)
                nameD = "%i#%i-%i" % (levelD, nodeDX, nodeDY)

                levelNode[nameC] = levelC

                edges.add((nameC, nameD))

                levelC, nodeCX, nodeCY = levelD, nodeDX, nodeDY
                # print((nameC,nameD))

        #    break
        # break

    return edges, levelNode


def generateProjection(projection, nodes, levels):
    lat, lng = {}, {}
    minLat, maxLat = projection[0][0], projection[1][0]
    minLng, maxLng = projection[0][1], projection[1][1]
    for il, l in enumerate(levels[::-1]):
        level = l + l + 1
        for ix, posX in enumerate(np.linspace(minLat, maxLat, level)[1::2]):
            for iy, posY in enumerate(np.linspace(minLng, maxLng, level)[1::2]):
                name = "%i#%i-%i" % (il, ix, iy)
                lat[name] = posX
                lng[name] = posY

    name = "%i#%i-%i" % (0, 0, 0)
    lat[name] = minLat + (maxLat - minLat) / 2
    lng[name] = minLng + (maxLng - minLng) / 2
    return lat, lng



def currentMovement(df, step):
    tt = df[df.VideoFrame == step]
    coordinates = {}
    for row in tt.iterrows():
        code = str(row[1]["CodeRoute"])
        lat = row[1]["Latitude"]
        lng = row[1]["Longitude"]
        coordinates[code] = (lat, lng)
    return coordinates

def namenode(level,px,py):
        # return "n%ilt%iln%i"%(level,px,py)
        return "%i#%i-%i"%(level,px,py)

def getClosedNode(point,projection,levels):
        """
        :param point:
        :return:
        The name of most closed node of the last level
        """
        pn = [(point[0] - projection[0][0]) / (projection[1][0] - projection[0][0]),
              (point[1] - projection[0][1]) / (projection[1][1] - projection[0][1])]

        level = levels[:-1][0]
        for posX,x in enumerate(np.linspace(0, 1, int(level)+1)):
            if x>pn[0]:
                if posX==0: posX=1
                break
        for posY,x in enumerate(np.linspace(0, 1, int(level)+1)):
            if x>pn[1]:
                if posY == 0: posY = 1
                break

        return namenode(len(levels)-1,posX-1,posY-1) #lastlevel



size = 16
ls = generateAllLevels(size)
edges,levelNode = list(getListEdges(ls))

G = nx.Graph()
G.add_edges_from(edges)

#Rome city
projection = [[41.878037, 12.4462643], [41.919234,12.5149603]]
attLat,attLng = generateProjection(projection, G.nodes(), ls)

nx.set_node_attributes(G,name="level",values=levelNode)
nx.set_node_attributes(G, name="latitude", values=attLat)
nx.set_node_attributes(G, name="longitude", values=attLng)

# tracks = trackanimation.read_track("multi-agent-policies/scenarios/TaxiRome/results_20201028/normalized_trajectories.csv")
tracks = trackanimation.read_track("results_P1_20201028/normalized_trajectories.csv")



# =============================================================================
# Generación de puntos y edges para CESIUM_TEST project
# =============================================================================
nodesCESIUM = [
     {"longitude":attLng[x], "latitude":attLat[x],
      "level":int(x.split("#")[0])} for x in attLat]

edgesCESIUM = [{"s":list(attLat.keys()).index(x),
       "t":list(attLat.keys()).index(y)}
       for x,y in edges]


## Management the movement of traces

# Generating JSON DATA to show it on CESIUM VIEWER
total_steps  = tracks.df.VideoFrame.max() #
total_steps  = 10 #
for current_step in range(total_steps):
    track_code_last_position = currentMovement(tracks.df,current_step)

    edgesTaxis = []
    nodesTaxis = []

    for code in track_code_last_position:
        (lat, lng) = track_code_last_position[code]
        point = [lat, lng]
        new_node = getClosedNode(point, projection, ls)
        nodesTaxis.append(
                    {"longitude": point[1], "latitude": point[0], "code":code})
        for posNode in range(len(attLat)):
            if list(attLat.keys())[posNode] == new_node:
                break
        edgesTaxis.append({"s": posNode, "t": len(nodesTaxis) - 1})

    print("STEP : %i"%current_step)
    print("const points = %s"%nodesCESIUM)
    print("const edges = %s"%edgesCESIUM)
    print("const taxis = %s"%nodesTaxis)
    print("const links = %s"%edgesTaxis)





# Generating INDICATORS from trajectories
# - total number of connections for each user
# - total aggr. and average number of diff connections.
prev_node = {}
total_steps  = tracks.df.VideoFrame.max() #
user = collections.defaultdict(list)
for current_step in range(total_steps):
    track_code_last_position = currentMovement(tracks.df,current_step)
    # Get the last user position
    for code in track_code_last_position:
        (lat, lng) = track_code_last_position[code]
        point = [lat, lng]
        new_node = getClosedNode(point, projection, ls)

        if code in prev_node:
            if new_node != prev_node[code]:
                user[code].append(new_node)
        else:
            user[code].append(new_node)

        prev_node[code] = new_node


print("User connections",user)

counter = {u:len(user[u]) for u in user}
hist = np.array(list(counter.values()))
print("Number of users: ",len(counter))
print("Average number of movements by user: %i"%hist.mean())
print("Desviation number of movements by user: %i"%hist.std())

fig, ax = plt.subplots()
plt.hist(hist)
start, end = ax.get_xlim()
ax.xaxis.set_ticks(np.arange(start, end, 1))
ax.set_xticklabels(list(range(0,hist.max())))
plt.title("Histogram of user different connections", fontsize=14)
plt.xlabel(r"Connections", fontsize=14)
plt.ylabel(r"Users", fontsize=14)
fig.savefig("hist_user_movements.pdf", dpi=400)
plt.show()


