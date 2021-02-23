from yafs.selection import Selection
import networkx as nx
from collections import Counter


class DeviceSpeedAwareRouting(Selection):

    def __init__(self):
        super(DeviceSpeedAwareRouting, self).__init__()
        self.cache = {}
        self.freq_use_DES_module = Counter()
        self.invalid_cache_value = True

        self.controlServices = {}
        # key: a service
        # value : a list of idDevices

    def compute_BEST_DES(self, node_src, alloc_DES, sim, DES_dst, message):
        try:
            bestLong = float('inf')
            minPath = []
            bestDES = []
            candidateDES = []

            for dev in DES_dst:
                node_dst = alloc_DES[dev]
                path = list(nx.shortest_path(sim.topology.G, source=node_src, target=node_dst))
                long = len(path)

                if long < bestLong:
                    bestLong = long
                    minPath = path
                    bestDES = dev
                    candidateDES = []
                elif long == bestLong:
                    # Another instance service is deployed in the same node
                    # the path is the same
                    # the DES process is added
                    if len(candidateDES) == 0: #the first element
                        candidateDES.append(bestDES)
                    candidateDES.append(dev) #another one

            # ROUND ROBIN Schedule
            # There are two or more options in a node:
            if len(candidateDES) > 0:
                #we update the list
                for des in candidateDES:
                     if des not in self.freq_use_DES_module:
                         self.freq_use_DES_module[des]=0

                #from the global list. we get only candidateDES values.
                filterCandidateDES = Counter({x: self.freq_use_DES_module[x] for x in self.freq_use_DES_module if x in candidateDES})

                DES_less_used = min(filterCandidateDES,
                                    key=filterCandidateDES.get)  # A round robing assignment
                self.freq_use_DES_module[DES_less_used] += 1
                return minPath, DES_less_used

            else:
                self.freq_use_DES_module[bestDES] += 1
                return minPath, bestDES

        except (nx.NetworkXNoPath, nx.NodeNotFound) as e:
            self.logger.warning("There is no path between two nodes: %s - %s " % (node_src, node_dst))
            # print("Simulation must ends?)"
            return [], None

    def get_path(self, sim, app_name, message, topology_src, alloc_DES, alloc_module, traffic, from_des):
        # Entity where the requests comes
        node_src = topology_src
        # Service Name
        service = message.dst

        # List of DES-modules that they can respond to the message
        DES_dst = alloc_module[app_name][message.dst]
        # print("Enrouting from SRC: %s  -<->- DES %s"%(str(node_src),DES_dst))
        # print("FROM DES ",from_des)
        # print("SERVICE ",service)

        # There is a cache to store the requests.
        # The cache is very sensitive. It depends on:
        # - idDES user: from_des
        # - node where it the user: node_src
        # - other DES modules that can respond to the message (DES_dst)
        if (from_des, node_src, tuple(DES_dst)) not in self.cache.keys():
            path, des = self.compute_BEST_DES(node_src, alloc_DES, sim, DES_dst, message)
            self.cache[(from_des, node_src, tuple(DES_dst))] = path, des

        else:
            path, des = self.cache[(from_des, node_src, tuple(DES_dst))]
        self.controlServices[(from_des,node_src, service)] = (path, des)

        return [path], [des]

    def get_path_from_src_dst(self,sim, node_src, node_dst):
        path = list(nx.shortest_path(sim.topology.G, source=node_src, target=node_dst))
        return path


    def clear_routing_cache(self):
        self.invalid_cache_value = False
        self.cache = {}
        self.freq_use_DES_module = Counter()
        self.controlServices = {}

    def get_path_from_failure(self, sim, message, link, alloc_DES, alloc_module, traffic, ctime, from_des):
        # print "Example of enrouting"
        # print message.path # [86, 242, 160, 164, 130, 301, 281, 216]
        # print message.dst_int  # 301
        # print link #(130, 301) link is broken! 301 is unreacheble

        idx = message.path.index(link[0])
        # print "IDX: ",idx
        if idx == len(message.path):
            # The node who serves ... not possible case
            return [], []
        else:
            node_src = message.path[idx]  # In this point to the other entity the system fail
            # print "SRC: ",node_src # 164

            node_dst = message.path[len(message.path) - 1]
            # print "DST: ",node_dst #261
            # print "INT: ",message.dst_int #301

            path, des = self.get_path(sim, message.app_name, message, node_src, alloc_DES, alloc_module, traffic,
                                      from_des)
            if len(path[0]) > 0:
                # print path # [[164, 130, 380, 110, 216]]
                # print des # [40]

                concPath = message.path[0:message.path.index(path[0][0])] + path[0]
                # print concPath # [86, 242, 160, 164, 130, 380, 110, 216]
                newINT = node_src  # path[0][2]
                # print newINT # 380

                message.dst_int = newINT
                return [concPath], des
            else:
                return [], []
