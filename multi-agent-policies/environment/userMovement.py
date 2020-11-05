from collections import defaultdict
import random

from trackanimation.animation import AnimationTrack
from yafs.core import Sim
import logging
import numpy as np
import time
import copy
import networkx as nx
import random
from yafs.distribution import *

class UserControlMovement:


    def __init__(self,pathResults, doExecutionVideo, tiledTopo, users,listIdApps, logger=None):
        self.logger = logger or logging.getLogger(__name__)
        self.current_step = 0
        self.path_results = pathResults

        self.previous_connections = {}
        # self.name_endpoints = {}
        self.doExecutionVideo = doExecutionVideo
        self.tiledTopo = tiledTopo
        self.users = users
        self.listIdApps = listIdApps
        self.mapsUser = {}
        self.total_diff_connections = 0


    def summarize(self):
        print("Number of evolutions %i" % self.current_step)

    def deploy_module(self,sim,service,idtopo):
        app_name = service[0:service.index("_")]
        app = sim.apps[app_name]
        services = app.services
        idDES = sim.deploy_module(app_name, service, services[service], [idtopo])

    def currentMovement(self, df, step):
        tt = df[df.VideoFrame == step]
        coordinates = {}
        for row in tt.iterrows():
            code = str(row[1]["CodeRoute"])
            lat = row[1]["Latitude"]
            lng = row[1]["Longitude"]
            coordinates[code] = (lat, lng)
        return coordinates

    # def update_topology_connections(self,sim,code,id_node,routing):
    #     # ##type: (Sim, String, int/String, Selection) -> None   ### CREATING ONLY ONE EDGE
    #     # or
    #     # ##type: (Sim, String, [int/String], Selection) -> None ### CREATING MORE EDGE
    #     # or
    #     # ##type: (Sim, String, None , Selection) -> None ### REMOVING EDGE
    #     """
    #     It changes the topology connections
    #     Args:
    #         sim:
    #         code:
    #         id_node:
    #
    #     Returns:
    #
    #     """
    #     # 1 - remove previous edge (in case)
    #
    #     # print " Removing edges of node: %s" %code
    #     edge = ()
    #     #TODO Improve a dynamic assignament of BW / PR (edge link properties)
    #     att = {"BW": 10,"PR": 10}
    #     try:
    #         existNode = False
    #         edges_to_remove = [e for e in sim.topology.G.edges() if int(code) in e]
    #         for edge in edges_to_remove:
    #             att = sim.topology.G[edge[0]][edge[1]]
    #             sim.topology.G.remove_edge(*edge)
    #             existNode = True
    #
    #         if len(edges_to_remove)==0 and id_node != None:
    #             if code in sim.mobile_fog_entities:
    #                 sim.topology.G.add_node(int(code), level=-1, **sim.mobile_fog_entities[code]["node_attributes"])
    #
    #                 # TODO REMOVE IN VERSION 2
    #                 sim.topology.nodeAttributes[int(code)] = sim.topology.G.node[int(code)]
    #
    #             else:
    #                 sim.topology.G.add_node(int(code), level=-1)
    #     except nx.NetworkXError:
    #         # The edge was not established, first time:
    #         # print "Does not exist: %s"%code
    #         None
    #         # sim.topology.G.add_node(code,level=-1)
    #
    #
    #     # 2 - add new edge
    #     # print "a new link between nodes: %s -> %s"%(code,id_node)
    #     if id_node!= None:
    #         if type(id_node)==list:
    #             for id_n in id_node:
    #                 sim.topology.G.add_edge(int(code), int(id_n), **att)
    #         else:
    #             sim.topology.G.add_edge(int(code), int(id_node), **att)
    #
    #     # 3 we can interact with other own classes:
    #     routing.invalid_cache_value = True # we can invalid the cache of routing packages


    def __call__(self, sim, routing,case, stop_time, it):
        """
        It updates network topology in function of user location and mobile agents in the scenario

        Args:
            sim: Simulator
            routing: selection strategy
            case: name of the simulation to show results
            stop_time: finalization time of the simulation
            it: int to identify the iteration

        Returns:
            None
        """
        self.logger.info("Movement number (#%i) at time: %i" % (self.current_step, sim.env.now))
        start_time = time.time()

        ##
        # TEST: for debug control !
        # It takes a snapshot of a specific movement
        # if self.current_step == 0:
        # self.animation = AnimationTrack(sim, dpi=100, bg_map=True, aspect='equal')



        # Get the coordinate of each user in this step
        track_code_last_position = self.currentMovement(sim.user_tracks.df,self.current_step)

        # Updating the last user position
        new_current_connection = {}
        # Get the last user position
        for code in track_code_last_position:
            (lat, lng) = track_code_last_position[code]
            point = [lat, lng]
            node = self.tiledTopo.getClosedNode(point)

            if code in self.previous_connections:
                if self.previous_connections[code][0] != node:
                    new_current_connection[code] = (node,self.previous_connections[code][0])
            else:
                new_current_connection[code] = (node, None)

        # Only update user localization when the user changes of node
        for code,(newnode,oldnode) in new_current_connection.items():
            print("Car %s, from %s to %s"%(code,oldnode,newnode))
            if oldnode == None:
                # creating new user on node newnode
                # app?
                self.logger.info("New user (#%s) on node %s" % (code, newnode))
                app_name = random.sample(self.listIdApps,1)[0]
                app = sim.apps[app_name]
                msg = app.get_message("M.USER.APP.%i"%app_name)
                dist = deterministic_distribution(30, name="Deterministic")

                idDES = sim.deploy_source(app_name, id_node=newnode, msg=msg, distribution=dist)
                self.mapsUser[code] = idDES
            else:
                self.logger.info("A new movement of user (#%s) from node %s to node %s" % (code, oldnode,newnode))
                self.total_diff_connections+=1
                sim.alloc_DES[self.mapsUser[code]] = newnode



        self.previous_connections = new_current_connection



        if self.current_step == 3: exit()
        ########
        #     # Actualizar vinculo en la red
        #
        #     ########
        #
        #     if code not in sim.mobile_fog_entities.keys():
        #         point_index = sim.coverage.connection(new_point)
        #         if point_index != None:
        #             all_current_connection[code] = self.name_endpoints[point_index]
        #
        # ##########
        # # UPDATING CONNECTIONS of MOBILE ENDPOINTS  with NETWORK ENDPOINTS
        # #
        # fixed_location_endpoints = dict(zip(sim.name_endpoints.values(), sim.endpoints))
        # mobile_location_endpoints = dict(zip(code_mobile_endpoints, mobile_endpoints))
        #
        # print("STEP ",self.current_step)
        # # print "FIX"
        # # print fixed_location_endpoints
        # # print "MOB"
        # # print mobile_location_endpoints
        # connection_mobiles = sim.coverage.connection_between_mobile_entities(fixed_location_endpoints,
        #                                                                      mobile_location_endpoints,sim.mobile_fog_entities)
        #
        # # we merge both type of connections to generate the graph
        # for k in connection_mobiles:
        #     all_current_connection[k]=connection_mobiles[k]
        #
        # # we detect the elements without a current disconection
        # previous_code = set(all_current_connection)
        # without_connection = [x for x in self.previous_graph_connection if x not in previous_code]
        #
        # # DEBUGING CODE
        # # if self.current_step >= 31 and self.current_step<41:
        # #     print "CURRENT CONNECTION "
        # #     print all_current_connection
        # #     print "PREVIOUS "
        # #     print self.previous_graph_connection
        # #     print "WITHOUT CONNECTION"
        # #     print without_connection
        #
        #
        # # updating previous network connections with the current ones
        #
        # changes = False
        # for k in all_current_connection:
        #     if not k in self.previous_graph_connection.keys():
        #         self.previous_graph_connection[k] = all_current_connection[k]
        #         self.update_topology_connections(sim, k, self.previous_graph_connection[k], routing)
        #         changes = True
        #     else:
        #         if all_current_connection[k]==self.previous_graph_connection[k]:
        #             # Position without changes
        #             None
        #         else:
        #             self.previous_graph_connection[k] = all_current_connection[k]
        #             self.update_topology_connections(sim, k, self.previous_graph_connection[k], routing)
        #             changes = True
        #
        #     # print "%s ;  %s" % (k,all_current_connection[k])
        #
        #
        #
        # # updating elements without a current connection: removing edges
        # for k in without_connection:
        #     self.update_topology_connections(sim, k, None, routing)
        #     del self.previous_graph_connection[k]
        #


        # WARNING:
        # This is an expensive task (optional)
        # if self.doExecutionVideo:
        #     # sim.topology.draw_png(self.path_results + "network_%i" % self.current_step)
        #     self.animation.make_snap(self.current_step, self.path_results + "snap_%05d" % self.current_step,
        #                              G=sim.topology.G, draw_connection_line=False)

        self.logger.info("\tEND movement #%i. Time taken: %s" %(self.current_step,(time.time()- start_time)))

        sim.print_debug_assignaments()
        # we prepare the next execution of this function
        self.current_step += 1



