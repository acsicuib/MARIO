import logging

import time
from yafs.distribution import *
import numpy as np
import random

class UserControlMovement:

    def __init__(self,
                 experiment_path,
                 appOp,
                 record_movements,
                 limit_steps,
                 limit_movements,
                 edgeNodes,
                 type_scenario,
                 logger=None):

        self.logger = logger or logging.getLogger(__name__)
        self.current_step = 0
        self.experiment_path = experiment_path

        self.previous_connections = {}
        # self.name_endpoints = {}

        self.mapsUser = {}
        self.total_diff_connections = 0
        self.prev_node = {} # store connections betw. users and nodes

        self.limit_step = limit_steps
        self.record_movements = record_movements
        # Mario is here to control the deployment of new modules in the cloud node when there is requested by new users.
        self.appOp = appOp
        self.edgeNodes = list(edgeNodes)
        self.limit_movements = limit_movements
        self.probability_matriz = {}

        # DAMMIT. This code should be loaded from any place. Future reader sorry for this mix.
        # case: Basic(B)> Probability Matrix, Medium(M),  Drone(D)
        self.type_scenario = type_scenario

        if self.type_scenario == "B":
            if self.probability_matriz == {}:
                #NOTE last column is the probability to go out check function "get_node"
                self.probability_matriz[1] = [  ## AKA USERS - TEACHERS
                    [0.3, 0.6, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0],
                    [0.2, 0.4, 0.3, 0.1, 0.0, 0.0, 0.0, 0.0, 0],
                    [0.0, 0.15, 0.5, 0.15, 0.0, 0.2, 0.0, 0.0, 0],
                    [0.0, 0.0, 0.4, 0.4, 0.2, 0.0, 0.0, 0.0, 0],
                    [0.0, 0.0, 0.1, 0.5, 0.4, 0.0, 0.0, 0.0, 0],
                    [0.0, 0.0, 0.1, 0.0, 0.0, 0.7, 0.2, 0.0, 0],
                    [0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.6, 0.3, 0],
                    [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.4, 0.6, 0]
                ]
                ## AKA USERS - Students
                self.probability_matriz[2] = [
                    [0.46, 0.44, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0],
                    [0.3, 0.4, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0],
                    [0.0, 0.1, 0.6, 0.1, 0.0, 0.2, 0.0, 0.0, 0],
                    [0.0, 0.0, 0.1, 0.5, 0.2, 0.2, 0.0, 0.0, 0],
                    [0.0, 0.0, 0.0, 0.24, 0.56, 0.2, 0.0, 0.0, 0],
                    [0.0, 0.0, 0.4, 0.0, 0.1, 0.3, 0.2, 0.0, 0],
                    [0.0, 0.0, 0.0, 0.0, 0.0, 0.6, 0.2, 0.2, 0],
                    [0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.6, 0.3, 0]
                ]


        if self.type_scenario == "M":
            self.buildings = {"b0": np.arange(6, 14),
                  "b1": np.arange(101, 106),
                  "b2": np.arange(124, 136),  # classes
                  "b3": np.arange(124, 136),  # classes
                  "b4": np.concatenate((np.arange(158, 170), np.arange(180, 186))),
                  "b5": np.arange(191, 193)  # subway
                  }

        if self.type_scenario == "L":
            self.buildings = {"b0": np.arange(6, 14),
              "b1": np.arange(101, 106),
              "b2": np.arange(124, 136),  # classes
              "b3": np.arange(124, 136),  # classes
              "b4": np.concatenate((np.arange(158, 170), np.arange(180, 186))),
              "b5": np.arange(191, 193),  # subway

              "b6": np.arange(406, 414),
              "b7": np.arange(201, 206),
              "b8": np.arange(224, 236),  # classes
              "b9": np.arange(224, 236),  # classes
              "b10": np.concatenate((np.arange(258, 270), np.arange(280, 286))),
              "b11": np.arange(291, 293)  # subway
              }

    def getNeighbourdNode(self,node):
        for k in self.buildings.keys():
            if node in self.buildings[k]:
                return self.buildings[k]



    def get_toNode(self,app,pos_node):
        """
        This functions return a possible node where the users will go

        The statical issue: if we have uniform distribution of users and a lots of users, the movement will have no consequences
        as there will be another similar user, and another user will be able to get to where the initial one has gone.

        :param app:
        :param pos_node:
        :return:
        """
        if self.type_scenario=="B":
            #NOTE edgeNodes goes from: 6..13 to 0..7
            matrix_pos = pos_node-6
            probabilities = list(self.probability_matriz[app][matrix_pos])
            toNode =  np.random.choice(self.edgeNodes+[-1],1,p=probabilities)[0]
            return toNode
        else:
            if random.random()>0.9: #we can change of building
                return np.random.choice(self.edgeNodes+[-1],1)[0]
            else:
                nodes = self.getNeighbourdNode(pos_node)
                return np.random.choice(nodes, 1)[0]


    def __call__(self, sim, routingAlgorithm,case, stop_time, it):
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

        usersDES = list(sim.alloc_source.keys())
        if len(usersDES) > 0:
            if self.current_step<=self.limit_step:
                self.logger.info("Movement number (#%i) at time: %i" % (self.current_step, sim.env.now))
                start_time = time.time()
                current_moves = 0
                random.shuffle(usersDES)
                for user in usersDES:
                    # We move some users
                    current_node = sim.alloc_DES[user]

                    pos_node = self.edgeNodes.index(current_node)
                    app = sim.alloc_source[user]["app"]

                    if app == 4:  # DRONE: it's not necessary to comment for MEDIUM and BASIC SCENARIO
                        toNode = np.random.choice(self.edgeNodes, 1)[0]
                        if current_node != toNode:
                            self.logger.debug(
                                "A new movement of user (#%s) from node %s to node %s" % (user, current_node, toNode))
                            sim.alloc_DES[user] = toNode
                            self.record_movements.write("%i,%i,%i,%i,%s\n" % (
                                sim.alloc_source[user]["app"], user, sim.env.now, current_node, toNode))
                        current_moves += 1

                    if app == 2:
                        toNode = self.get_toNode(app,current_node)

                        if current_node != toNode and toNode >= 0:
                            self.logger.debug("A new movement of user (#%s) from node %s to node %s" % (user, current_node, toNode))
                            sim.alloc_DES[user] = toNode
                            self.record_movements.write("%i,%i,%i,%i,%s\n" % (sim.alloc_source[user]["app"],user, sim.env.now, current_node,toNode))
                            current_moves +=1
                        elif toNode == -1:
                            self.logger.debug(
                                "A new movement of user (#%s) from node %s to EXIT" % (user, current_node))
                            self.record_movements.write("%i,%i,%i,%i,%s\n" % (
                                sim.alloc_source[user]["app"], user, sim.env.now, current_node, toNode))
                            sim.undeploy_source(user)
                        current_moves += 1

                    if app == 1:
                        toNode = self.get_toNode(app, current_node)

                        if current_node != toNode and toNode >= 0:
                            self.logger.debug(
                                "A new movement of user (#%s) from node %s to node %s" % (user, current_node, toNode))
                            sim.alloc_DES[user] = toNode
                            self.record_movements.write("%i,%i,%i,%i,%s\n" % (
                            sim.alloc_source[user]["app"], user, sim.env.now, current_node, toNode))

                        elif toNode == -1:
                            self.logger.debug(
                                "A new movement of user (#%s) from node %s to EXIT" % (user, current_node))
                            self.record_movements.write("%i,%i,%i,%i,%s\n" % (
                                sim.alloc_source[user]["app"], user, sim.env.now, current_node, toNode))
                            sim.undeploy_source(user)
                        current_moves += 1

                    if current_moves >= self.limit_movements:
                        break

                    # if app != 3: #This users have a fixed position along the simulation
                    #     self.logger.info("\t without user movements  ")

                #end for
                self.logger.debug("\tEND movement #%i. Time taken: %s" %(self.current_step,(time.time()- start_time)))
                # sim.print_debug_assignaments()
                # we prepare the next execution of this function
                self.current_step += 1
            #end allo_source > 0
