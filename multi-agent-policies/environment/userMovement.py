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

        # if self.probability_matriz == {}:
        #     self.probability_matriz[1]=[ ## AKA USERS - TEACHERS
        #         [0.2, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3 ],
        #         [0.1, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3 ],
        #         [0.0, 0.05, 0.3, 0.05, 0.0, 0.1, 0.0, 0.0,0.5],
        #         [0.0, 0.0, 0.2, 0.2, 0.1, 0.0, 0.0, 0.0,.5],
        #         [0.0, 0.0, 0.0, 0.3, 0.1, 0.0, 0.0, 0.0, 0.6],
        #         [0.0, 0.0, 0.1, 0.0, 0.0, 0.6, 0.1, 0.0,.2],
        #         [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.4, 0.2,0.4],
        #         [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.4,0.4]
        #     ]
        #     ## AKA USERS - Students
        #     self.probability_matriz[2]= [
        #         [0.36, 0.34, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, .3],
        #         [0.2, 0.3, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3],
        #         [0.0, 0.1, 0.5, 0.1, 0.0, 0.2, 0.0, 0.0, 0.1],
        #         [0.0, 0.0, 0.1, 0.4, 0.2, 0.1, 0.0, 0.0, 0.2],
        #         [0.0, 0.0, 0.0, 0.14, 0.46, 0.0, 0.0, 0.0, 0.4],
        #         [0.0, 0.0, 0.4, 0.0, 0.0, 0.2, 0.1, 0.0, 0.3],
        #         [0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.2, 0.2, 0.1],
        #         [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.6, 0.2, 0.2]
        #     ]


        if self.probability_matriz == {}:
            self.probability_matriz[1]=[ ## AKA USERS - TEACHERS
                [0.3, 0.6, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0,0],
                [0.2, 0.4, 0.3, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0 ],
                [0.0, 0.15, 0.5, 0.15, 0.0, 0.2, 0.0, 0.0,0],
                [0.0, 0.0, 0.4, 0.4, 0.2, 0.0, 0.0, 0.0,0],
                [0.0, 0.0, 0.1, 0.5, 0.4, 0.0, 0.0, 0.0, 0],
                [0.0, 0.0, 0.1, 0.0, 0.0, 0.7, 0.2, 0.0,0],
                [0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.6, 0.3,0],
                [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.4, 0.6,0]
            ]
            ## AKA USERS - Students
            self.probability_matriz[2]= [
                [0.46, 0.44, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, .0],
                [0.3, 0.4, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0],
                [0.0, 0.1, 0.6, 0.1, 0.0, 0.2, 0.0, 0.0, 0],
                [0.0, 0.0, 0.1, 0.5, 0.2, 0.2, 0.0, 0.0, 0],
                [0.0, 0.0, 0.0, 0.24, 0.56, 0.2, 0.0, 0.0, 0],
                [0.0, 0.0, 0.4, 0.0, 0.1, 0.3, 0.2, 0.0, 0],
                [0.0, 0.0, 0.0, 0.0, 0.0, 0.6, 0.2, 0.2, 0],
                [0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.6, 0.3, 0]
            ]


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
                    if app == 2:
                        probabilities = list(self.probability_matriz[app][pos_node])
                        toNode =  np.random.choice(self.edgeNodes+[-1],1,p=probabilities)[0]

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
                        probabilities = list(self.probability_matriz[app][pos_node])
                        toNode = np.random.choice(self.edgeNodes+[-1], 1, p=probabilities)[0]
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

                    # if app != 3:
                    #     self.logger.info("\t without user movements  ")

                #end for
                self.logger.debug("\tEND movement #%i. Time taken: %s" %(self.current_step,(time.time()- start_time)))
                # sim.print_debug_assignaments()
                # we prepare the next execution of this function
                self.current_step += 1
            #end allo_source > 0
