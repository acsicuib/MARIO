import logging

import time
from yafs.distribution import *

class UserControlMovement:

    def __init__(self,
                 experiment_path,
                 doExecutionVideo,
                 appOp,
                 record_movements,
                 limit_steps,
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
        self.edgeNodes = edgeNodes


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

                if (random.random()>0.2):
                    # We move some users
                    numberUsersToMove = np.random.choice(np.random.randint(3, 6))
                    ids = np.random.choice(usersDES, numberUsersToMove)
                    toNode = np.random.choice(self.edgeNodes,numberUsersToMove)
                    usersDestination = zip(ids,toNode)
                    for idDES, new_node in usersDestination:

                        if new_node != sim.alloc_DES[idDES]:
                            old_node = sim.alloc_DES[idDES]
                            self.logger.debug("A new movement of user (#%s) from node %s to node %s" % (
                            idDES, sim.alloc_DES[idDES], new_node))
                            sim.alloc_DES[idDES] = new_node
                            self.record_movements.write("%i,%i,%i,%i,%s\n" % (sim.alloc_source[idDES]["app"],idDES, sim.env.now, old_node,new_node))
                else:
                    self.logger.info("\t without user movements  ")


                self.logger.debug("\tEND movement #%i. Time taken: %s" %(self.current_step,(time.time()- start_time)))
                # sim.print_debug_assignaments()
                # we prepare the next execution of this function
                self.current_step += 1
            #end allo_source > 0
