import logging

import time
from yafs.distribution import *

class UserControlMovement:


    def __init__(self,
                 experiment_path,
                 doExecutionVideo,
                 tiledTopo,
                 listIdApps,
                 appOp,
                 record_movements,
                 ratio_message,
                 logger=None):

        self.logger = logger or logging.getLogger(__name__)
        self.current_step = 0
        self.experiment_path = experiment_path

        self.previous_connections = {}
        # self.name_endpoints = {}
        self.doExecutionVideo = doExecutionVideo
        self.tiledTopo = tiledTopo

        self.all_available_Apps = listIdApps
        self.mapsUser = {}
        self.total_diff_connections = 0
        self.prev_node = {} # store connections betw. users and nodes
        self.ratio_message_generation = ratio_message

        self.record_movements = record_movements
        # Mario is here to control the deployment of new modules in the cloud node when there is requested by new users.
        self.appOp = appOp

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
        self.logger.info("Movement number (#%i) at time: %i" % (self.current_step, sim.env.now))
        start_time = time.time()

        ##
        # TEST: for debug control !
        # It takes a snapshot of a specific movement
        # if self.current_step == 0:
        # self.animation = AnimationTrack(sim, dpi=100, bg_map=True, aspect='equal')


        # Get the coordinate of each user in this step
        user_positions = self.currentMovement(sim.user_tracks.df,self.current_step)

        # Updating positions
        for code in user_positions:
            (lat, lng) = user_positions[code]
            point = [lat, lng]
            new_node = self.tiledTopo.getClosedNode(point)

            # User already exists
            if code in self.prev_node:

                # User has changed node
                if self.prev_node[code] != new_node:
                    self.logger.debug("A new movement of user (#%s) from node %s to node %s" % (code, self.prev_node[code], new_node))
                    self.total_diff_connections += 1
                    sim.alloc_DES[self.mapsUser[code]] = new_node
                    self.record_movements.write("%s,%i,%i,%s,%s\n"%(code,self.mapsUser[code],sim.env.now,self.prev_node[code],new_node))
            else:
                # Creating new user on new_node
                self.logger.debug("New user (#%s) on node %s" % (code, new_node))

                # What application does the user decide to use?
                app_name = random.sample(self.all_available_Apps, 1)[0]

                #TODO !!! THIS CODE IS TO HAVE THE SAME QUANTITY MOVEMENTS IN EACH APPS EXPERIMENTATION
                #### Only to "cuadrar" the experimentation
                if code == "taxi_298":
                    app_name = 1
                elif code == "taxi_297":
                    app_name = 2
                elif code == "taxi_94":
                    app_name = 3
                ######

                # Is the app currently deployed on the infrastructure? Along the simulation the app can be "undeployed"-oper.
                # if not is deployed the app on the sim.
                if not app_name in sim.alloc_module: #only check the APP deployed (not all the submodules/services DAO)
                    # we deploy a new module in the cloud node
                    # NOTE: The initial name of the service is this "x_01" in our scenario
                    self.appOp.deploy_module(sim,"%i_01"%app_name,self.appOp.cloud_node,routingAlgorithm,self.experiment_path)

                app = sim.apps[app_name]
                msg = app.get_message("M.USER.APP.%i" % app_name)
                dist = deterministic_distribution(self.ratio_message_generation, name="Deterministic")
                idDES = sim.deploy_source(app_name, id_node=new_node, msg=msg, distribution=dist)
                self.mapsUser[code] = idDES
                self.record_movements.write("%s,%i,%i,none,%s\n" % (code, idDES, sim.env.now, new_node))

            self.prev_node[code] = new_node

        self.logger.debug("\tEND movement #%i. Time taken: %s" %(self.current_step,(time.time()- start_time)))
        # sim.print_debug_assignaments()
        # we prepare the next execution of this function
        self.current_step += 1

    # def write_map_user_des(self,path):
    #     with open(path,"w") as f:
    #         f.write("code,DES\n")
    #         for code in self.mapsUser:
    #             f.write("%s,%i\n"%(code,self.mapsUser[code]))
