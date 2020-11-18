from collections import defaultdict
from agent import PolicyManager
from yafs.distribution import *
import logging
import matplotlib.pyplot as plt
import networkx as nx
from PIL import Image
from pathlib import Path
from matplotlib import colors
import matplotlib as mpl
import os
import json
from collections import Counter
import tiledTopology
import matplotlib.patches as mpatches


RENDERSNAP = True

class Mario():
    """
    We called it Mario in honour to our video game moustached character ;)

    """


    def __init__(self, common_rules, service_rule_profile, path_csv_files, app_number, period, render, path_results, cloud_node, window_agent_size):
        self.create_initial_services = True
        self.logger = logging.getLogger(__name__)
        self.memory = []
        # self.memory = deque(maxlen=200)
        self.period = period
        self.common_rules = common_rules
        self.service_rule_profile = service_rule_profile
        self.pos = None
        self.image_dir = None
        self.step = 0
        self.UID = 0 # count all steps (valid or not valid rules)
        self.path_csv_files = path_csv_files
        self.total_services = app_number
        self.path_results = path_results
        #render
        self.render_action = render
        self.image_id =0
        self.snap_id =0
        self.__draw_controlUser = {}

        self.active_monitor = {}
        # key: id_service (DES), value: id_monitor (DES)

        #STATS
        self.dump_stats = 0

        #Communication channel with the agents
        self.agent_communication = defaultdict(list) #it keeps a record about the last action taken by the agent and its status: accept or reject
        self.window_agent_comm = defaultdict(int)
        self.window_agent_size = window_agent_size

        self.cloud_node = cloud_node

    def __call__(self, sim, routing, path):
        """
        This functions is called periodically

        First time, it generates the initial DES-process//agents for the initial deployment services
        Second time and so one, it controls and perfoms the agent actions
        :param sim:
        :param routing:
        :param path:
        :return:
        """
        # The occupation of a node can be managed by the simulator but to easy integration with the visualization both structures are different

        if self.create_initial_services:
            self.service_calls = defaultdict(list)
            for app in sim.alloc_module:
                for service in sim.alloc_module[app]:
                    for des in sim.alloc_module[app][service]:
                        self.create_monitor_of_module(des, path, routing, service, sim)
            self.create_initial_services = False #only one time

            self.action_stats = open(self.path_results+"action_stats.txt",'w+')
            self.action_stats.write("time,undeploy,nop,migrate,replicate,none\n")
        else:
            if len(self.memory)>0:
                self.step += 1
                # DEBUG
                # print("+"*20)
                self.logger.debug("MARIO - Activation step: %i  - Time: %i" %(self.step,sim.env.now))
                # print("- Size buffer of actions: %i" % len(self.memory))
                # print("- Current situation:")
                # sim.print_debug_assignaments()


            # current implementation FCFS
            self.UID += 1
            take_last_action = [] # It perfoms the last set of agent rules
            counter_actions = Counter()
            clean_routing_cache = False
            # (name, DES, currentNode, ('migrate', '2', 'n0lt0ln0'),)
            for name,DES,currentNode,operations in reversed(self.memory):
                for (action,service_id,onNode) in operations:
                    # print("+ Actions from DES_service: ", DES)
                    # print("\tService: ", name)
                    # print("\tID_Service: ", service_id)
                    # print("\tNode: ", currentNode)
                    # print("\tAction: ", action)
                    # print("\t+ OnNode:", onNode)

                    service_id = int(service_id)
                    if onNode == "self":
                        onNode = currentNode

                    if DES not in take_last_action: # One rule for agent we get the last one
                        take_last_action.append(DES)

                        #Reset the buffer of lastoutcome statements
                        if self.window_agent_size == self.window_agent_comm[service_id]:
                            self.window_agent_comm[service_id] = 0
                            self.agent_communication[service_id] = []

                        # The render of the action is done the state of the simulator changes
                        if self.render_action:
                              image_file = self.render(sim, path, routing,
                                                       service=name,
                                                       serviceID=service_id,
                                                       currentNode=currentNode,
                                                       action=action,
                                                       onNode=onNode)


                        done = self.perfom_action(sim,
                                                  service = name,
                                                  serviceID=service_id,
                                                  currentNode = currentNode,
                                                  action = action,
                                                  onNode = onNode,
                                                  routingAlgorithm= routing,
                                                  path = path)



                        if self.render_action and not done:
                            try:
                                self.logger.warning("Image removed")
                                os.remove(image_file)
                            except FileNotFoundError:
                                None


                        if action == "undeploy" and service_id in self.agent_communication:
                            del self.agent_communication[service_id]

                        # status = "accepted"
                        if done:
                            if not clean_routing_cache:
                                clean_routing_cache = (action != "nop")
                            self.logger.debug("Action %s taken on Node %s." % (action, onNode))
                            counter_actions[action] += 1
                            break
                        else:
                            self.logger.debug("Action %s on nNode %s not possible."%(action,onNode))
                            counter_actions["none"] += 1
                            status = "rejected"
                            self.agent_communication[service_id].append(((action, service_id, onNode), status))

                #end for (operations)
                self.window_agent_comm[service_id] += 1
            #end for all-operations

            self.memory = []
            if clean_routing_cache:
                # Cache routing data is stored to improve the execution time of the simulator
                if RENDERSNAP:
                    self.snapshot(sim, path, routing) # IMPORTANT previous to clean the cache!
                # if the cache has to be cleared it is because there have been changes.
                routing.clear_routing_cache()

            #writing stats
            if len(counter_actions)>0:
                # print(counter_actions)
                line = ""
                for k in ["undeploy","nop","migrate","replicate","none"]:
                    line += "%i,"%counter_actions[k]

                # print(line[:-1])
                self.action_stats.write("%i,%s\n"%(sim.env.now,line[:-1]))
                self.action_stats.flush()


    def perfom_action(self, sim,
                      service,
                      serviceID,
                      currentNode,
                      action,
                      onNode,
                      routingAlgorithm,
                      path):
        """
        Decode the action and perfom the action in the simulator

        :param sim:
        :param action:
        :return:
        """
        if action == "replicate": #it is prepared to get a list of nodes
            nodes_to_replicate = [onNode]
            space_on_node = self.get_free_space_on_nodes(sim)
            # All these comments made mandatory that all nodes have capacity to host the all instances.
            # feasible = True #
            nodes_with_space = []
            for n in nodes_to_replicate:
                if space_on_node[n]==0:
                    self.logger.warning("There is not more free space on node: %s"%n)
                    # print("\t WARNING: NO FREE SPACE ON NODE:%s"%n)
                else:
                    nodes_with_space.append(n)

            for n in nodes_with_space:
                self.logger.debug("Action Replicate new instance of %s on node: %s" % (service, n))
                # print("\t\t+Action Replicate new instance of %s on node: %s" % (service, n))
                self.deploy_module(sim, service, n, routingAlgorithm, path)

            return len(nodes_with_space)>0

        elif action == "migrate":
            # Example
            # parameters: 0,[3, 2]
            try:
                target_node = onNode
                space_on_node = self.get_free_space_on_nodes(sim)
                feasible = True
                if space_on_node[target_node] <= 0:
                    self.logger.warning("There is not more free space on node: %s" % target_node)
                    print("\t WARNING: NO FREE SPACE ON NODE:%s" % target_node)
                    return False
                else:
                    self.logger.debug("Action Migrate new instance of %s on node: %s" % (service, target_node))
                    # print("\t\t+Action Migrate new instance of %s on node: %s" % (service, target_node))
                    self.deploy_module(sim, service, target_node, routingAlgorithm, path)
                    self.logger.debug("\t Remove current instance %s on node: %s" % (service, currentNode))
                    # print("\t\t+Remove current instance of %s on node: %s" % (service, currentNode))
                    self.undeploy_module(sim, service, currentNode, serviceID)
                    return True
            except:
                self.logger.critical("A migration rule with no target node")
                # print("A migration rule with no target node")
                return False

        elif action == "undeploy":
            # no arguments
            self.logger.debug("Action UNDEPLOY instance %s on node: %s" % (service, currentNode))
            self.undeploy_module(sim,service,currentNode,serviceID)

            return True

        elif action == "nop":
            # Doing nothing
            self.logger.debug("Action NOP instance %s on node: %s" % (service, currentNode))
            return True

        elif action == "fusion":
            return True


    def undeploy_module(self,sim,service,node,id_service):
        sim.undeploy_module(self.get_app_identifier(service), service, id_service)
        #Remove stop process
        del self.active_monitor[id_service]
        sim.stop_process(id_service)

    def deploy_module(self, sim, service, node, routingAlgorithm, experiment_path):
        # Allocation the module in the simulator
        app_name = self.get_app_identifier(service)
        app = sim.apps[app_name]
        services = app.services
        des = sim.deploy_module(app_name, service, services[service], [node])[0]
        # Creating a new monitor associated to the module
        self.create_monitor_of_module(des, experiment_path, routingAlgorithm, service, sim)

    def create_monitor_of_module(self, des, path, routing, service, sim):
        period = deterministic_distribution(self.period, name="Deterministic")
        pm = PolicyManager(des, service, self.common_rules, self.service_rule_profile, self.path_csv_files, self, self.render)

        # Performance tip: it avoids an huge load of useless samples
        number_of_lines = 0
        with open(self.path_csv_files+ ".csv", "r") as filehandle:
            number_of_lines = len(filehandle.readlines())
        pm.load_samples_from = number_of_lines
        ####

        id_monitor = sim.deploy_monitor("Policy Manager %i" % des, pm, period,
                                        **{"sim": sim, "routing": routing, "experiment_path": path, "path_results":self.path_results})
        pm.id_monitor = id_monitor
        logging.debug("Generating a new agent control from: %s with id: %i - Monitor: %i" % (service, des, id_monitor))
        # print("Generating a new agent control from: %s with id: %i - Monitor: %i" % (service, des, id_monitor))
        self.active_monitor[des] = id_monitor

    def get_actions_from_agents(self, something):
        """
        Agents write in this buffer

        :param something:
        :return:
        """
        self.memory.append(something)

    def __draw_user(self, node, service, ax, newcolors):
        """
        Draw a dot for each user, plus sort the dots for each node
        :param node:
        :param service:
        :param ax:
        :param newcolors:
        :return:
        """
        if node not in self.__draw_controlUser.keys():
            self.__draw_controlUser[node] = 0
        total = self.__draw_controlUser[node]
        line = int(total / 8) + 1
        # duy = 0.06 * line
        # dux = 0.01 * (total % 4)

        #simple policies
        duy = -0.26 * (line*1.1)
        dux = (.0 * (total % 4))+(0.2*total)-0.4*line

        # #
        # # new
        # duy = 4.56 * line
        # dux = 2.55 * (total % 4)
        # self.__draw_controlUser[node] += 1

        ax.scatter(self.pos[node][0] + dux, self.pos[node][1] + duy, s=400.0, marker='o', color=newcolors[service],edgecolors="black")

        self.__draw_controlUser[node]+=1

    def get_app_identifier(self,nameservice):
        return int(nameservice[0:nameservice.index("_")])

    def get_nodes_with_users(self, routing):
        nodes_with_users = defaultdict(list)
        for (_,node_user,user_service) in routing.controlServices.keys():
            nodes_with_users[node_user].append(self.get_app_identifier(user_service))
        return nodes_with_users

    def get_free_space_on_nodes(self,sim):
        currentOccupation =dict([a, int(x)] for a,x in nx.get_node_attributes(G=sim.topology.G, name="HwReqs").items())
        for app in sim.alloc_module:
            dict_module_node = sim.alloc_module[app]  # modules deployed
            for module in dict_module_node:
                for des in dict_module_node[module]:
                    currentOccupation[sim.alloc_DES[des]] -= 1
        return currentOccupation





    def get_nodes_with_services(self,sim):
        """
        It returns a dictionary for node with a np.array with the occupation for visualization purporse

        :param sim:
        :return:
        """
        HwReqs = nx.get_node_attributes(G=sim.topology.G, name="HwReqs")
        currentOccupation = {}
        #Init. the shape-grid structure to render it with zeros
        for n in sim.topology.G.nodes:
            currentOccupation[n] = np.zeros(int(HwReqs[n])).astype(int)

        for app in sim.alloc_module:
            dict_module_node = sim.alloc_module[app] # modules deployed
            for module in dict_module_node:
                for des in dict_module_node[module]:
                    indx = list(currentOccupation[sim.alloc_DES[des]]).index(0)
                    currentOccupation[sim.alloc_DES[des]][indx] = app

        shape = nx.get_node_attributes(G=sim.topology.G, name="shape")
        for node in currentOccupation:
            try:
                currentOccupation[node] = np.array(currentOccupation[node]).reshape(eval(shape[node]))
            except ValueError:
                raise "Network node: %s defined with a bad shape "%node
                # print("Network node: %i defined with a bad shape "%node)
                # currentOccupation[node] = np.zeros(shape(1,1))

        return currentOccupation

    def render(self,sim,path,routing,service,serviceID,currentNode,action,onNode):

        if self.pos == None: # Only the first time
            self.pos = nx.get_node_attributes(sim.topology.G,'pos')
            # if len(pos)>0:
            #     for k in pos.keys():
            #         pos[k] = np.array(eval(pos[k]))
            #     self.pos = pos
            # else:
            #
            # self.pos = nx.random_layout(sim.topology.G)

            image_dir = Path(self.path_results+"images/")
            image_dir.mkdir(parents=True, exist_ok=True)
            self.image_dir = str(image_dir)

        tab20 = plt.cm.get_cmap('tab20', self.total_services+5)
        bounds = range(self.total_services+5)
        newcolors = tab20(np.linspace(0, 1, self.total_services+5))
        newcolors[0] = np.array([250.0 / 256.0, 250. / 256., 250. / 256., 1])
        newcmp = mpl.colors.ListedColormap(newcolors)
        norm = mpl.colors.BoundaryNorm(bounds, newcmp.N)

        fig, ax = plt.subplots(figsize=(16.0, 10.0))
        plt.tight_layout(h_pad=0.55)
        # left, bottom, width, height = ax.get_position().bounds
        #

        nx.draw(sim.topology.G, self.pos, with_labels=False, node_size=1, node_color="#1260A0", edge_color="gray", node_shape="o",
                 font_size=7, font_color="white", ax=ax)

        width = ax.get_xlim()[1]
        top = ax.get_ylim()[1]
        # Some viz. vars.
        piesize = .078
        # piesize = .08
        p2 = piesize / 2.5

        try:
            idApp = int(service.split("_")[0])
        except ValueError: #It triggers when the simulation ends: action(null)
            idApp = 0

        color_app = newcmp(idApp)


        ##########
        # Textual data
        ##########
        # TODO UNCOMMENT
        plt.text(3.5, top, "Simulation time: %i" % sim.env.now,{'color': color_app, 'fontsize': 10})

        info_text = "Action %s on node: %s" % (action,onNode)
        plt.text(3.5 , top-0.5, info_text, {'color': color_app, 'fontsize': 10})

        info_text = "by Service: S%i on Node: %s" % (serviceID, tiledTopology.getAbbrNodeName(currentNode))
        plt.text(3.5, top -1, info_text, {'color': color_app, 'fontsize': 10})


        # Get the POLICY FILE
        # As the service is named: "idApp_IdModule", we can get the app id from there.
        dataApps = json.load(open(path + 'appDefinition.json'))
        rule_policy = ""

        try:
            for app in dataApps:
                if app["id"] == idApp:
                    rule_policy = app["profile_rules"]
                    break
        except UnboundLocalError:
            print("- WARNING - Rendering the image of the last case")
            None #Render the last case

        # TODO UNCOMMENT
        info_text = "App: %i with policy: %s" % (idApp, rule_policy)
        plt.text(3.5, top -1.5, info_text, {'color': color_app, 'fontsize': 10})
        info_text = "Debug file: rules_swi_UID%i_n%s_s%i_X_%i.pl" % (self.UID, tiledTopology.getAbbrNodeName(currentNode), serviceID, sim.env.now)
        plt.text(3.5, top - 2., info_text, {'color': color_app, 'fontsize': 10})


        # Labels on nodes
        for x in sim.topology.G.nodes:
            if x == 0:
                ax.text(self.pos[x][0]- (width/12), self.pos[x][1] , tiledTopology.getAbbrNodeName(x), fontsize=10,
                        fontweight="bold")
            else:
                ax.text(self.pos[x][0] - (width/75), self.pos[x][1] + (width/35) , tiledTopology.getAbbrNodeName(x), fontsize=10,fontweight="bold")

        #TODO IMPROVE THE GENERATION OF THE LEGEND according with APP & Policies
            # TODO UNCOMMENT
        # APP Legend
        if not "closers" in self.image_dir:
        # DEFAULT Legends apps
            legendItems = []
            for i in range(1,len(dataApps)+1):
                color_app = newcmp(i)
                legendItems.append(mpatches.Patch(color=color_app, label='App: %i'%i))
            # plt.legend(handles=legendItems, title=rule_policy)
            # plt.legend(handles=legendItems)
            plt.legend(loc="lower center",handles=legendItems, ncol=len(dataApps))
        #
        # else:
        #     # SPECIFIC LEGEND for experiment: I_II_III
        #     offset = 0
        #     text=["Get_Closer","Get_Closer_II","Get_Closer_III"]
        #     for i in range(0, len(dataApps) ):
        #         color_app = newcmp(i+1)
        #         if (i%2 == 0):
        #             ax.annotate("%s" % text[offset],
        #                         xy=(0.95, 1.0 - ((i + offset) * 0.05)), xycoords='axes fraction',
        #                         textcoords='offset points',
        #                         size=14,
        #                         bbox=dict(boxstyle="round", fc="white", ec="none"))
        #             offset+=1
        #
        #         ax.annotate("App: %i"%(i+1),
        #             xy=(0.95, 1.0-((i+offset)*0.05)), xycoords='axes fraction',
        #             textcoords='offset points',
        #             size=14,
        #             bbox=dict(boxstyle="round", fc=color_app, ec="none"))
        #
        #         # TODO IMPROVE THE GENERATION OF THE LEGEND according with APP & Policies
        #         # APP Legend


        #TODO INDIVIDUAL POLKICIES IMAGE LABEL
        # offset = 0
        # # text = ["GetCloser"]
        # # text = ["Eco_GetCloser"]
        # text = ["Turbo_GetCloser"]
        # ax.annotate("%s" % text[offset],
        #             xy=(0.01, 0.96 - ((0 + offset) * 0.08)), xycoords='axes fraction',
        #             textcoords='offset points',
        #             size=38,
        #             weight="bold",
        #             bbox=dict(boxstyle="round", fc="white", ec="none"))
        # offset += 1
        #
        # for i in range(0, len(dataApps),2):
        #     color_app = newcmp(i + 1)
        #     ax.annotate("  App: %i " % (i + 1),
        #                 xy=(0.01, 0.92 - ((i + offset) * 0.05)), xycoords='axes fraction',
        #                 textcoords='offset points',
        #                 size=30,
        #                 weight="bold",
        #                 bbox=dict(boxstyle="round", fc=color_app, ec="none",alpha=0.8))
        #
        #     color_app = newcmp(i + 2)
        #     ax.annotate("  App: %i " % (i + 2),
        #                 xy=(0.16, 0.92 - ((i + offset) * 0.05)), xycoords='axes fraction',
        #                 textcoords='offset points',
        #                 size=30,
        #                 weight="bold",
        #                 bbox=dict(boxstyle="round", fc=color_app, ec="none",alpha=0.8))


        ### END INDIVIDUAL LEGEND

        ## MANUAL LEGEND FOR MIX I II III Cases

        # offset = 0
        # text = ["GetCloser"]
        # ax.annotate("%s" % text[offset],
        #             xy=(0.05, 0.96), xycoords='axes fraction',
        #             textcoords='offset points',
        #             size=34,
        #             weight="bold",
        #             bbox=dict(boxstyle="round", fc="white", ec="none"))
        # offset += 1
        #
        # for i in range(0, 2):
        #     color_app = newcmp(i + 1)
        #     ax.annotate("  App: %i " % (i + 1),
        #                 xy=(0.05+(i * 0.13), 0.90 ) , xycoords='axes fraction',
        #                 textcoords='offset points',
        #                 size=28,
        #                 # weight="bold",
        #                 bbox=dict(boxstyle="round", fc=color_app, ec="none"))
        #
        #
        # offset = 0
        # text = ["Eco_GetCloser"]
        # ax.annotate("%s" % text[offset],
        #             xy=(0.05, 0.82), xycoords='axes fraction',
        #             textcoords='offset points',
        #             size=34,
        #             weight="bold",
        #             bbox=dict(boxstyle="round", fc="white", ec="none"))
        # offset += 1
        #
        # for i in range(0, 2):
        #     color_app = newcmp(i + 3)
        #     ax.annotate("  App: %i " % (i + 3),
        #                xy=(0.05+(i * 0.13), 0.76 ), xycoords='axes fraction',
        #                 textcoords='offset points',
        #                 size=28,
        #                 # weight="bold",
        #                 bbox=dict(boxstyle="round", fc=color_app, ec="none"))
        #
        # offset = 0
        # text = ["Turbo_GetCloser"]
        # ax.annotate("%s" % text[offset],
        #             xy=(0.05, 0.68), xycoords='axes fraction',
        #             textcoords='offset points',
        #             size=34,
        #             weight="bold",
        #             bbox=dict(boxstyle="round", fc="white", ec="none"))
        # offset += 1
        #
        # for i in range(0, 2):
        #     color_app = newcmp(i + 5)
        #     ax.annotate("  App: %i " % (i + 5),
        #             xy=(0.05 + (i * 0.13), 0.62), xycoords='axes fraction',
        #             textcoords='offset points',
        #             size=28,
        #             # weight="bold",
        #             bbox=dict(boxstyle="round", fc=color_app, ec="none"))


        # Plotting users dots
        self.__draw_controlUser = {}
        nodes_with_users = self.get_nodes_with_users(routing)

        # DEBUG CODE
        # print("Nodes with users",nodes_with_users)
        # print("ROUTING",routing.controlServices)

        #PRINT ALL USERS
        for node in nodes_with_users:
            # print(node)
            for app in nodes_with_users[node]:
                self.__draw_user(node, int(app), ax, newcolors)

        # LAST step:
        # Displaying capacity, changing node shape
        trans = ax.transData.transform
        trans2 = fig.transFigure.inverted().transform
        data_occupation = self.get_nodes_with_services(sim)

        # Generate node shape
        for n in sim.topology.G.nodes():
            xx, yy = trans(self.pos[n])  # figure coordinates
            xa, ya = trans2((xx, yy))  # axes coordinates
            a = plt.axes([xa - p2, ya - p2, piesize, piesize])
            a.set_aspect('equal')

            #For labelling cells in the imshow
            nrows, ncols = data_occupation[n].shape
            real_x = np.array(range(0,ncols))
            real_y = np.array(range(0,nrows))
            if len(real_x) > 1:
                dx = (real_x[1] - real_x[0]) / 2.
            else:
                dx = 0.5
            if len(real_y)>1:
                dy = (real_y[1] - real_y[0]) / 2.
            else:
                dy = 0.5
            extent = [real_x[0] - dx, real_x[-1] + dx, real_y[0] - dy, real_y[-1] + dy]

            # Include the current instance service identificator close to the node
            a.imshow(data_occupation[n], cmap=newcmp, interpolation='none', norm=norm,extent=extent)

            # DEBUG BORDERS of the imshow
            # a.text(extent[0],extent[2], "%i,%i"%(extent[0],extent[2]), ha="center", va="center", color="yellow") #esquina inferior izquierda
            # a.text(extent[1],extent[2], "%i,%i"%(extent[1],extent[2]), ha="center", va="center", color="black") #esquina inferior derecha
            # a.text(extent[1],extent[3], "%i,%i"%(extent[1],extent[3]), ha="center", va="center", color="green") #esquina superior derecha

            sizerow = 0.5
            if nrows==0:
                sizerow = (extent[3]+abs(extent[2]))/float(nrows)

            sizecol = 0.5
            if ncols-1>0:
                sizecol = (extent[1]+abs(extent[0]))/float(ncols)

            # DEBUG
            # a.text(extent[0]+sizecol-0.5,extent[2]+sizerow, "11", ha="center", va="center", color="black") #esquina inferior izquierda
            # a.text(extent[0]+(sizecol*2)-0.5,extent[2]+sizerow, "21", ha="center", va="center", color="black") #esquina inferior izquierda
            # a.text(extent[0]+(sizecol*3)-0.5,extent[2]+sizerow, "31", ha="center", va="center", color="black") #esquina inferior izquierda
            # a.text(extent[0]+(sizecol)-0.5,extent[2]+(sizerow*2)+0.5, "12", ha="center", va="center", color="black") #esquina inferior izquierda


            for irow,row in enumerate(data_occupation[n],start=1):
                irev_row = (nrows+1)-irow
                py = extent[2]+(sizerow*irev_row)
                if irev_row>=2: py += 0.5
                for icol,value in enumerate(row,start=1):
                    if value==0: break
                    px = extent[0]+(sizecol*icol)-0.5
                    a.text(px,py, value, ha="center", va="center", color="w",weight="bold", size=18)

            a.axes.get_yaxis().set_visible(False)
            a.axes.get_xaxis().set_visible(False)

        canvas = plt.get_current_fig_manager().canvas
        canvas.draw()
        pil_image = Image.frombytes('RGB', canvas.get_width_height(), canvas.tostring_rgb())

        # pil_image.save(self.image_dir + "/network_%05d.pdf" % self.image_id)
        # pil_image.save(self.image_dir + "/network__UID%i_n%i_s%i_X_%i.png" % (self.UID, currentNode, serviceID, sim.env.now))
        pil_image.save(self.image_dir + "/network_%05d.png" % self.image_id)
        self.image_id += 1

        plt.close(fig)
        # print("Rendering fILE: %s"%(self.image_dir + "/network_%05d.png" % self.image_id))
        return self.image_dir + "/network_%05d.png" % self.image_id


    def snapshot(self,sim,path,routing):
        """
        An alternative to render for having a global view of the situation in each

        :param sim:
        :param path:
        :param routing:

        """

        if self.pos == None: # Only the first time
            self.pos = nx.get_node_attributes(sim.topology.G,'pos')
            image_dir = Path(self.path_results+"images/")
            image_dir.mkdir(parents=True, exist_ok=True)
            self.image_dir = str(image_dir)

        tab20 = plt.cm.get_cmap('tab20', self.total_services+5)
        bounds = range(self.total_services+5)
        newcolors = tab20(np.linspace(0, 1, self.total_services+5))
        newcolors[0] = np.array([250.0 / 256.0, 250. / 256., 250. / 256., 1])
        newcmp = mpl.colors.ListedColormap(newcolors)
        norm = mpl.colors.BoundaryNorm(bounds, newcmp.N)

        fig, ax = plt.subplots(figsize=(16.0, 10.0))
        plt.tight_layout(h_pad=0.55)


        nx.draw(sim.topology.G, self.pos, with_labels=False, node_size=1, node_color="#1260A0", edge_color="gray", node_shape="o",
                 font_size=7, font_color="white", ax=ax)

        width = ax.get_xlim()[1]
        top = ax.get_ylim()[1]
        # Some viz. vars.
        piesize = .078
        # piesize = .08
        p2 = piesize / 2.5

        dataApps = json.load(open(path + 'appDefinition.json'))

        # Labels on nodes
        for x in sim.topology.G.nodes:
            if x == 0:
                ax.text(self.pos[x][0]- (width/12), self.pos[x][1] , tiledTopology.getAbbrNodeName(x), fontsize=10,
                        fontweight="bold")
            else:
                ax.text(self.pos[x][0] - (width/75), self.pos[x][1] + (width/35) , tiledTopology.getAbbrNodeName(x), fontsize=10,fontweight="bold")

        if not "closers" in self.image_dir:
            legendItems = []
            for i in range(1,len(dataApps)+1):
                color_app = newcmp(i)
                legendItems.append(mpatches.Patch(color=color_app, label='App: %i'%i))
            plt.legend(loc="lower center",handles=legendItems, ncol=len(dataApps))


        # Plotting users dots
        self.__draw_controlUser = {}
        nodes_with_users = self.get_nodes_with_users(routing)

        #PRINT ALL USERS
        for node in nodes_with_users:
            # print(node)
            for app in nodes_with_users[node]:
                self.__draw_user(node, int(app), ax, newcolors)

        # LAST step:
        # Displaying capacity, changing node shape
        trans = ax.transData.transform
        trans2 = fig.transFigure.inverted().transform
        data_occupation = self.get_nodes_with_services(sim)

        # Generate node shape
        for n in sim.topology.G.nodes():
            xx, yy = trans(self.pos[n])  # figure coordinates
            xa, ya = trans2((xx, yy))  # axes coordinates
            a = plt.axes([xa - p2, ya - p2, piesize, piesize])
            a.set_aspect('equal')

            #For labelling cells in the imshow
            nrows, ncols = data_occupation[n].shape
            real_x = np.array(range(0,ncols))
            real_y = np.array(range(0,nrows))
            if len(real_x) > 1:
                dx = (real_x[1] - real_x[0]) / 2.
            else:
                dx = 0.5
            if len(real_y)>1:
                dy = (real_y[1] - real_y[0]) / 2.
            else:
                dy = 0.5
            extent = [real_x[0] - dx, real_x[-1] + dx, real_y[0] - dy, real_y[-1] + dy]

            # Include the current instance service identificator close to the node
            a.imshow(data_occupation[n], cmap=newcmp, interpolation='none', norm=norm,extent=extent)

            sizerow = 0.5
            if nrows==0:
                sizerow = (extent[3]+abs(extent[2]))/float(nrows)

            sizecol = 0.5
            if ncols-1>0:
                sizecol = (extent[1]+abs(extent[0]))/float(ncols)


            for irow,row in enumerate(data_occupation[n],start=1):
                irev_row = (nrows+1)-irow
                py = extent[2]+(sizerow*irev_row)
                if irev_row>=2: py += 0.5
                for icol,value in enumerate(row,start=1):
                    if value==0: break
                    px = extent[0]+(sizecol*icol)-0.5
                    a.text(px,py, value, ha="center", va="center", color="w",weight="bold", size=18)

            a.axes.get_yaxis().set_visible(False)
            a.axes.get_xaxis().set_visible(False)

        canvas = plt.get_current_fig_manager().canvas
        canvas.draw()
        pil_image = Image.frombytes('RGB', canvas.get_width_height(), canvas.tostring_rgb())

        pil_image.save(self.image_dir + "/snap_%05d.png" % self.snap_id)
        self.snap_id += 1
        plt.close(fig)

