from collections import defaultdict
from agent import PolicyManager
from yafs.distribution import *
import logging
from collections import deque
import matplotlib.pyplot as plt
import networkx as nx
from PIL import Image
from pathlib import Path
from matplotlib import colors
import matplotlib as mpl
import scipy
import sys
import os
import json
from collections import Counter

import matplotlib.patches as mpatches

class Mario():
    """
    We called it Mario in honour to our video game moustached character ;)

    """

    def __init__(self, common_rules, service_rule_profile, path_csv_files, app_number, period, render):
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

        #render
        self.render_action = render
        self.image_id =0
        self.__draw_controlUser = {}

        self.active_monitor = {}
        # key: id_service (DES), value: id_monitor (DES)

        #STATS
        self.dump_stats = 0

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

            self.action_stats = open(path+"results/action_stats.txt",'w+')
            self.action_stats.write("time,suicide,nop,migrate,replicate,none\n")
        else:
            if len(self.memory)>0:
                self.step += 1
                # DEBUG
                print("+"*20)
                print("\nMARIOOOOOOOOOO is here!! - Activation: %i  - Time: %i" %(self.step,sim.env.now))
                print("- Size buffer of actions: %i" % len(self.memory))
                print("- Current situation:")
                sim.print_debug_assignaments()


            if sim.env.now == 4100:
                print("DEBUG")
            # TODO v2 Control actions from all agents & select the best option
            # current implementation FCFS
            self.UID += 1
            take_last_action = [] # It perfoms the last set of agent rules
            counter_actions = Counter()

            for rule in reversed(self.memory): #(self.name,self.DES,currentNode,actions,action_priority)
                acts, prob = [], []
                # print("+ Actions from DES_service: %i"%(rule[1]))
                # print("\tService:"+rule[0])
                # print("\tID_Service:%i"%rule[1])
                # print("\tNode:%i"%rule[2])
                # print("\tActions:%s"%rule[3])

                if rule[1] not in take_last_action: # One rule for agent
                    take_last_action.append(rule[1])
                    for i,act in enumerate(rule[3]): # actions are ordered
                        # print(" ++ action #:%i rule: %s"%(i,act))
                        if act != None:
                            tuple_action = (rule[0],rule[1],rule[2],act)

                            # The render of the action is done the state of the simulator changes
                            if self.render_action:
                                image_file = self.render(sim,path,routing,tuple_action)

                            done = self.perfom_action(sim,tuple_action,routing,path)

                            # print("\t action confirmed: %s"%done)
                            # print("type: ",type(done))
                            # print(tuple_action)

                            if self.render_action and not done:
                                try:
                                    self.logger.warning("Image removed")
                                    os.remove(image_file)
                                except FileNotFoundError:
                                    None

                            if not done:
                                self.logger.debug("Action taken!: None")
                                counter_actions["none"] += 1
                            else:
                                type_action = str(act)[0:act.index("(")]
                                self.logger.debug("Action taken!: %s"%type_action)
                                counter_actions[type_action] += 1

            self.memory = []
            routing.clear_routing_cache() # Cache data is stored to improve the execution time of the simulator
            #writing stats
            if len(counter_actions)>0:
                print(counter_actions)
                line = ""
                for k in ["suicide","nop","migrate","replicate","none"]:
                    line += "%i,"%counter_actions[k]

                # print(line[:-1])
                self.action_stats.write("%i,%s\n"%(sim.env.now,line[:-1]))
                self.action_stats.flush()


    def perfom_action(self,sim,action,routing,path):
        """
        Decode the action and perfom the action in the simulator

        :param sim:
        :param action:
        :return:
        """

        # print(" + Perfom action on time: %i"%sim.env.now)
        # print("\t Service: ",action[0])
        # print("\t ID_S: ",action[1])
        # print("\t Node: ",action[2])
        # print("\t action: ",action[3])
        # print("\t ********* ")

        service = action[0]
        id_service = action[1]
        on_node = action[2]
        act = str(action[3])
        type_action = act[0:act.index("(")]
        parameters = act[len(type_action+"("):-1]

        # print("Action type: %s"%type_action)
        # print("parameters: %s"%parameters)

        if type_action == "replicate": # replicate(Si,[TargetNodes])
            # Example
            # parameters: 0,[3, 2]
            nodes_to_replicate = np.array(eval(parameters[parameters.index(",") + 1:])).astype(int)
            # parameters = parameters.replace("[","")
            # parameters = parameters.replace("]","")
            # print("Nodes where replicate: %s"%(nodes_to_replicate))

            if len(nodes_to_replicate)==0:
                self.logger.warning("WARNING: a replicate action without target nodes")
                print("WARNING: a replicate action without target nodes")
                return False
            else:
                space_on_node = self.get_free_space_on_nodes(sim)
                # All these comments made mandatory that all nodes have capacity to host the all instances.
                # feasible = True #
                nodes_with_space = []
                for n in nodes_to_replicate:
                    if space_on_node[n]<=0:
                        self.logger.warning("There is not more free space on node: %i"%n)
                        print("\t WARNING: NO FREE SPACE ON NODE:%i"%n)
                        # feasible = False
                        # break
                    else:
                        nodes_with_space.append(n)
                # if feasible:
                if True:
                    for n in nodes_with_space:
                    # for n in nodes_to_replicate:
                        self.logger.info("Action Replicate new instance of %s on node: %i" % (service, n))
                        print("\t\t+Action Replicate new instance of %s on node: %i" % (service, n))
                        self.deploy_module(sim, service, n, routing, path)
                # return feasible
                return len(nodes_with_space)>0

        elif type_action == "migrate":
            # Example
            # parameters: 0,[3, 2]
            parameters = parameters.split(",")
            try:
                target_node = int(parameters[-1])
                space_on_node = self.get_free_space_on_nodes(sim)
                feasible = True
                if space_on_node[target_node] <= 0:
                    self.logger.warning("There is not more free space on node: %i" % target_node)
                    print("\t WARNING: NO FREE SPACE ON NODE:%i" % target_node)
                    return False
                else:
                    self.logger.info("Action Migrate new instance of %s on node: %i" % (service, target_node))
                    print("\t\t+Action Migrate new instance of %s on node: %i" % (service, target_node))
                    self.deploy_module(sim, service, target_node, routing, path)
                    self.logger.info("\t Remove current instance %s on node: %i" % (service, on_node))
                    print("\t\t+Remove current instance of %s on node: %i" % (service, on_node))
                    self.undeploy_module(sim, service, on_node, id_service)
                    return True
            except:
                self.logger.critical("A migration rule with no target node")
                print("A migration rule with no target node")
                return False

        elif type_action == "suicide":
            # no arguments
            self.logger.info("Action Suicide instance %s on node: %i" % (service, on_node))
            self.undeploy_module(sim,service,on_node,id_service)
            return True

        elif type_action == "nop":
            # Doing nothing
            self.logger.info("Action NOP instance %s on node: %i" % (service, on_node))
            return True

        elif type_action == "fusion":
            return True

    def undeploy_module(self,sim,service,node,id_service):
        sim.undeploy_module(self.get_app_identifier(service), service, id_service)
        #Remove stop process
        del self.active_monitor[id_service]
        sim.stop_process(id_service)

    def deploy_module(self, sim, service, node,routing,path):
        # Allocation the module in the simulator
        app_name = self.get_app_identifier(service)
        app = sim.apps[app_name]
        services = app.services
        des = sim.deploy_module(app_name, service, services[service], [node])[0]
        # Creating a new monitor associated to the module
        self.create_monitor_of_module(des, path, routing, service, sim)

    def create_monitor_of_module(self, des, path, routing, service, sim):
        period = deterministic_distribution(self.period, name="Deterministic")
        pm = PolicyManager(des, service, self.common_rules, self.service_rule_profile, self.path_csv_files, self, self.render)


        id_monitor = sim.deploy_monitor("Policy Manager %i" % des, pm, period,
                                        **{"sim": sim, "routing": routing, "experiment_path": path})
        pm.id_monitor = id_monitor
        logging.info("Generating a new agent control from: %s with id: %i - Monitor: %i" % (service, des, id_monitor))
        print("Generating a new agent control from: %s with id: %i - Monitor: %i" % (service, des, id_monitor))
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
        line = int(total / 4) + 1
        # duy = 0.06 * line
        # dux = 0.01 * (total % 4)

        #simple policies
        duy = -0.26 * line
        dux = -0.15 * (total % 4)
        # #
        # # new
        # duy = 4.56 * line
        # dux = 2.55 * (total % 4)
        # self.__draw_controlUser[node] += 1

        ax.scatter(self.pos[node][0] + dux, self.pos[node][1] + duy, s=100.0, marker='o', color=newcolors[service])

        self.__draw_controlUser[node]+=1

    def get_app_identifier(self,nameservice):
        return int(nameservice[0:nameservice.index("_")])

    def get_nodes_with_users(self, routing):
        nodes_with_users = defaultdict(list)
        for user_service in routing.controlServices.keys():
            nodes_with_users[user_service[0]].append(self.get_app_identifier(user_service[1]))
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
                raise "Network node: %i defined with a bad shape "%node
                # print("Network node: %i defined with a bad shape "%node)
                # currentOccupation[node] = np.zeros(shape(1,1))

        return currentOccupation

    def render(self,sim,path,routing,action):
        # print("\t All paths [wl-node,service-node: ", routing.controlServices)
        # print("RENDERING  action")
        # print("\t Service: ", action[0])
        # print("\t ID_S: ", action[1])
        # print("\t Node: ", action[2])
        # print("Action: ", action[3])
        # sys.exit()

        if self.pos == None: # Only the first time
            pos = nx.get_node_attributes(sim.topology.G,'pos')
            if len(pos)>0:
                for k in pos.keys():
                    pos[k] = np.array(eval(pos[k]))
                self.pos = pos
            else:
                self.pos = nx.random_layout(sim.topology.G)

            image_dir = Path(path+"results/images/")
            image_dir.mkdir(parents=True, exist_ok=True)
            self.image_dir = str(image_dir)

        tab20 = plt.cm.get_cmap('tab20', self.total_services+5)
        bounds = range(self.total_services+5)
        newcolors = tab20(np.linspace(0, 1, self.total_services+5))
        newcolors[0] = np.array([250.0 / 256.0, 250. / 256., 250. / 256., 1])
        newcmp = mpl.colors.ListedColormap(newcolors)
        norm = mpl.colors.BoundaryNorm(bounds, newcmp.N)

        fig, ax = plt.subplots(figsize=(16.0, 10.0))
        # left, bottom, width, height = ax.get_position().bounds
        #

        nx.draw(sim.topology.G, self.pos, with_labels=False, node_size=1, node_color="#1260A0", edge_color="gray", node_shape="o",
                 font_size=7, font_color="white", ax=ax)

        width = ax.get_xlim()[1]
        top = ax.get_ylim()[1]
        # Some viz. vars.
        piesize = .06
        p2 = piesize / 2.5

        try:
            idApp = int(action[0].split("_")[0])
        except ValueError: #It triggers when the simulation ends: action(null)
            idApp = 0

        color_app = newcmp(idApp)


        ##########
        # Textual data
        ##########
        plt.text(0, top, "Simulation time: %i" % sim.env.now,{'color': color_app, 'fontsize': 12})

        info_text = "Action: %s" % action[3]
        plt.text(0 , top-0.2, info_text, {'color': color_app, 'fontsize': 14})

        info_text = "by Service: S%i on Node: N%i" % (action[1], action[2])
        plt.text(0, top -0.4, info_text, {'color': color_app, 'fontsize': 14})


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

        info_text = "App: %i with policy: %s" % (idApp, rule_policy)
        plt.text(0, top -0.6, info_text, {'color': color_app, 'fontsize': 12})

        info_text = "Debug file: rules_swi_UID%i_n%i_s%i_X_%i.pl" % (self.UID, action[2], action[1], sim.env.now)
        plt.text(0, top - 0.8, info_text, {'color': color_app, 'fontsize': 12})


        # Labels on nodes
        for x in sim.topology.G.nodes:
            ax.text(self.pos[x][0] + (width/45), self.pos[x][1] + (width/45) , "N%i" % (x), fontsize=10)

        #TODO IMPROVE THE GENERATION OF THE LEGEND according with APP & Policies
        # APP Legend
        if not "closers" in self.image_dir:
        # DEFAULT Legends apps
            legendItems = []
            for i in range(1,len(dataApps)+1):
                color_app = newcmp(i)
                legendItems.append(mpatches.Patch(color=color_app, label='App: %i'%i))
            plt.legend(handles=legendItems, title="title")

        else:
            # SPECIFIC LEGEND for experiment: I_II_III
            offset = 0
            text=["Get_Closer","Get_Closer_II","Get_Closer_III"]
            for i in range(0, len(dataApps) ):
                color_app = newcmp(i+1)
                if (i%2 == 0):
                    ax.annotate("%s" % text[offset],
                                xy=(0.95, 1.0 - ((i + offset) * 0.05)), xycoords='axes fraction',
                                textcoords='offset points',
                                size=14,
                                bbox=dict(boxstyle="round", fc="white", ec="none"))
                    offset+=1

                ax.annotate("App: %i"%(i+1),
                    xy=(0.95, 1.0-((i+offset)*0.05)), xycoords='axes fraction',
                    textcoords='offset points',
                    size=14,
                    bbox=dict(boxstyle="round", fc=color_app, ec="none"))



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
            # Include the current instance service identificator close to the node
            if idApp in data_occupation[n] and action[2]==n:
                plt.text(xa+piesize*10,ya+(piesize*30), "S%i"%action[1], {'color': newcmp(idApp), 'fontsize': 16})

            a.imshow(data_occupation[n], cmap=newcmp, interpolation='none', norm=norm)
            a.axes.get_yaxis().set_visible(False)
            a.axes.get_xaxis().set_visible(False)

        canvas = plt.get_current_fig_manager().canvas
        canvas.draw()
        pil_image = Image.frombytes('RGB', canvas.get_width_height(), canvas.tostring_rgb())
        pil_image.save(self.image_dir + "/network_%05d.png" % self.image_id)
        self.image_id += 1

        plt.close(fig)
        # print("Rendering fILE: %s"%(self.image_dir + "/network_%05d.png" % self.image_id))
        return self.image_dir + "/network_%05d.png" % self.image_id
