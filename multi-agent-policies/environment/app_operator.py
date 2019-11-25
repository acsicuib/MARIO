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

class Mario():

    def __init__(self,rules,service_rule_profile,path_csv_files,app_number,period=100):
        self.create_initial_services = True
        self.logger = logging.getLogger(__name__)
        self.memory = []
        # self.memory = deque(maxlen=200)
        self.period = period
        self.globalrules = rules
        self.service_rule_profile = service_rule_profile
        self.pos = None
        self.image_dir = None
        self.step = 0
        self.path_csv_files = path_csv_files
        self.total_services = app_number

        #render
        self.image_id =0
        self.__draw_controlUser = {}

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
                        logging.info("Generating a new agent control from: %s with id: %i"%(service,des))
                        period = deterministicDistribution(self.period, name="Deterministic")
                        pm = PolicyManager(des,service,self.globalrules,self.service_rule_profile,self.path_csv_files,app_operator=self)
                        sim.deploy_monitor("Policy Manager %i"%des, pm, period, **{"sim": sim, "routing": routing, "experiment_path":path})
            self.create_initial_services = False #only one time
        else:
            if len(self.memory)>0:
                # TODO Control actions from all agents & select the best option
                print("MARIO IS HERE ")
                acts, prob = [], []
                for rule in self.memory: #(self.name,self.DES,currentNode,actions)
                    # print("\tService:"+rule[0])
                    # print("\tID_Service:%i"%rule[1])
                    # print("\tNode:%i"%rule[2])
                    # print("\tActions:%s"%rule[3])
                    for action in rule[3]:
                        acts.append((rule[0],rule[1],rule[2],action[0]))
                        prob.append(action[1])

                # TODO get the priorities of tha agent
                prob = np.array(prob)
                prob = [v/prob.sum() for v in prob] #May be there are problems when the sum>=1 by decimals
                act_index = range(len(acts))
                rnd_index_action = np.random.choice(np.array(act_index),1, p=prob)
                action = acts[rnd_index_action[0]]

                self.render(sim,path,routing,action)

                self.perfom_action(sim,action)
                self.memory = []

            else:
                pass
            self.step += 1

    def perfom_action(self,sim,action):
        """
        Decode the action and perfom the action in the simulator

        :param sim:
        :param action:
        :return:
        """
        print("***"*10)
        print("Performing action")
        print("\t Service: ",action[0])
        print("\t ID_S: ",action[1])
        print("\t Node: ",action[2])
        print("\t action: ",action[3])

        service = action[0]
        on_node = action[2]
        act = str(action[3])
        type_action = act[0:act.index("(")]
        parameters = act[len(type_action+"("):-1]

        # print("\t\tname action: %s"%type_action)
        # print("\t\tparameters: %s"%parameters)

        if type_action == "replicate": # replicate(Si,[TargetNodes])
            # Example
            # parameters: 0,[3, 2]
            parameters = parameters.replace("[","")
            parameters = parameters.replace("]","")

            if len(parameters)<=1:
                print("WARNING: a replicate-command without parameters")
            else:
                nodes_to_replicate = np.array(parameters.split(",")).astype(int)[1:]
                for n in nodes_to_replicate:
                    self.logger.info("Action Replicate New instance of %s on node: %i"%(service,n))
                    print("\t\t+Action Replicate New instance of %s on node: %i"%(service,n))
                    self.deploy_module(sim,service,n)
                    #TODO Crear agente
            pass

        elif type_action == "nop":
            # Doing nothing
            self.logger.info("Action NOP instance %s on node: %i" % (service, on_node))
            pass

        elif type_action == "migrate": # migrate(Si,TargetNode,MaxLatency):-

            pass

        elif type_action == "replicate":
            pass

        elif type_action == "suicide":
            # no arguments
            self.logger.info("Action Suicide instance %s on node: %i" % (service, on_node))
            sim.undeploy_module(self.get_app_identifier(service),service, on_node)
            # TODO REMOVE agente

            pass

        elif type_action == "fusion":
            pass

    def deploy_module(self, sim, service, node):
        app_name = self.get_app_identifier(service)
        app = sim.apps[app_name]
        services = app.services
        return sim.deploy_module(app_name, service, services[service], [node])


    # sim.un_deploy_module(self,sim,service_name,node):

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
        duy = 0.12 * line
        dux = 0.06 * (total % 4)
        self.__draw_controlUser[node] += 1
        ax.scatter(self.pos[node][0] + dux, self.pos[node][1] + duy, s=100.0, marker='o', color=newcolors[service])


    def get_app_identifier(self,nameservice):
        return nameservice[0:nameservice.index("_")]

    def get_nodes_with_users(self, routing):
        nodes_with_users = defaultdict(list)
        for user_service in routing.controlServices.keys():
            nodes_with_users[user_service[0]].append(self.get_app_identifier(user_service[1]))
        return nodes_with_users

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
                print("Network node: %i defined with a bad shape "%node)
                currentOccupation[node] = np.zeros(shape(1,1))

        return currentOccupation


    def render(self,sim,path,routing,action):
        # print("\t All paths [wl-node,service-node: ", routing.controlServices)
        print("Performing action")
        print("\t Service: ", action[0])
        print("\t ID_S: ", action[1])
        print("\t Node: ", action[2])
        print("Action: ", action[3])

        if self.pos == None: #first time
            # self.pos = nx.kamada_kawai_layout(sim.topology.G)  # el layout podria ser una entrada?
            self.pos = nx.random_layout(sim.topology.G)  # el layout podria ser una entrada?
            image_dir = Path(path+"results/images/")
            image_dir.mkdir(parents=True, exist_ok=True)
            self.image_dir = str(image_dir)

        # Some viz. vars.
        piesize = .08
        p2 = piesize / 2.5

        tab20 = plt.cm.get_cmap('tab20', self.total_services+5)
        bounds = range(self.total_services+5)
        newcolors = tab20(np.linspace(0, 1, self.total_services+5))
        newcolors[0] = np.array([250.0 / 256.0, 250. / 256., 250. / 256., 1])
        newcmp = mpl.colors.ListedColormap(newcolors)
        norm = mpl.colors.BoundaryNorm(bounds, newcmp.N)

        fig, ax = plt.subplots(figsize=(16.0, 10.0))
        plt.text(0.5, 1.1, "Step: %i" % self.step, {'color': 'black', 'fontsize': 16})
        action_text = "Node N%i + Service: %i(%s) -> Action: %s" % (action[2], action[1], action[0], action[3])
        plt.text(0.5,1.,action_text, {'color': 'black', 'fontsize': 14})

        nx.draw(sim.topology.G, self.pos, with_labels=False, node_size=1, node_color="#1260A0", edge_color="gray", node_shape="o",
                font_size=7, font_color="white", ax=ax)

        # Labels on nodes
        for x in sim.topology.G.nodes:
            ax.text(self.pos[x][0] + p2, self.pos[x][1] + p2 , "N%i" % (x), fontsize=10)

        # Plotting users dots
        self.__draw_controlUser = {}
        nodes_with_users = self.get_nodes_with_users(routing)
        # print("Nodes with users",nodes_with_users)
        for node in nodes_with_users:
            for app in nodes_with_users[node]:
                self.__draw_user(node, int(app), ax, newcolors)

        # LAST step:
        # Displaying capacity, changing node shape
        trans = ax.transData.transform
        trans2 = fig.transFigure.inverted().transform
        data_occupation = self.get_nodes_with_services(sim)
        for n in sim.topology.G.nodes():
            xx, yy = trans(self.pos[n])  # figure coordinates
            xa, ya = trans2((xx, yy))  # axes coordinates
            a = plt.axes([xa - p2, ya - p2, piesize, piesize])
            a.set_aspect('equal')
            a.imshow(data_occupation[n], cmap=newcmp, interpolation='none', norm=norm)
            a.axes.get_yaxis().set_visible(False)
            a.axes.get_xaxis().set_visible(False)

        #plt.text(2, 1000, "Step: %i" % self.activation, {'color': 'C0', 'fontsize': 16})

        canvas = plt.get_current_fig_manager().canvas
        canvas.draw()
        pil_image = Image.frombytes('RGB', canvas.get_width_height(), canvas.tostring_rgb())
        pil_image.save(self.image_dir + "/network_%05d.png" % self.image_id)
        self.image_id += 1

        plt.close(fig)

        #TODO DEBUG
        if self.image_id >5:
            sys.exit()
