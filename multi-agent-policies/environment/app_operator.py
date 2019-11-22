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
                        sim.deploy_monitor("Policy Manager %i"%des, pm, period, **{"sim": sim, "routing": routing})
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
        # print("Performing action")
        # print("\t Service: ",action[0])
        # print("\t ID_S: ",action[1])
        # print("\t Node: ",action[2])
        print("\t Action: ",action[3])
        act = str(action[3])
        type_action = act[0:act.index("(")]
        parameters = act[len(type_action+"("):-1]
        print("Action:",type_action)
        print("parameters:",parameters)

        if type_action == "replicate": # replicate(Si,[TargetNodes])
            #parameters: 0,[3, 2]
            parameters = parameters.replace("[","")
            parameters = parameters.replace("]","")
            nodes_to_replicate = np.array(parameters.split(",")).astype(int)[1:]
            print(nodes_to_replicate)
            pass

        elif type_action == "nop":
            # Doing nothing
            pass

        elif type_action == "migrate": # migrate(Si,TargetNode,MaxLatency):-
            pass

        elif type_action == "replicate":
            pass

        elif type_action == "suicide":
            pass

        elif type_action == "fusion":
            pass



    def get_actions_from_agents(self, something):
        """
        Agents write in this buffer

        :param something:
        :return:
        """
        self.memory.append(something)


    def __draw_showUser(self,node, service,ax,newcolors):
        piesize = .05
        p2 = piesize / 2.
        if node not in self.__draw_controlUser.keys():
            self.__draw_controlUser[node] = 0

        total = self.__draw_controlUser[node]
        line = int(total / 4) + 1
        duy = 0.2 * line
        dux = 0.15 * (total % 4)
        self.__draw_controlUser[node] += 1
        ax.scatter(node[0] + duy, node[1] + dux, s=100.0, marker='o', color=newcolors[service])


    # TODO RENDER
    def render(self,sim,path):
        if self.pos == None: #first time
            self.pos = nx.kamada_kawai_layout(sim.topology.G)  # el layout podria ser una entrada?
            image_dir = Path(path+"results/images/")
            image_dir.mkdir(parents=True, exist_ok=True)
            self.image_dir = str(image_dir)

        # Some decoration vars.
        piesize = .05
        p2 = piesize / 2.
        tab20 = plt.cm.get_cmap('tab20', self.total_services)
        bounds = range(self.total_services)
        newcolors = tab20(np.linspace(0, 1, self.total_services))
        newcolors[0] = np.array([250.0 / 256.0, 250. / 256., 250. / 256., 1])
        newcmp = mpl.colors.ListedColormap(newcolors)
        norm = mpl.colors.BoundaryNorm(bounds, newcmp.N)
        #

        fig, ax = plt.subplots(figsize=(16.0, 10.0))
        # Nodes + Egdes
        plt.text(4., -2., "Step: %i" % self.step, {'color': 'black', 'fontsize': 16})

        nx.draw(sim.topology.G, self.pos, with_labels=False, node_size=200, node_color="#1260A0", edge_color="gray", node_shape="o",
                font_size=7, font_color="white", ax=ax)

        # Labels on nodes
        for x in sim.topology.G.nodes:
                ax.text(self.pos[x][0] + piesize * 2.5, self.pos[x][1] + piesize * 7.5, "N%i" % (x), fontsize=8)

        # Plotting users dots
        self.__draw_controlUser = {}

        # TODO
        # for node in nodes_with_users:
        #     for app in nodes_with_users[node]:
        #         for i, v in enumerate(self.__my_hash_service_map.keys()):#getting the index of the app #TODO improve
        #             if "%s_"%app in v:
        #                 break
        #         self.__draw_showUser(self.pos[node], i+1, ax, newcolors)

        # LAST STEP ALWAYS: to maintain coordinates
        # Displaying capacity, changing node shape
        trans = ax.transData.transform
        trans2 = fig.transFigure.inverted().transform
        for n in sim.topology.G.nodes():
            xx, yy = trans(self.pos[n])  # figure coordinates
            xa, ya = trans2((xx, yy))  # axes coordinates

            a = plt.axes([xa - p2, ya - p2, piesize, piesize])
            a.set_aspect('equal')

            shape = np.array(eval(sim.topology.G.nodes[n]["capacity"]))
            # TODO
            #
            # if n in self.__currentOccupation:
            #     occ = self.__currentOccupation[n]
            # else:
            #     occ = np.array(eval((sim.topology.G.nodes[n]["occupation"])))
            #
            # data = occ.reshape(shape)
            #    data=[[1.,3.,1.]]
            data =[[1]]

            a.imshow(data, cmap=newcmp, interpolation='none', norm=norm)
            #    a.axis('off')
            a.axes.get_yaxis().set_visible(False)
            a.axes.get_xaxis().set_visible(False)


        #plt.text(2, 1000, "Step: %i" % self.activation, {'color': 'C0', 'fontsize': 16})

        canvas = plt.get_current_fig_manager().canvas
        canvas.draw()
        pil_image = Image.frombytes('RGB', canvas.get_width_height(), canvas.tostring_rgb())
        pil_image.save(self.image_dir + "/network_%05d.png" % self.step)
        ax.clear()
        plt.close(fig)

