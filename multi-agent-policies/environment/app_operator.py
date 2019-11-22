from collections import defaultdict
from agent import PolicyManager
from yafs.distribution import *
import logging
from collections import deque
import matplotlib.pyplot as plt
import networkx as nx
from PIL import Image
from pathlib import Path

class Mario():

    def __init__(self,rules,service_rule_profile,path_csv_files,period=100):
        self.create_initial_services = True
        self.logger = logging.getLogger(__name__)
        self.memory = deque(maxlen=200)
        self.period = period
        self.globalrules = rules
        self.service_rule_profile = service_rule_profile
        self.pos = None
        self.image_dir = None
        self.step = 0
        self.path_csv_files = path_csv_files

    def __call__(self, sim, routing, path):
        # The occupation of a node can be managed by the simulator but to easy integration with the visualization both structures are different
        if self.create_initial_services:
            self.service_calls = defaultdict(list)
            # print(sim.alloc_DES)
            # print(sim.alloc_module)
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
                # TODO Control actions from all agents
                print("MARIO IS HERE")

                pass
            else:
                pass
            self.step += 1

    def get_actions_from_agents(self, something):
        self.memory.append(something)

    def render(self,sim,path):
        if self.pos == None: #first time
            self.pos = nx.kamada_kawai_layout(sim.topology.G)  # el layout podria ser una entrada?
            image_dir = Path(path+"results/images/")
            image_dir.mkdir(parents=True, exist_ok=True)
            self.image_dir = str(image_dir)

        fig, ax = plt.subplots(figsize=(9.0, 6.0))
        left, bottom, width, height = ax.get_position().bounds
        right = left + width
        top = bottom + height

        nx.draw(sim.topology.G, self.pos, with_labels=True, node_size=200, node_color="#1260A0", edge_color="gray",
                node_shape="o",font_size=7, font_color="white", ax=ax)

        # for wl in self.workloads:
        #     ax.scatter(self.pos[wl.pos][0],self.pos[wl.pos][1], s=750.0, marker='H', color="red")
        #
        # for pos in get_services_position(self.observation_space):
        #     ax.scatter(self.pos[pos][0], self.pos[pos][1], s=500.0, marker='o', color="orange")

        ax.text(right,top, "Step: %i"%self.step,horizontalalignment='right',verticalalignment='top',
                transform=ax.transAxes,fontsize=16)
        canvas = plt.get_current_fig_manager().canvas
        canvas.draw()
        pil_image = Image.frombytes('RGB', canvas.get_width_height(), canvas.tostring_rgb())
        pil_image.save(self.image_dir+"/network_%05d.png"%self.step)
        ax.clear()

