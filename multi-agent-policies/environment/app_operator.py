from collections import defaultdict
from agent import PolicyManager
from yafs.distribution import *
import logging
from collections import deque

class Mario():

    def __init__(self,period=100):
        self.create_initial_services = True
        self.logger = logging.getLogger(__name__)
        self.memory = deque(maxlen=200)
        self.period = period

    def __call__(self, sim, routing,pathCSV):
        # The occupation of a node can be managed by the simulator but to easy integration with the visualization both structures are different
        if self.create_initial_services:
            self.service_calls = defaultdict(list)
            print(sim.alloc_DES)
            print(sim.alloc_module)
            for app in sim.alloc_module:
                for service in sim.alloc_module[app]:
                    for des in sim.alloc_module[app][service]:
                        logging.info("Generating a new agent control from: %s with id: %i"%(service,des))

                        period = deterministicDistribution(self.period, name="Deterministic")
                        pm = PolicyManager(des,service,pathCSV)
                        sim.deploy_monitor("Policy Manager %i"%des, pm, period, **{"sim": sim, "routing": routing})

            self.create_initial_services = False #only one time
        else:
            #TODO Control actions from all agents
            pass

    def add_new_action(self, something):
        self.memory.append((something))