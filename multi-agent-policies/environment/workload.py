from yafs.population import Population
from yafs.distribution import exponentialDistribution,deterministic_distribution
import math
import logging


class DynamicWorkload(Population):
    """
    We launch one user by invocation
    """
    def __init__(self, data, iteration,logger=None,**kwargs):
        super(DynamicWorkload, self).__init__(**kwargs)
        self.data = data
        self.it = iteration
        self.userOrderInputByInvocation = []
        self.times = []
        self.exits = []
        self.id_sources = []
        self.logger = logger or logging.getLogger(__name__)
        self.logger.info(" Initializating dynamic population: %s"%self.name)

    """
    In userOrderInputByInvocation, we create the user apparition sequence
    """
    def initial_allocation(self, sim, app_name):
        for item in self.data:
            self.times.append(item["start"])
            if "exit" in item:
                self.exits.append(item["exit"])
            else:
                self.exits.append(math.inf)

    """
    In each invocation, we launch one user
    """
    def run(self, sim):
        for i,time in enumerate(self.times):
            if time <= sim.env.now : #launch this issue
                self.times[i] = math.inf #only one time

                item = self.data[i]
                app_name = item["app"]
                idtopo = item["id_resource"]
                lambd = item["lambda"]

                self.logger.info("Launching user %i (app: %s), in node: %i, at time: %i " % (item["id_resource"],app_name, idtopo,sim.env.now))
                print("Launching user %i (app: %s), in node: %i, at time: %i " % (item["id_resource"],app_name, idtopo,sim.env.now))

                app = sim.apps[app_name]
                msg = app.get_message(item["message"])

                # A basic creation of the seed: unique for each user and different in each simulation repetition
                # seed = item["id_resource"] * 1000 + item["lambda"] + self.it

                # dDistribution = exponentialDistribution(name="Exp", lambd=lambd, seed=seed)
                dDistribution = deterministic_distribution(name="DET", time=lambd)
                self.id_sources.append(sim.deploy_source(app_name, id_node=idtopo, msg=msg, distribution=dDistribution))

        for i, exit in enumerate(self.exits):
            if exit <= sim.env.now:  # launch this issue
                sim.undeploy_source(self.id_sources[i])
                self.exits[i] = math.inf