import logging
from yafs.topology import *
import json
import sys
from collections import defaultdict
import pandas as pd
import numpy as np
from problogRulesGenerator import Rules
from problog.program import PrologString
from problog import get_evaluatable
from problog.tasks import sample


class PolicyManager():

    def get_app_identifier(self,nameservice):
        print("nameservice:",nameservice)
        return nameservice[0:nameservice.index("_")]

    def get_latency(self, path, topology):
        speed = 0
        for i in range(len(path) - 1):
            link = (path[i], path[i + 1])
            speed += topology.G.edges[link][Topology.LINK_PR]
        return speed

    def get_current_services(self, sim):
        """ returns a dictionary with name_service and a list of node where they are deployed
        example: defaultdict(<type 'list'>, {u'2_19': [15], u'3_22': [5]})
        """
        current_services = sim.get_alloc_entities()
        nodes_with_services = defaultdict(list)
        current_services = dict((k, v) for k, v in current_services.items() if len(v)>0)

        deployed_services = defaultdict(list)
        for k,v  in current_services.items():
            for service_name in v:
                if not "None" in service_name: #[u'2#2_19']
                    deployed_services[service_name[service_name.index("#")+1:]].append(k)
                else:
                    nodes_with_services[k].append(service_name[:service_name.index("#")])

        return deployed_services,nodes_with_services

    def __init__(self,DES,name,rules,service_rule_profile,path,app_operator):
        self.DES = DES
        self.name = name
        self.app_name = self.get_app_identifier(self.name)
        self.active = True
        self.path_csv_files = path
        self.rule_profile = service_rule_profile[self.app_name]

        self.logger = logging.getLogger(__name__)
        self.previous_number_samples = 0
        self.rules = Rules(rules)
        self.agents = {}
        self.app_operator = app_operator
        # data = json.load(open(path + 'usersDefinition.json'))

    def __call__(self, sim, routing):
        if self.active:
            self.rules.clear()
            print("Running control instance: %s: %i"%(self.name,self.DES))
            self.logger.info("Running control instance: %s: %i"%(self.name,self.DES))

            currentNode = sim.alloc_DES[self.DES]
            self.rules.and_rule("serviceInstance",self.DES,self.app_name,currentNode)

            print("\t All paths [wl-node,service-node: ",routing.controlServices)
            routes = []
            neighbours = []
            for (path,des) in routing.controlServices.values():
                if des==self.DES:
                    routes.append([self.get_latency(path, sim.topology), path])
                    neighbours += path

            neighbours += [e[1] for e in sim.topology.G.edges(currentNode)]
            neighbours = list(dict.fromkeys(neighbours))
            assert len(neighbours)>0,"Node without edges?"
            # print("All neighbours ",neighbours)
            # NODE FACTS
            node_hreqs = nx.get_node_attributes(G=sim.topology.G,name="HwReqs")
            for n in neighbours:
                n_neigh = [e[1] for e in sim.topology.G.edges(n)]
                self.rules.and_rule("node",n,node_hreqs[n],n_neigh)

            # LINK FACTS
            lat = nx.get_edge_attributes(sim.topology.G,"PR")
            bw = nx.get_edge_attributes(sim.topology.G,"BW")
            for e in sim.topology.G.edges(currentNode):
                self.rules.and_rule("link", e[0], e[1], lat[e],bw[e])


            # Getting the number of user requests == number of  messages in that path
            # we get the number of msg from a csv file
            sim.metrics.flush()
            # Loading samples generated along current period (self.activations-1,self.activation)
            df = pd.read_csv(self.path_csv_files + ".csv", skiprows=range(1, self.previous_number_samples))  # include header
            df = df[df["DES.dst"]==self.DES]
            if len(df)>0:
                # print("Number of samples: %i (from: %i)" % (len(df.index)-1, self.previous_number_samples))
                self.previous_number_samples += len(df.index) - 1  # avoid header
                if len(routes)>0:
                    # print(routes)
                    # print(df[["TOPO.src","TOPO.dst"]])
                    for r in routes:
                        assert r[1][-1] == currentNode, "Last path node and source target are different"
                        n_user = r[1][0]
                        n_messages = len(df[df["TOPO.src"] == n_user])
                        if n_messages >= 0:
                            path = Rules()
                            path.inner_rule("path", n_user, currentNode, r[1])
                            self.rules.and_rule("route", self.DES, path, r[0],n_messages)
            else:
                print("Not information yet")
                self.logger.warning("There are not messages.")


            actions = self.run_problog_model(self.rules)
            print(actions)

            #Sending the rules to the app_operator, aka MARIO
            self.app_operator.get_actions_from_agents((self.name,self.DES,currentNode,actions))
            sys.exit()


    def run_problog_model(self, rules):
        all_rules = ""
        with open(self.rule_profile, "r") as f:
            all_rules = f.read()
        # print(all_rules)
        modeltext = """
        :- use_module(library(lists)).
        %s
        """%(all_rules+"\n"+str(rules))
        print("******")
        print(modeltext)
        print("******")
        model = PrologString(modeltext)
        result = get_evaluatable().create_from(model).evaluate()
        
        return result



