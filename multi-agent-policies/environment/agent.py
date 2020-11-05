import logging
from yafs.topology import *
import json
import sys
from collections import defaultdict
import pandas as pd
from problogRulesGenerator import Rules
from problog.program import PrologString
from problog import get_evaluatable
from pathlib import Path
from subprocess import Popen, PIPE
import re

PROBLOG = False #TODO def like global var on the project

class PolicyManager():

    def get_app_identifier(self,nameservice):
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

    def __init__(self,DES,name,rules,service_rule_profile,path,app_operator,render):
        self.id_monitor = None
        self.DES = DES #Service ID
        self.name = name
        self.app_name = int(self.get_app_identifier(self.name))
        self.active = True
        self.path_csv_files = path

        self.rule_profile = service_rule_profile[self.app_name]

        self.logger = logging.getLogger(__name__)
        self.previous_number_samples = 0
        self.rules = Rules(rules)
        self.agents = {}
        self.app_operator = app_operator
        self.action_on_render =0

        self.render_action = render
        # data = json.load(open(path + 'usersDefinition.json'))


    def get_free_space_on_nodes(self,sim):
        currentOccupation = dict([a, int(x)] for a,x in nx.get_node_attributes(G=sim.topology.G, name="HwReqs").items())

        sim.print_debug_assignaments()
        for app in sim.alloc_module:
            dict_module_node = sim.alloc_module[app]  # modules deployed
            for module in dict_module_node:
                for des in dict_module_node[module]:
                    currentOccupation[sim.alloc_DES[des]] -= 1
        return currentOccupation

    """
    When a new instance of a service is deployed in the infrastructure, a DES process will be generated within the simulator that manages the facts of the logical model.
    This DES process is an instance of agent. 
    The _call_ function is called when requested by the app_operator.
    """
    def __call__(self, sim, routing, experiment_path, path_results):
        
        if self.id_monitor in self.app_operator.active_monitor.values():# Once a process is finished, the service may run for the last time. The simulator does not control this last call.
            self.rules.clear()
            print("\nMonitor ID: %i for service: %i (%s)  running rules "%(self.id_monitor,self.DES,self.name))
            self.logger.info("\nMonitor ID: %i for service: %i (%s)  running rules "%(self.id_monitor,self.DES,self.name))

            # print("PREVIOUS SERVICES")
            # print(self.app_operator.active_monitor)

            currentNode = sim.alloc_DES[self.DES]
            self.rules.and_rule("serviceInstance",self.DES,self.app_name,currentNode)

             # print("\t All paths [wl-node,service-node: ",routing.controlServices)
            alllinkedNodes = []
            routes = []
            neighbours = [currentNode]
            for (path,des) in routing.controlServices.values():
                if des==self.DES:
                    routes.append([self.get_latency(path, sim.topology), path])
                    alllinkedNodes.append(path)
                    # neighbours += path # Uncomment in case of considering nodes from user-paths

            ## DIRECT NODES FACTS
            # neighbours += [e[1] for e in sim.topology.G.edges(currentNode)]
            # neighbours = list(dict.fromkeys(neighbours))
            # assert len(neighbours)>0,"Node without edges?"
            # # print("All neighbours ",neighbours)
            # # NODE FACTS
            # node_hreqs = nx.get_node_attributes(G=sim.topology.G,name="HwReqs")
            # for n in neighbours:
            #     n_neigh = [e[1] for e in sim.topology.G.edges(n)]
            #     self.rules.and_rule("node",n,node_hreqs[n],n_neigh)

            #Generating NODE facts from all the nodes in the path
            node_hreqs = nx.get_node_attributes(G=sim.topology.G, name="HwReqs")

            #INFO: free space on a node
            # available_space_on_node = self.get_free_space_on_nodes(sim)


            setNodes = set()
            for path in alllinkedNodes:
                for node in path:
                    setNodes.add(node)

            setNodes.add(currentNode) # at least the own node is there

            for n in setNodes:
                neighbours += [e[1] for e in sim.topology.G.edges(n)]
                neighbours = list(dict.fromkeys(neighbours))
                n_neigh = [e[1] for e in sim.topology.G.edges(n)]
                self.rules.and_rule("node", n, node_hreqs[n], n_neigh)


            # LINK FACTS
            lat = nx.get_edge_attributes(sim.topology.G,"PR")
            bw = nx.get_edge_attributes(sim.topology.G,"BW")

            for e in sim.topology.G.edges(currentNode):
                try:
                    self.rules.and_rule("link", e[0], e[1], lat[e],bw[e])
                except KeyError:
                    # G is not a directional Graph
                    # it doesn't have to be, but it doesn't find the shortestpath
                    e = (e[1],e[0])
                    self.rules.and_rule("link", e[0], e[1], lat[e], bw[e])


            # Getting the number of user requests == number of  messages in that path
            # we get the number of msg from a csv file
            sim.metrics.flush()
            # Loading samples generated along current period (self.activations-1,self.activation)
            df = pd.read_csv(self.path_csv_files + ".csv", skiprows=range(1, self.previous_number_samples))  # include header
            self.previous_number_samples += len(df.index) - 1  # avoid header
            df = df[df["DES.dst"]==self.DES]
            if len(df)>0:
                # print("Number of samples: %i (from: %i)" % (len(df.index)-1, self.previous_number_samples))
                if len(routes)>0:
                    # print(df[["TOPO.src","TOPO.dst"]])
                    for r in routes:
                        assert r[1][-1] == currentNode, "Last path node and source target are different"
                        n_user = r[1][0] # The last node. It is not the ID-user.
                        n_messages = len(df[df["TOPO.src"] == n_user])
                        if n_messages >= 0:
                            path = Rules()

                            #Example: path(4, 1, [2, 3, 5])
                            # the service instance is on node 4
                            # the user is on node 1
                            # the path of nodes between both is the array: [4,2,3,5,1]
                            # r[1] contains the reversed path and contains the initial and end node, we need to remove them

                            #Initial v0
                            #path.inner_rule("path", currentNode, n_user, r[1][::-1][1:])

                            # Example: path(4, 1, [2, 3, 5])
                            # the path of nodes between both is the array: [4,2,3,5,1]
                            path.inner_rule("path", r[1][::-1])

                            print("PATH: %s"%path)
                            print(r[0])
                            print(n_messages)

                            self.rules.and_rule("route", self.DES, path, r[0], n_messages)
            else:
                print("INFO - No messages among users and service")
                self.logger.warning("INFO - There are not new messages among users and service")

            self.logger.info("Performing problog model")

            # if PROBLOG:
            #     actions = self.run_problog_model(self.rules,self.DES,currentNode,experiment_path)
            # else:
            action = self.run_swi_model(self.rules,self.DES,currentNode,path_results,sim)

            #Sending the rules to the app_operator, aka MARIO
            # print("Sending new rules to MARIO: %s",actions)
            self.app_operator.get_actions_from_agents((self.name,self.DES,currentNode,[action]))

    def run_swi_model(self, facts, service_name, current_node, path_results, sim):
        # Load policy rules
        rule_file = ""
        with open(self.rule_profile, "r") as f:
            rule_file = f.read()

        rules_and_facts = ":- initialization(main).\n" \
                          ":- discontiguous route/4.\n\n" \
                          "%s\n%s\n"%(rule_file,str(facts))

        # Write rules and facts in a Prolog file *.pl
        rules_dir = Path(path_results + "models/")
        rules_dir.mkdir(parents=True, exist_ok=True)
        rules_dir = str(rules_dir)
        model_file = rules_dir + "/rules_swi_UID%i_n%s_s%s_%i_%i.pl" % (self.app_operator.UID + 1, current_node, service_name, self.action_on_render,sim.env.now)
        self.action_on_render += 1

        with open(model_file, "w") as f:
            f.write(rules_and_facts)
            f.write("main :- current_prolog_flag(argv, Argv),\n" \
                    "  nth0(0, Argv, Argument0),\n" \
                    "  atom_number(Argument0, Arg0),\n"\
                    "  action(Arg0,ACTION,M),\n"\
                    "  format('~q,~q~n', [ACTION,M]),\n"\
                    "  halt.\n"\
                    "main :-\n"\
                    "  halt(1).\n")

        try:

            # from pyswip import Prolog
            # # swipl - -dump - runtime - variables
            # prolog = Prolog()
            #
            # # print(model_file)
            #
            # prolog.consult(model_file)
            # action_query = list(prolog.query("action(%i,ACTION,P)"%service_name))

            cmd = ["swipl",model_file,str(service_name)]
            # cmd = ["swipl","%s/%s"%(gwc,model_file),service_name]
            p = Popen(cmd, stdout=PIPE, stderr=PIPE)
            stdout, stderr = p.communicate()

            output = stdout.decode("utf-8")
            action = output.split(",")[0]
            params = output.split(",")[1:]

            if action == "suicide":
               action = 'suicide(%i)' % service_name
            elif action == "nop":
               action = 'nop(%i)' % service_name
            elif action== "migrate":
               action = 'migrate(%i,X,%s)' % (service_name,params)
            elif action  == "replicate":
               action = 'replicate(%i,%s)' % (service_name, params)
            else:
               self.logger.critical("An action is not defined")
               print("An action is not defined")

            return action

        except:
            raise "Error running PYSWIP model on file: %s" % model_file

    def render(self,service_name,current_node,modeltext,experiment_path):
        """
        write the model into a file

        :param service_name:
        :param current_node:
        :param modeltext:
        :param experiment_path:
        :return:
        """
        rules_dir = Path(experiment_path + "prolog_facts/")
        rules_dir.mkdir(parents=True, exist_ok=True)
        rules_dir = str(rules_dir)

        with open(rules_dir+"/rules_UID%i_n%s_s%s_%i.pl"%(self.app_operator.UID+1,current_node,service_name,self.action_on_render),"w") as f:
            f.write(modeltext)

        self.action_on_render +=1
