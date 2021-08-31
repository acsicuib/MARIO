import pandas as pd
from pathlib import Path
from collections import defaultdict
from subprocess import Popen, PIPE, TimeoutExpired

from problogRulesGenerator import Rules
from yafs.topology import *

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

    def __init__(self,DES,name,rules,service_rule_profile,path,app_operator,render,radius,reversepath,iteration):
        self.id_monitor = None
        self.DES = DES #Service ID
        self.name = name
        self.app_name = int(self.get_app_identifier(self.name))
        self.active = True
        self.path_csv_files = path

        self.rule_profile = service_rule_profile[self.app_name]

        self.logger = logging.getLogger(__name__)
        self.load_samples_from = 0
        self.rules = Rules(rules)
        self.agents = {}
        self.app_operator = app_operator
        self.action_on_render =0

        self.render_action = render

        self.radius = radius
        self.reversepath = reversepath
        # data = json.load(open(path + 'usersDefinition.json'))
        self.iteration = iteration

    def get_free_space_on_nodes(self,sim):
        currentOccupation = dict([a, int(x)] for a,x in nx.get_node_attributes(G=sim.topology.G, name="HwReqs").items())

        for app in sim.alloc_module:
            dict_module_node = sim.alloc_module[app]  # modules deployed
            for module in dict_module_node:
                for des in dict_module_node[module]:
                    size = sim.get_size_service(app,sim.alloc_level[des])
                    currentOccupation[sim.alloc_DES[des]] -= size
        return currentOccupation


    def __call__(self, sim, routing, experiment_path, path_results):
        """
        This is the main function of the AGENT-service-deployed that manages the facts of the logical/prolog model.

        When a new instance of a service is deployed in the infrastructure, a DES process is generated in the simulator.
        The DES process is an instance of this class.
        Each agent runs this function to obtain the list of operations/action and it communicates to the appOperator.

        :param sim:
        :param routing:
        :param experiment_path:
        :param path_results:
        :return: None
        """
        
        if self.id_monitor in self.app_operator.active_monitor.values():# Once a process is finished, the service may run for the last time. The simulator does not control this last call.
            self.rules.clear()
            # print("\n(agent.py) Monitor ID: %i for service: %i (%s)  running rules "%(self.id_monitor,self.DES,self.name))
            self.logger.debug("\nMonitor ID: %i for service: %i (%s)  running rules "%(self.id_monitor,self.DES,self.name))

            # print("SERVICE")
            # print(self.app_operator.active_monitor)

            # current node where the service with id (self.DES) is deployed.
            currentNode = sim.alloc_DES[self.DES]
            level = sim.alloc_level[self.DES]


            ################################################################################
            # SERVICEINSTANCE fact
            # serviceInstance(ServiceInstanceId, ServiceId, Node).
            ################################################################################
            message_level = "(%s,%i,%i)"%(level,sim.get_size_service(self.app_name, level),sim.get_speed_service(self.app_name,level))


            self.rules.and_rule("serviceInstance","s%i"%self.DES,"app%i"%self.app_name,message_level,"self")

            ################################################################################
            # NODE fact
            # node(NodeId, AvailableHW, Neighbours).
            ################################################################################

            # Data about HWReqs of a node
            node_hreqs = nx.get_node_attributes(G=sim.topology.G, name="HwReqs")
            # Data about available space of a node
            available_space_on_node = self.get_free_space_on_nodes(sim)

            ## LIST OF NODES to the CurrentNode from PATH REQUEST
            # neighbours = [currentNode]
            # alllinkedNodes = []
            # requests = [] #(latency of the path, path)
            # # print("\t All paths [wl-node,service-node: ",routing.controlServices)
            # for (path,des) in routing.controlServices.values():
            #     if des==self.DES:
            #         requests.append([self.get_latency(path, sim.topology), path])
                    # alllinkedNodes.append(path)
                    # neighbours += path # Uncomment in case of considering nodes from user-paths
            # allRelatedNodes = set()
            # for path in alllinkedNodes:
            #     for node in path:
            #         allRelatedNodes.add(node)
            # allRelatedNodes.add(currentNode)  # at least the own node is there
            # for n in allRelatedNodes:
            #     neighbours += [e[1] for e in sim.topology.G.edges(n)]
            #     neighbours = list(dict.fromkeys(neighbours))
            #     n_neigh = [e[1] for e in sim.topology.G.edges(n)]
            #     self.rules.and_rule("node", n, available_space_on_node[n], n_neigh)

            # print("HERE")
            # print(currentNode)

            # Generate the node fact of the current node
            neighbours = [e[1] for e in sim.topology.G.edges(currentNode)]
            neighbours = list(dict.fromkeys(neighbours))
            neighbours = ["n%i"%x for x in neighbours]
            self.rules.and_rule("node", "self", available_space_on_node[currentNode], neighbours)

            # Generate the node fact from neighbourds
            nodesRadius = nx.single_source_shortest_path_length(sim.topology.G, source=currentNode, cutoff=self.radius)
            for n in nodesRadius:
                if n != currentNode:
                    neighbours = [e[1] for e in sim.topology.G.edges(n)]
                    neighbours = list(dict.fromkeys(neighbours))
                    neighbours = ["n%i"%x for x in neighbours]
                    self.rules.and_rule("node", "n%i"%n, available_space_on_node[n], neighbours)



            ## LIST OF DIRECT NODES to the CurrentNode
            # neighbours = [e[1] for e in sim.topology.G.edges(currentNode)]
            # neighbours = list(dict.fromkeys(neighbours))
            # for n in neighbours:
            #     self.rules.and_rule("node", n, available_space_on_node[n], [])
            #
            # self.rules.and_rule("node", currentNode, available_space_on_node[currentNode], neighbours)



            ################################################################################
            # LINK fact (DEPRECATED)
            # IN MARIO v1.3 these facts are removed.
            ################################################################################
            # lat = nx.get_edge_attributes(sim.topology.G,"PR")
            # bw = nx.get_edge_attributes(sim.topology.G,"BW")
            #
            # for e in sim.topology.G.edges(currentNode):
            #     try:
            #         self.rules.and_rule("link", e[0], e[1], lat[e],bw[e])
            #     except KeyError:
            #         # G is not a directional Graph
            #         # it doesn't have to be, but it doesn't find the shortestpath
            #         e = (e[1],e[0])
            #         self.rules.and_rule("link", e[0], e[1], lat[e], bw[e])



            ################################################################################
            # REQUEST fact
            # requests(ServiceInstanceId, Neighbour, RequestRate, LatencyToClient).
            ################################################################################
            # The request rate is the number of  messages in the path's request
            # To obtain the the number of msg get need to analyse the simulation results on the csv file
            sim.metrics.flush()
            # We only load samples that are generated along current period (self.activations-1,self.activation)
            df = pd.read_csv(self.path_csv_files + ".csv", skiprows=range(1,self.load_samples_from))  # It includes the csv header
            self.load_samples_from += len(df.index)   # avoid header

            # print("AGENT LOADING REQUESTS")
            # print("SERVICE DES:",self.DES)
            # print("TIME:",sim.env.now)
            # print("OFFSET: ",self.load_samples_from)
            # print("DF len : ",len(df))
            # print("REQUEST ",requests)

            df = df[df["DES.dst"] == self.DES]

            # print("DF2 len : ", len(df))

            #TODO debug
            # if self.DES == 16:
            #     df.to_csv("test16_%i.csv"%sim.env.now)

            if len(df) > 0:
                #WARNING . If the topology changes (ie. node failure) this code should be checked again
                dg = df.groupby("TOPO.src")["TOPO.src"].count()
                for node_src, sumMessages in dg.iteritems():
                    path = routing.get_path_from_src_dst(sim,node_src,currentNode)

                    latency = self.get_latency(path,sim.topology)
                    if sumMessages > 0:
                        if len(path)==1:
                            # node_code= ["self"]
                            node_name = ["self"]
                        else:
                            node_code = path[-2]
                            if (self.reversepath + 1) > len(path)-1:
                                node_code = path[0:-1]
                            else:
                                node_code = path[-(self.reversepath+1):-1]
                            node_name = ["n%i"%x for x in node_code]
                        self.rules.and_rule("requests", "s%i"%self.DES, node_name, sumMessages, latency)
            else:
                # print("INFO - No messages among users and service")
                self.logger.warning("WARN - There are not new messages among users and service")

            ################################################################################
            # Action history from AppOperator (MARIO)
            # lastOutcome((OperationName,Si,Node), Result).
            ################################################################################
            if self.DES in self.app_operator.agent_communication:
                for (prevOperation,status) in self.app_operator.agent_communication[self.DES]:
                    action, service_id, onNode = prevOperation
                    self.rules.and_rule("refused",action, "s%i"%service_id, "n%i"%onNode)



            ################################################################################
            # RUN the model
            ################################################################################
            actions = self.run_prolog_model(self.rules, self.DES, currentNode, path_results, sim)


            ################################################################################
            # The agent communicates to the appOperator (MARIO) its operations
            ################################################################################

            # print(" ACTIONS FROM SERVICE: %i in NOde: %i, SIM.TIME: %i"%(self.DES,currentNode,sim.env.now))
            # print(actions)
            # print("*"*10)

            #the 3rd field is the nodeID
            priorityActionIndex = 0
            action = actions[priorityActionIndex]

            if "_" in action[2]: #TODO improve with the final version of the rules
                node_requests_alloc = "self"
            else:
                node_requests_alloc = action[2]

            if node_requests_alloc =="self":
                node_requests_alloc = currentNode
            else:
                node_requests_alloc = node_requests_alloc.replace("n","")

            # print(node_requests_alloc)
            node_requests_alloc = int(node_requests_alloc)

            self.app_operator.get_actions_from_agents(node_requests_alloc,(self.name,self.DES,currentNode,action))


    def run_prolog_model(self, facts, serviceID, current_node, path_results, sim):
        ###
        ### MARIO v.1.3.
        ###
        ## It prepares the file (.pl) to run it in a terminal command


        # External pl files to be included in the rules
        #loadOtherPLModels = ["lib", "agentRequests"]
        loadOtherPLModels = ["interfaceAMRequests"]

        pathparts = self.rule_profile.split("/")
        # Include other models
        rule_file = ""
        for other in loadOtherPLModels:
            pathModel = "/".join(pathparts[0:-1]) + "/%s.pl"%other
            with open(pathModel, "r") as f:
                rule_file += f.read()

        # Include policy agent
        with open(self.rule_profile, "r") as f:
            rule_file += f.read()

        # Include agent facts
        rules_and_facts = ":- initialization(main).\n\n " \
                          "%s\n%s\n"%(rule_file,str(facts))

        # Write rules and facts in a Prolog file *.pl
        rules_dir = Path(path_results + "models_%i/"%self.iteration)
        rules_dir.mkdir(parents=True, exist_ok=True)
        rules_dir = str(rules_dir)
        model_file = rules_dir + "/model_B%i_n%s_DES%s_%i_%i.pl" % (self.app_operator.UID + 1, current_node, serviceID, self.action_on_render, sim.env.now)

        # print(model_file)
        self.action_on_render += 1

        with open(model_file, "w") as f:
            f.write(rules_and_facts)
            f.write("\nmain :- current_prolog_flag(argv, Argv),\n" \
                    "  nth0(0, Argv, Argument0),\n" \
                    # "  atom_number(Argument0, ServiceID),\n"\
                    "  operations(Argument0,RequestedActions),\n"\
                    "  format('~q~n', [RequestedActions]),\n"\
                    "  halt.\n"\
                    "main :-\n"\
                    "  halt(1).\n")


        ### Run the model using swipl command on the terminal
        try:
            cmd = ["swipl", model_file, "s"+str(serviceID)]

            # print(cmd)
            p = Popen(cmd, stdout=PIPE, stderr=PIPE)
            stdout, stderr = p.communicate(timeout=10)


            expr = stdout.decode("utf-8")
            expr = expr.replace("\n","")


            it = iter("".join(c for c in expr if c not in "()[] ").split(","))
            result = [(x, y, z, v ) for x, y, z, v in zip(it, it, it, it)]
            ## result == [('migrate', '2', 'n0lt0ln0'), ('replicate', '2', 'n0lt0ln0')]

            # print("*-*-" * 5)
            # print("ServiceID: ",serviceID)
            # print("model_file: ",model_file)
            # print("Actions :",result)
            # print("*-*-"*5)

            assert len(result)>=0, "(agent.py) Prolog return is incorrect"

            result.append(("nop", "s%i"%serviceID, "n%i"%current_node, "UKN"))
            return result

        except TimeoutExpired as err:
            p.terminate()
            raise Exception("Error running PYSWIP model on file: %s - TIMEOUT EXPERIED" % model_file)
        # except Exception as err:
        #     print(err)
        #     raise Exception("Error running PYSWIP model on file: %s" % model_file)
