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
            print("\n(agent.py) Monitor ID: %i for service: %i (%s)  running rules "%(self.id_monitor,self.DES,self.name))
            self.logger.info("\nMonitor ID: %i for service: %i (%s)  running rules "%(self.id_monitor,self.DES,self.name))

            # print("SERVICE")
            # print(self.app_operator.active_monitor)

            # current node where the service with id (self.DES) is deployed.
            currentNode = sim.alloc_DES[self.DES]

            ################################################################################
            # SERVICEINSTANCE fact
            # serviceInstance(ServiceInstanceId, ServiceId, Node).
            ################################################################################
            self.rules.and_rule("serviceInstance",self.DES,self.app_name,currentNode)

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
            requests = [] #(latency of the path, path)
            # print("\t All paths [wl-node,service-node: ",routing.controlServices)
            for (path,des) in routing.controlServices.values():
                if des==self.DES:
                    requests.append([self.get_latency(path, sim.topology), path])
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


            ## LIST OF DIRECT NODES to the CurrentNode
            neighbours = [e[1] for e in sim.topology.G.edges(currentNode)]
            neighbours = list(dict.fromkeys(neighbours))
            for n in neighbours:
                self.rules.and_rule("node", n, available_space_on_node[n], [])

            self.rules.and_rule("node", currentNode, available_space_on_node[currentNode], neighbours)

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
            df = pd.read_csv(self.path_csv_files + ".csv", skiprows=range(1, self.previous_number_samples))  # It includes the csv header
            self.previous_number_samples += len(df.index) - 1  # avoid header
            df = df[df["DES.dst"]==self.DES]
            if len(df)>0:
                # print("Number of samples: %i (from: %i)" % (len(df.index)-1, self.previous_number_samples))
                if len(requests)>0:
                    # print(df[["TOPO.src","TOPO.dst"]])
                    for (latencyPath,path) in requests:
                        node_user = path[0] # The last node. It is not the ID-user.
                        n_messages = len(df[df["TOPO.src"] == node_user])
                        if n_messages > 0:
                            if len(path)==1:
                                node_code= "self"
                            else:
                                node_code = path[-2]

                            self.rules.and_rule("requests", self.DES, node_code, n_messages,latencyPath)
            else:
                print("INFO - No messages among users and service")
                self.logger.warning("INFO - There are not new messages among users and service")

            ################################################################################
            # Action history from AppOperator (MARIO)
            # lastOutcome((OperationName,Si,Node), Result).
            ################################################################################
            if self.DES in self.app_operator.agent_communication:
                for (prevOperation,status) in self.app_operator.agent_communication[self.DES]:
                    self.rules.and_rule("lastOutcome",prevOperation, status)

            ################################################################################
            # RUN the model
            ################################################################################
            action = self.run_prolog_model(self.rules, self.DES, currentNode, path_results, sim)

            ################################################################################
            # The agent communicates to the appOperator (MARIO) its operations
            ################################################################################
            self.app_operator.get_actions_from_agents((self.name,self.DES,currentNode,action))


    def run_prolog_model(self, facts, service_name, current_node, path_results, sim):
        ###
        ### MARIO v.1.3.
        ###
        ## It prepares the file (.pl) to run it in a terminal command

        # There is a lib.pl to clean the policy rules
        # this file is loaded in this point, but its path is implicit in self.rule_profile
        loadOtherPLModels = ["lib","agentRequests"]
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
        rules_dir = Path(path_results + "models/")
        rules_dir.mkdir(parents=True, exist_ok=True)
        rules_dir = str(rules_dir)
        model_file = rules_dir + "/rules_swi_UID%i_n%s_s%s_%i_%i.pl" % (self.app_operator.UID + 1, current_node, service_name, self.action_on_render,sim.env.now)
        self.action_on_render += 1

        with open(model_file, "w") as f:
            f.write(rules_and_facts)
            f.write("\nmain :- current_prolog_flag(argv, Argv),\n" \
                    "  nth0(0, Argv, Argument0),\n" \
                    "  atom_number(Argument0, ServiceID),\n"\
                    "  operations(ServiceID,RequestedActions),\n"\
                    "  format('~q~n', [RequestedActions]),\n"\
                    "  halt.\n"\
                    "main :-\n"\
                    "  halt(1).\n")


        ### Run the model using swipl command on the terminal
        try:
            cmd = ["swipl",model_file,str(service_name)]
            p = Popen(cmd, stdout=PIPE, stderr=PIPE)
            stdout, stderr = p.communicate(timeout=10)


            expr = stdout.decode("utf-8")
            expr = expr.replace("\n","")
            it = iter("".join(c for c in expr if c not in "()[] ").split(","))
            result = [(x, y, z ) for x, y, z in zip(it, it, it)]
            ## result == [('migrate', '2', 'n0lt0ln0'), ('replicate', '2', 'n0lt0ln0')]

            # print("Actions :",result)

            assert len(result)>=0, "(agent.py) Prolog return is incorrect"
            if len(result)==0:
                result = [("nop",0,"X")]
            return result[0]

        except TimeoutExpired as err:
            p.terminate()
            raise Exception("Error running PYSWIP model on file: %s - TIMEOUT EXPERIED" % model_file)
        # except Exception as err:
        #     print(err)
        #     raise Exception("Error running PYSWIP model on file: %s" % model_file)
