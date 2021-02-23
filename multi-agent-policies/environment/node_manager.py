from collections import defaultdict
from agent import PolicyManager
from problogRulesGenerator import Rules
from yafs.topology import *
from yafs.distribution import *
import logging
import matplotlib.pyplot as plt
import networkx as nx
from PIL import Image
from pathlib import Path
from matplotlib import colors
import matplotlib as mpl
import os
import json
from collections import Counter
import tiledTopology
import matplotlib.patches as mpatches
from collections import defaultdict
import pandas as pd
from subprocess import Popen, PIPE, TimeoutExpired

RENDERSNAP = False
DEBUG_TEXT_ON_RENDER = True

class NodeManager():

    def __init__(self, common_rules, service_rule_profile, path_csv_files, app_number, period, render, path_results, cloud_node, window_agent_size,
                    radius,reversepath ):
        self.create_initial_services = True
        self.logger = logging.getLogger(__name__)
        self.memory = defaultdict(list)
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
        self.path_results = path_results

        # Render
        self.render_action = render
        self.image_id =0
        self.snap_id =0
        self.__draw_controlUser = {}

        self.active_monitor = {}
        # key: id_service (DES), value: id_monitor (DES)

        self.load_samples_from = defaultdict(int)

        #STATS
        self.dump_stats = 0

        #Communication channel with the agents
        self.agent_communication = defaultdict(list) #it keeps a record about the last action taken by the agent and its status: accept or reject
        self.window_agent_comm = defaultdict(int)
        self.window_agent_size = window_agent_size

        self.cloud_node = cloud_node

        self.radius = radius
        self.reversepath = reversepath

    def close(self):
        self.action_stats.close()
        self.actions_moves.close()

    def get_latency(self, path, topology):
        speed = 0
        for i in range(len(path) - 1):
            link = (path[i], path[i + 1])
            speed += topology.G.edges[link][Topology.LINK_PR]
        return speed

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
            self.logger.debug("Node Managers - Global Initialization - Time: %i" % (sim.env.now))
            self.service_calls = defaultdict(list)
            for app in sim.alloc_module:
                for service in sim.alloc_module[app]:
                    for des in sim.alloc_module[app][service]:
                        self.create_monitor_of_module(des, path, routing, service, sim)
            self.create_initial_services = False #only one time

            self.action_stats = open(self.path_results+"action_stats.txt",'w')
            self.action_stats.write("time,undeploy,nop,migrate,replicate,none,shrink,expand,DES,app\n")

            self.actions_moves = open(self.path_results+"moves_stats.txt",'w')
            self.actions_moves.write("idService,app,time,action,level\n")

            # Initial snapshot for debugging issues
            self.snapshot(sim, routing)
        else:
            clean_routing_cache = False

            for nodeManagerId in self.memory:
                counter_actions = Counter()
                #
                # if nodeManagerId < 0 and len(self.memory[nodeManagerId])>0: #Special treatment of nop operations only for recording stats
                #     counter_actions["nop"] += 1 #len(self.memory[nodeManagerId])
                #     line = ""
                #     for nodeManagerId in ["undeploy", "nop", "migrate", "replicate", "none", "shrink", "evict",
                #                           "reject"]:
                #         line += "%i," % counter_actions[nodeManagerId]
                #     self.action_stats.write("%i,%s\n" % (sim.env.now, line[:-1]))
                #     self.action_stats.flush()
                #     # self.memory[nodeManagerId] = list()

                if len(self.memory[nodeManagerId])>0:
                    self.step += 1
                    self.logger.info("NodeManager Node %i - Activation step: %i  - Time: %i" %(nodeManagerId,self.step,sim.env.now))

                    if RENDERSNAP:
                        self.snapshot(sim, routing)
                    self.UID += 1

                    previous_services = set()
                    #  LIFO agent request - reversed list - Only do the first one


                    # print("Action NodeManager: ",nodeManagerId)
                    # print(list(self.memory[nodeManagerId]))

                    for service_name,DES,fromNode,operation in self.memory[nodeManagerId]:
                        (action, serviceId, _, level) = operation
                        #assert DES == serviceId
                        # We only deal with the last action of the agent that is possible
                        if serviceId in previous_services: continue
                        previous_services.add(serviceId)

                        if action == "nop":
                            counter_actions["nop"] += 1  # len(self.memory[nodeManagerId])
                            line = ""
                            for item in ["undeploy", "nop", "migrate", "replicate", "none", "shrink", "evict", "reject", "adapt"]:
                                line += "%i," % counter_actions[item]
                            line +="%i,%s"%(serviceId, self.get_app_identifier(service_name))
                            # self.action_stats.write("%i,%s\n" % (sim.env.now, line[:-1]))
                            self.action_stats.write("%i,%s\n" % (sim.env.now, line))
                            self.action_stats.flush()
                            continue

                        appname = self.get_app_identifier(service_name)
                        # for action,serviceId,onNode,level in operation:
                        print("+ Node Manager: %i + get actions from DES_service: %i"%(nodeManagerId,DES))
                        print("\tService: ", service_name)
                        print("\tApp name: ", appname)
                        print("\tServiceId: ", serviceId)
                        print("\tFrom Node: ", fromNode)
                        print("\tAction: ", action)
                        print("\tFlavour:", level)
                        # print("\tNodeManager:", onNode)

                        serviceId = int(serviceId)


                        #### GENERATING NEW FACTS
                        #TODO create some cache from same facts
                        rules = Rules(self.common_rules)

                        available_space_on_node = self.get_free_space_on_nodes(sim)
                        nodesRadius = nx.single_source_shortest_path_length(sim.topology.G, source=nodeManagerId,cutoff=self.radius)
                        neighbours = [e[1] for e in sim.topology.G.edges(nodeManagerId)]
                        neighbours = list(dict.fromkeys(neighbours))
                        for n in nodesRadius:
                            if n != nodeManagerId:
                                rules.and_rule("node", n, available_space_on_node[n], [])
                        rules.and_rule("node", "self", available_space_on_node[nodeManagerId], neighbours)


                        ####
                        # Service Instance
                        # Que servicios instnacias hay en nodos vecinos y self nodos.

                        usedDES = set()
                        neighbours = set(neighbours)
                        neighbours.add(nodeManagerId)
                        for (path, des) in routing.controlServices.values():
                            if path[-1] in neighbours:
                                desOnThatNode = des
                                if des not in usedDES:
                                    usedDES.add(des)
                                    try:
                                        levelOnNode = sim.alloc_level[desOnThatNode]
                                        module = sim.get_module(desOnThatNode)
                                        appname = self.get_app_identifier(module)
                                        if path[-1] == nodeManagerId:
                                            rules.and_rule("serviceInstance", desOnThatNode, appname, levelOnNode, "self")
                                        else:
                                            rules.and_rule("serviceInstance", desOnThatNode, appname, levelOnNode, path[-1])
                                    except KeyError:
                                        self.logger.warning("At serviceInstance rule fact. A service was moved in previous operation")
                                        continue


                        ################################################################################
                        # REQUEST fact
                        # requests(ServiceInstanceId, Neighbour, RequestRate, LatencyToClient).
                        ################################################################################
                        # requests = []
                        # for (path, des) in routing.controlServices.values():
                        #     if path[-1]==nodeManagerId:
                        #         requests.append([self.get_latency(path, sim.topology), path])

                        sim.metrics.flush()
                        # We only load samples that are generated along current period (self.activations-1,self.activation)
                        df = pd.read_csv(self.path_csv_files + ".csv",
                                         skiprows=range(1, self.load_samples_from[nodeManagerId]))  # It includes the csv header
                        self.load_samples_from[nodeManagerId] += len(df.index) - 1  # avoid header
                        df = df[df["DES.dst"] == serviceId]
                        if len(df) > 0:
                            # WARNING . If the topology changes (ie. node failure) this code should be checked again
                            dg = df.groupby("TOPO.src")["TOPO.src"].count()
                            for node_src, sumMessages in dg.iteritems():
                                path = routing.get_path_from_src_dst(sim, node_src, nodeManagerId)

                                latency = self.get_latency(path, sim.topology)
                                if sumMessages > 0:
                                    if len(path) == 1:
                                        node_code = ["self"]
                                    else:
                                        node_code = path[-2]
                                        if (self.reversepath + 1) > len(path) - 1:
                                            node_code = path[0:-1]
                                        else:
                                            node_code = path[-(self.reversepath + 1):-1]
                                    rules.and_rule("requests", serviceId, node_code, sumMessages, latency)
                            # print("Number of samples: %i (from: %i)" % (len(df.index)-1, self.previous_number_samples))
                            # if len(requests) > 0:
                            #     for (latencyPath, path) in requests:
                            #         node_user = path[0]  # The last node. It is not the ID-user.
                            #         n_messages = len(df[df["TOPO.src"] == node_user])
                            #         if n_messages > 0:
                            #             if len(path) == 1:
                            #                 node_code = ["self"]
                            #             else:
                            #                 if (self.reversepath + 1) > len(path) - 1:
                            #                     node_code = path[0:-1]
                            #                 else:
                            #                     node_code = path[-(self.reversepath + 1):-1]
                            #             rules.and_rule("requests", serviceId, node_code, n_messages, latencyPath)

                        # Current operation from the node
                        rules.and_rule("operation",action,serviceId,"self",level)

                        # Run Prolog Model
                        actions = self.run_prolog_model(rules, serviceId, nodeManagerId, self.path_results, sim,appname)
                        ###

                        # print("Actions debugging by NodeManager %i "%nodeManagerId)
                        # print("\tRaw actions from prolog:")
                        # print(actions)
                        #TODO rethink the sort of the actions.
                        #TODO all these actions are coherent, is it important the order?

                        for (accept,service,operation,levelaction) in actions:
                            # TODO debug and implement
                            print("\tAction: ")
                            print(accept,service,operation,levelaction)

                            if accept == "accept":
                                # print(action)
                                # print(operation)
                                assert action == operation, "Actions are different in NodeManager"
                                assert int(service) == serviceId, "ServiceId (DES) should be equals in NodeManager"
                                done = self.prepare_perform_action(sim, routing, path, service_name, serviceId, operation, fromNode,nodeManagerId,level,"Accept")
                                if done: #it should be done
                                    clean_routing_cache = (action != "reject") or (clean_routing_cache != True)
                                    self.logger.debug("Action %s taken on NodeManager %s." % (action, nodeManagerId))
                                    counter_actions[action] += 1
                                    self.actions_moves.write("%i,%i,%i,%i,%s,%s\n" % (
                                        nodeManagerId, serviceId, self.get_app_identifier(service_name), sim.env.now, action, level))


                        # End-for actions from NodeManager with the serviceID

                    sim.print_debug_assignaments()
                    # End-for. Managed all operations in memory of the NodeManager.
                    self.memory[nodeManagerId] = list()
                    # Writing stats
                    if len(counter_actions) > 0:
                        line = ""
                        for item in ["undeploy", "nop", "migrate", "replicate", "none", "shrink", "evict", "reject", "adapt"]:
                            line += "%i," % counter_actions[item]
                        line += "%i,%i" % (int(serviceId), self.get_app_identifier(service_name))
                        # self.action_stats.write("%i,%s\n" % (sim.env.now, line[:-1]))
                        self.action_stats.write("%i,%s\n" % (sim.env.now, line))
                        self.action_stats.flush()

            #End all node managers
            if clean_routing_cache:
                # if the cache has to be cleared it is because there have been changes.
                routing.clear_routing_cache()
                # Cache routing data is stored to improve the execution time of the simulator
                if RENDERSNAP: #there are changes due to modifictions in the cache
                    self.snapshot(sim, path, routing) # IMPORTANT previous to clean the cache!




    def prepare_perform_action(self, sim, routing, path, service_name, serviceId, action, fromNode, onNode, level,message):
        # Reset the buffer of lastoutcome statements
        if self.window_agent_size == self.window_agent_comm[serviceId]:
            self.window_agent_comm[serviceId] = 0
            self.agent_communication[serviceId] = []

        # The render of the action is done before the action is perfomed and internal variables of the simulator changes
        if self.render_action and action != "nop":
            image_file = self.render(sim, routing,
                                     service=service_name,
                                     serviceID=serviceId,
                                     currentNode=fromNode,
                                     action=action,
                                     onNode=onNode,
                                     level=level,
                                     message = message)


        done = self.perfom_action(sim,
                                  service=service_name,
                                  serviceId=serviceId,
                                  fromNode=fromNode,
                                  action=action,
                                  toNode=onNode,
                                  level=level,
                                  routingAlgorithm=routing,
                                  path=path)


        # if self.render_action and not done:
        #     try:
        #         self.logger.critical("Action %s not done on NodeManager %i " % (action, onNode))
        #         os.remove(image_file)
        #     except FileNotFoundError:
        #         None

        if action == "undeploy" and serviceId in self.agent_communication:
            del self.agent_communication[serviceId]

        self.window_agent_comm[serviceId] += 1
        return done

    def run_prolog_model(self, facts, serviceID, current_node, path_results, sim,app_name):
        ###
        ### MARIO v.2.5. or Node Manager
        ###
        ## It prepares the file (.pl) to run it in a terminal command

        # There is a lib.pl to clean the policy rules
        # this file is loaded in this point, but its path is implicit in self.rule_profile
        loadOtherPLModels = ["nodeManagerRequest"]
        # Include other models
        rule_file = ""
        for other in loadOtherPLModels:
            pathModel = "policies/%s.pl" % other
            with open(pathModel, "r") as f:
                rule_file += f.read()

        #TODO improve the load of specific policies
        with open("policies/n_manager1.pl", "r") as f:
            rule_file += f.read()

        # Include agent facts
        rules_and_facts = ":- initialization(main).\n\n " \
                          "%s\n%s\n" % (rule_file, str(facts))

        # Write rules and facts in a Prolog file *.pl
        rules_dir = Path(path_results + "model_nmanager/")
        rules_dir.mkdir(parents=True, exist_ok=True)
        rules_dir = str(rules_dir)
        model_file = rules_dir + "/model_NM%i_DES%s_%i.pl" % (current_node, serviceID, sim.env.now)

        with open(model_file, "w") as f:
            f.write(rules_and_facts)
            f.write("\nmain :- current_prolog_flag(argv, Argv),\n" \
                    "  nth0(0, Argv, Argument0),\n" \
                    "  atom_number(Argument0, ServiceID),\n" \
                    "  n_operations(RequestedActions),\n" \
                    "  format('~q~n', [RequestedActions]),\n" \
                    "  halt.\n" \
                    "main :-\n" \
                    "  halt(1).\n")

        ### Run the model using swipl command on the terminal
        try:
            cmd = ["swipl", model_file, str(serviceID)]
            p = Popen(cmd, stdout=PIPE, stderr=PIPE)
            stdout, stderr = p.communicate(timeout=10)

            expr = stdout.decode("utf-8")
            expr = expr.replace("\n", "")

            it = iter("".join(c for c in expr if c not in "()[] ").split(","))
            result = [(x, y, z, v) for x, y, z, v in zip(it, it, it, it)]

            assert len(result) >= 0, "(nodeManager.py) Prolog response is incorrect"
            return result

        except TimeoutExpired as err:
            p.terminate()
            raise Exception("Error running PYSWIP model on file: %s - TIMEOUT EXPERIED" % model_file)


    def perfom_action(self, sim,
                      service,
                      serviceId,
                      fromNode,
                      action,
                      toNode,
                      level,
                      routingAlgorithm,
                      path):
        """
        Decode the action and perfom the action in the simulator

        :param sim:
        :param action:
        :return:
        """
        print("*"*10)
        print("-: ACTION :-")
        print("\tService: ", service)
        print("\tDES-ServiceId: ", serviceId)
        print("\tAction: ", action)
        print("\tOn Node:", fromNode)
        print("\tTo Node: ", toNode)
        print("\tFlavour:", level)


        if action == "replicate": #it is prepared to get a list of nodes
            nodes_to_replicate = [toNode]
            space_on_node = self.get_free_space_on_nodes(sim)
            # All these comments made mandatory that all nodes have capacity to host the all instances.
            # feasible = True #
            size = self.get_size_level(sim, service, level)

            nodes_with_space = []
            for n in nodes_to_replicate:
                if space_on_node[n] < size:
                    self.logger.warning("There is not more free space on node: %s"%n)
                    # print("\t WARNING: NO FREE SPACE ON NODE:%s"%n)
                else:
                    nodes_with_space.append(n)

            for n in nodes_with_space:
                self.logger.debug("Action Replicate new instance of %s on node: %s" % (service, n))
                # print("\t\t+Action Replicate new instance of %s on node: %s" % (service, n))
                self.deploy_module(sim, service, n, routingAlgorithm, path,level)

            return len(nodes_with_space)>0

        elif action == "migrate":
            try:
                target_node = toNode
                space_on_nodes = self.get_free_space_on_nodes(sim) # double check, but there're always space due to rules
                size = self.get_size_level(sim, service, level)

                # print("MIGRATE ")
                # print(sim.apps_level)
                # print("space on nodes")
                # print(space_on_nodes)
                # print("NODE target",target_node)
                # print("Size necesario",size)


                if space_on_nodes[target_node] < size: #check the size
                    self.logger.warning("There is not more free space on node: %s" % target_node)
                    print("\t WARNING: NO FREE SPACE ON NODE:%s" % target_node)
                    return False
                else:
                    self.logger.debug("Action Migrate new instance of %s on node: %s" % (service, target_node))
                    # print("\t\t+Action Migrate new instance of %s on node: %s" % (service, target_node))
                    self.deploy_module(sim, service, target_node, routingAlgorithm, path, level)
                    self.logger.debug("\t Remove current instance %s on node: %s" % (service, fromNode))
                    # print("\t\t+Remove current instance of %s on node: %s" % (service, currentNode))
                    self.undeploy_module(sim, service, fromNode, serviceId)
                    return True
            except:
                self.logger.critical("A migration rule with no target node")
                # print("A migration rule with no target node")
                return False


        elif action == "adapt":
            # no arguments
            self.logger.debug("Action ADAPT instance %i to level %s" % (serviceId, level))
            sim.alloc_level[serviceId] = level
            print("\n ADAPT \n")
            print(sim.alloc_level[serviceId])
            return True

        elif action == "undeploy":
            # no arguments
            self.logger.debug("Action UNDEPLOY instance %s on node: %s" % (service, fromNode))
            self.undeploy_module(sim, service, fromNode, serviceId)

            return True

        elif action == "nop":
            # Doing nothing
            self.logger.debug("Action NOP instance %s on node: %s" % (service, fromNode))
            return True

        elif action == "fusion":
            return True


    def undeploy_module(self,sim,service,node,id_service):
        sim.undeploy_module(self.get_app_identifier(service), service, id_service)
        #Remove stop process
        del self.active_monitor[id_service]
        sim.stop_process(id_service)

    def deploy_module(self, sim, service, node, routingAlgorithm, experiment_path,level):
        # Allocation the module in the simulator
        app_name = self.get_app_identifier(service)
        app = sim.apps[app_name]
        services = app.services
        des = sim.deploy_module(app_name, service, services[service], [node],level=level)[0]
        # Creating a new monitor associated to the module
        self.create_monitor_of_module(des, experiment_path, routingAlgorithm, service, sim)

    # News in MARIO 2.5
    # shrink or enlarge
    # HW checks should be done previously
    def modify_level_module(self, sim, id_service,level):
        sim.alloc_level[id_service] = level

    # News in MARIO 2.5
    def get_size_level(self,sim,service,level):
        app_name = self.get_app_identifier(service)
        return sim.apps_level[app_name][level][0]


    def create_monitor_of_module(self, des, path, routing, service, sim):
        period = deterministic_distribution(self.period, name="Deterministic")
        pm = PolicyManager(des, service, self.common_rules, self.service_rule_profile, self.path_csv_files, self, self.render,self.radius,self.reversepath)

        # Performance tip: it avoids an huge load of useless samples
        number_of_lines = 0
        with open(self.path_csv_files+ ".csv", "r") as filehandle:
            number_of_lines = len(filehandle.readlines())
        pm.load_samples_from = number_of_lines
        ####

        id_monitor = sim.deploy_monitor("Policy Manager %i" % des, pm, period,
                                        **{"sim": sim, "routing": routing, "experiment_path": path, "path_results":self.path_results})
        pm.id_monitor = id_monitor
        logging.debug("Generating a new agent control from: %s with id: %i - Monitor: %i" % (service, des, id_monitor))
        # print("Generating a new agent control from: %s with id: %i - Monitor: %i" % (service, des, id_monitor))
        self.active_monitor[des] = id_monitor

    def get_actions_from_agents(self, nodeID, something):
        """
        Agents write in this buffer

        :param something:
        :return:
        """
        self.memory[nodeID].append(something)

    def __draw_user(self, node, service, ax, newcolors,scenario=None):
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

        numberUserRow = 4
        inPlace = self.__draw_controlUser[node]
        line = int(inPlace // numberUserRow)
        duy = -0.25 - (0.15 * line)
        dux = -.2 + (.15* (inPlace % numberUserRow))

        ax.scatter(self.pos[node][0] + dux, self.pos[node][1] + duy, s=400.0, marker='o', color=newcolors[service],edgecolors="black")
        self.__draw_controlUser[node]+=1

    def get_app_identifier(self,nameservice):
        return int(nameservice[0:nameservice.index("_")])

    def get_nodes_with_users(self, routing):
        nodes_with_users = defaultdict(list)
        for (_,node_user,user_service) in routing.controlServices.keys():
            nodes_with_users[node_user].append(self.get_app_identifier(user_service))
        return nodes_with_users

    def get_free_space_on_nodes(self,sim):
        currentOccupation =dict([a, int(x)] for a,x in nx.get_node_attributes(G=sim.topology.G, name="HwReqs").items())
        for app in sim.alloc_module:
            dict_module_node = sim.alloc_module[app]  # modules deployed
            for module in dict_module_node:
                for des in dict_module_node[module]:
                    size = sim.get_size_service(app, sim.alloc_level[des])
                    currentOccupation[sim.alloc_DES[des]] -= size
        return currentOccupation


    def get_nodes_with_services(self,sim):
        """
        It returns a dictionary for node with a np.array with the occupation for visualization purporse

        :param sim:
        :return:
        """
        HwReqs = nx.get_node_attributes(G=sim.topology.G, name="HwReqs")
        currentOccupation = {}
        currentIDoccupation = {}
        #Init. the shape-grid structure to render it with zeros
        for n in sim.topology.G.nodes:
            currentOccupation[n] = np.zeros(int(HwReqs[n])).astype(int)
            currentIDoccupation[n] = np.zeros(int(HwReqs[n])).astype(int)

        for app in sim.alloc_module:
            dict_module_node = sim.alloc_module[app] # modules deployed
            for module in dict_module_node:
                for des in dict_module_node[module]:
                    indx = list(currentOccupation[sim.alloc_DES[des]]).index(0)
                    size = sim.get_size_service(app,sim.alloc_level[des])
                    for i in range(size):
                        currentOccupation[sim.alloc_DES[des]][indx] = app
                        currentIDoccupation[sim.alloc_DES[des]][indx] = des
                        indx+=1

        shape = nx.get_node_attributes(G=sim.topology.G, name="shape")
        for node in currentOccupation:
            try:
                currentOccupation[node] = np.array(currentOccupation[node]).reshape(eval(shape[node]))
                currentIDoccupation[node] = np.array(currentIDoccupation[node]).reshape(eval(shape[node]))

            except ValueError:
                raise "Network node: %s defined with a bad shape "%node
                # print("Network node: %i defined with a bad shape "%node)
                # currentOccupation[node] = np.zeros(shape(1,1))

        return currentOccupation,currentIDoccupation

    def render(self,sim,routing,service,serviceID,currentNode,action,onNode,level,message=""):
        # if "Grid" in path:
        #     scenario="Grid"
        # else:
        #     scenario="Rome"

        # if self.pos == None: # Only the first time
        #     if "pos" in sim.topology.G.nodes[0]: #Cloud Node
        #         self.pos = nx.get_node_attributes(sim.topology.G,'pos')
        #     else:
        #         self.pos = nx.kamada_kawai_layout(sim.topology.G)

        if self.pos == None:  # Only the first time
            pos = nx.get_node_attributes(sim.topology.G, 'pos')
            if len(pos) > 0:
                for k in pos.keys():
                    pos[k] = np.array(eval(pos[k]))
                    self.pos = pos
            else:
                self.pos = nx.kamada_kawai_layout(sim.topology.G)
            image_dir = Path(self.path_results+"images/")
            image_dir.mkdir(parents=True, exist_ok=True)
            self.image_dir = str(image_dir)

        tab20 = plt.cm.get_cmap('tab20', self.total_services+5)
        bounds = range(self.total_services+5)
        newcolors = tab20(np.linspace(0, 1, self.total_services+5))
        newcolors[0] = np.array([250.0 / 256.0, 250. / 256., 250. / 256., 1])
        newcmp = mpl.colors.ListedColormap(newcolors)
        norm = mpl.colors.BoundaryNorm(bounds, newcmp.N)

        fig, ax = plt.subplots(figsize=(16.0, 10.0))
        plt.tight_layout(h_pad=0.55)
        # left, bottom, width, height = ax.get_position().bounds

        nx.draw(sim.topology.G, self.pos, with_labels=False, node_size=1, node_color="#1260A0", edge_color="gray", node_shape="o",
                 font_size=7, font_color="white", ax=ax)

        width = ax.get_xlim()[1]
        top = ax.get_ylim()[1]

        try:
            idApp = int(service.split("_")[0])
        except ValueError: #It triggers when the simulation ends: action(null)
            idApp = 0

        color_app = newcmp(idApp)

        # Get the POLICY FILE
        # As the service is named: "idApp_IdModule", we can get the app id from there.

        # rule_policy = ""
        #
        # try:
        #     for app in dataApps:
        #         if app["id"] == idApp:
        #             rule_policy = app["profile_rules"]
        #             break
        # except UnboundLocalError:
        #     print("- WARNING - Rendering the image of the last case")
        #     None #Render the last case


        # info_text = "App: %i with policy: %s" % (idApp, rule_policy)
        # plt.text(3.5, top -1.5, info_text, {'color': color_app, 'fontsize': 10})

        proportion = width/top
        if DEBUG_TEXT_ON_RENDER:
            plt.text(.5, top-(top/4), "Simulation time: %i" % sim.env.now, {'color': "black", 'fontsize': 14})
            info_text = "Action: %s on node: %s with level: %s" % (action, onNode,level)
            plt.text(.5, top-(top/4)-0.2, info_text, {'color': color_app, 'fontsize': 14})

            info_text = "by Service: S%i in node: %s" % (serviceID, tiledTopology.getAbbrNodeName(currentNode))
            plt.text(.5, top-(top/4)-0.4, info_text, {'color': color_app, 'fontsize': 14})

            info_text = "Service: model_B%i_n%s_DES%i_X_X.pl" % (self.UID, tiledTopology.getAbbrNodeName(currentNode), serviceID)
            plt.text(.5, top-(top/4)-0.6, info_text, {'color': color_app, 'fontsize': 14})

            info_text = "NM: model_NM_%i_DES%s_%i.pl" %(currentNode, serviceID, sim.env.now)
            plt.text(.5,top-(top/4)-0.8, info_text, {'color': "black", 'fontsize': 14})
            plt.text(.5,top-(top/4)-1., "Action: %s"%message, {'color': "black", 'fontsize': 14})

        # if DEBUG_TEXT_ON_RENDER:
        #     plt.text(.5, top-0.4, "Simulation time: %i" % sim.env.now, {'color': "black", 'fontsize': 14})
        #     info_text = "Action: %s on node: %s with level: %s" % (action, onNode,level)
        #     plt.text(.5, top-0.45, info_text, {'color': color_app, 'fontsize': 14})
        #
        #     info_text = "by Service: S%i in node: %s" % (serviceID, tiledTopology.getAbbrNodeName(currentNode))
        #     plt.text(.5, top-0.50, info_text, {'color': color_app, 'fontsize': 14})
        #
        #     info_text = "Service: model_B%i_n%s_DES%i_X_X.pl" % (self.UID, tiledTopology.getAbbrNodeName(currentNode), serviceID)
        #     plt.text(.5, top-0.55, info_text, {'color': color_app, 'fontsize': 14})
        #
        #     info_text = "NM: model_NM_%i_DES%s_%i.pl" %(currentNode, serviceID, sim.env.now)
        #     plt.text(.5, top - 0.60, info_text, {'color': "black", 'fontsize': 14})
        #     plt.text(.5, top - 0.65, "Action: %s"%message, {'color': "black", 'fontsize': 14})



        # Labels on nodes
        for x in sim.topology.G.nodes:
            if x == 0:
                ax.text(self.pos[x][0]- (width/16), self.pos[x][1] , tiledTopology.getAbbrNodeName(x), fontsize=20,fontweight="bold")
            else:
                ax.text(self.pos[x][0] - (width/35), self.pos[x][1] + (width/50) , tiledTopology.getAbbrNodeName(x), fontsize=20,fontweight="bold")


        if not "closers" in self.image_dir:
            legendItems = []
            for i in range(1,self.total_services+1):
                color_app = newcmp(i)
                legendItems.append(mpatches.Patch(facecolor=color_app, edgecolor="black", linewidth=".7" ,label='App: %i'%i))
            plt.legend(loc="upper left",handles=legendItems, ncol=self.total_services//2,fontsize=18)

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
        data_occupation,data_id = self.get_nodes_with_services(sim)



        # Generate node shape
        nodesDESUsed = set()
        for n in sim.topology.G.nodes():
            nodesDESUsed = set()
            xx, yy = trans(self.pos[n])  # figure coordinates
            xa, ya = trans2((xx, yy))  # axes coordinates
            nrows, ncols = data_occupation[n].shape
            if nrows * ncols == 1:
                piesize = .020
            elif nrows * ncols < 4:
                piesize = .050
            elif nrows * ncols < 6:
                piesize = .08
            elif nrows * ncols < 8:
                piesize = .1
            else:
                piesize = .12

            p2 = piesize / 2.5
            a = plt.axes([xa - p2, ya - p2, piesize, piesize])
            # a.set_aspect('equal')

            #For labelling cells in the imshow
            real_x = np.array(range(0,ncols))
            real_y = np.array(range(0,nrows))
            if len(real_x) > 1:
                dx = (real_x[1] - real_x[0]) / 2.
            else:
                dx = 0.5
            if len(real_y)>1:
                dy = (real_y[1] - real_y[0]) / 2.
            else:
                dy = 0.5
            extent = [real_x[0] - dx, real_x[-1] + dx, real_y[0] - dy, real_y[-1] + dy]

            # Include the current instance service identificator close to the node
            a.imshow(data_occupation[n], cmap=newcmp, interpolation='none', norm=norm,extent=extent)

            sizerow = 0.5
            if nrows==0:
                sizerow = (extent[3]+abs(extent[2]))/float(nrows)

            sizecol = 0.5
            if ncols-1>0:
                sizecol = (extent[1]+abs(extent[0]))/float(ncols)


            for irow,row in enumerate(data_occupation[n],start=1):
                irev_row = (nrows+1)-irow
                py = extent[2]+(sizerow*irev_row)
                if irev_row>=2: py += 0.5
                for icol,value in enumerate(row,start=1):
                    if value==0: break
                    des = data_id[n][irow-1][icol-1]
                    if des in nodesDESUsed: continue #avoid the duplication of text-label in node with services-levels DES
                    nrows, ncols = data_occupation[n].shape
                    if nrows * ncols == 1:
                        px = extent[0] + 0.5
                    else:
                        px = extent[0] + (sizecol * icol) - 0.5
                    a.text(px,py, value, ha="center", va="center", color="w",weight="bold", size=18)
                    nodesDESUsed.add(des)

            a.axes.get_yaxis().set_visible(False)
            a.axes.get_xaxis().set_visible(False)

        canvas = plt.get_current_fig_manager().canvas
        canvas.draw()
        pil_image = Image.frombytes('RGB', canvas.get_width_height(), canvas.tostring_rgb())

        # pil_image.save(self.image_dir + "/network_%05d.pdf" % self.image_id)
        # pil_image.save(self.image_dir + "/network__UID%i_n%i_s%i_X_%i.png" % (self.UID, currentNode, serviceID, sim.env.now))
        pil_image.save(self.image_dir + "/network_%05d.png" % self.image_id)
        # print("Saving render image at: %s"%(self.image_dir + "/network_%05d.png" % self.image_id))

        self.image_id += 1
        plt.close(fig)
        return self.image_dir + "/network_%05d.png" % self.image_id

    def snapshot(self,sim,routing):
        """
        An alternative to render for having a global view of the situation in each

        :param sim:
        :param path:
        :param routing:

        """
        if self.pos == None:  # Only the first time
            pos = nx.get_node_attributes(sim.topology.G, 'pos')
            if len(pos) > 0:
                for k in pos.keys():
                    pos[k] = np.array(eval(pos[k]))
                    self.pos = pos
            else:
                self.pos = nx.kamada_kawai_layout(sim.topology.G)
            image_dir = Path(self.path_results + "images/")
            image_dir.mkdir(parents=True, exist_ok=True)
            self.image_dir = str(image_dir)

        tab20 = plt.cm.get_cmap('tab20', self.total_services+5)
        bounds = range(self.total_services+5)
        newcolors = tab20(np.linspace(0, 1, self.total_services+5))
        newcolors[0] = np.array([250.0 / 256.0, 250. / 256., 250. / 256., 1])
        newcmp = mpl.colors.ListedColormap(newcolors)
        norm = mpl.colors.BoundaryNorm(bounds, newcmp.N)

        fig, ax = plt.subplots(figsize=(16.0, 10.0))
        plt.tight_layout(h_pad=0.55)


        nx.draw(sim.topology.G, self.pos, with_labels=False, node_size=1, node_color="#1260A0", edge_color="gray", node_shape="o",
                 font_size=7, font_color="white", ax=ax)

        width = ax.get_xlim()[1]
        top = ax.get_ylim()[1]
        # Some viz. vars.
        piesize = .078
        # piesize = .08
        p2 = piesize / 2.5


        # Labels on nodes
        for x in sim.topology.G.nodes:
            if x == 0:
                ax.text(self.pos[x][0]- (width/16), self.pos[x][1] , tiledTopology.getAbbrNodeName(x), fontsize=20,fontweight="bold")
            else:
                ax.text(self.pos[x][0] - (width/35), self.pos[x][1] + (width/50) , tiledTopology.getAbbrNodeName(x), fontsize=20,fontweight="bold")

        if not "closers" in self.image_dir:
            legendItems = []
            for i in range(1,self.total_services+1):
                color_app = newcmp(i)
                legendItems.append(mpatches.Patch(facecolor=color_app, edgecolor="black", linewidth=".7" ,label='App: %i'%i))
            # plt.legend(loc="lower center",handles=legendItems, ncol=len(dataApps),fontsize=14)
            plt.legend(loc="upper left",handles=legendItems, ncol=self.total_services//2,fontsize=18)


        # Plotting users dots
        self.__draw_controlUser = {}
        nodes_with_users = self.get_nodes_with_users(routing)


        #PRINT ALL USERS
        for node in nodes_with_users:
            for app in nodes_with_users[node]:
                self.__draw_user(node, int(app), ax, newcolors)

        # LAST step:
        # Displaying capacity, changing node shape
        trans = ax.transData.transform
        trans2 = fig.transFigure.inverted().transform
        data_occupation,data_id = self.get_nodes_with_services(sim)

        # Generate node shape
        for n in sim.topology.G.nodes():
            nodesDESUsed = set()
            xx, yy = trans(self.pos[n])  # figure coordinates
            xa, ya = trans2((xx, yy))  # axes coordinates
            nrows, ncols = data_occupation[n].shape
            if nrows * ncols == 1:
                piesize = .020
            elif nrows * ncols < 4:
                piesize = .050
            elif nrows * ncols < 6:
                piesize = .08
            elif nrows * ncols < 8:
                piesize = .1
            else:
                piesize = .12

            p2 = piesize / 2.5

            a = plt.axes([xa - p2, ya - p2, piesize, piesize])

            # For labelling cells in the imshow
            real_x = np.array(range(0, ncols))
            real_y = np.array(range(0, nrows))
            if len(real_x) > 1:
                dx = (real_x[1] - real_x[0]) / 2.
            else:
                dx = 0.5
            if len(real_y) > 1:
                dy = (real_y[1] - real_y[0]) / 2.
            else:
                dy = 0.5
            extent = [real_x[0] - dx, real_x[-1] + dx, real_y[0] - dy, real_y[-1] + dy]

            # Include the current instance service identificator close to the node

            # Include the current instance service identificator close to the node
            a.imshow(data_occupation[n], cmap=newcmp, interpolation='none', norm=norm,extent=extent)

            sizerow = 0.5
            if nrows==0:
                sizerow = (extent[3]+abs(extent[2]))/float(nrows)

            sizecol = 0.5
            if ncols-1>0:
                sizecol = (extent[1]+abs(extent[0]))/float(ncols)


            for irow,row in enumerate(data_occupation[n],start=1):
                irev_row = (nrows+1)-irow
                py = extent[2]+(sizerow*irev_row)
                if irev_row>=2: py += 0.5
                for icol,value in enumerate(row,start=1):
                    if value==0: break
                    des = data_id[n][irow-1][icol-1]
                    if des in nodesDESUsed: continue
                    px = extent[0]+(sizecol*icol)-0.5
                    a.text(px,py, value, ha="center", va="center", color="w",weight="bold", size=18)
                    nodesDESUsed.add(des)

            a.axes.get_yaxis().set_visible(False)
            a.axes.get_xaxis().set_visible(False)

        canvas = plt.get_current_fig_manager().canvas
        canvas.draw()
        pil_image = Image.frombytes('RGB', canvas.get_width_height(), canvas.tostring_rgb())

        pil_image.save(self.image_dir + "/snap_%05d.png" % self.snap_id)
        self.snap_id += 1
        plt.close(fig)
