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

    """
    When a new instance of a service is deployed in the infrastructure, a DES process will be generated within the simulator that manages the facts of the logical model.
    This DES process is an instance of agent. 
    The _call_ function is called when requested by the app_operator.
    """
    def __call__(self, sim, routing, experiment_path):
        
        if self.id_monitor in self.app_operator.active_monitor.values():# Once a process is finished, the service may run for the last time. The simulator does not control this last call.
            self.rules.clear()
            print("\nMonitor ID: %i for service: %i (%s)  running rules "%(self.id_monitor,self.DES,self.name))
            self.logger.info("\nMonitor ID: %i for service: %i (%s)  running rules "%(self.id_monitor,self.DES,self.name))

            # print("PREVIOUS SERVICES")
            # print(self.app_operator.active_monitor)

            currentNode = sim.alloc_DES[self.DES]
            self.rules.and_rule("serviceInstance",self.DES,self.app_name,currentNode)

             # print("\t All paths [wl-node,service-node: ",routing.controlServices)
            routes = []
            neighbours = [currentNode]
            for (path,des) in routing.controlServices.values():
                if des==self.DES:
                    routes.append([self.get_latency(path, sim.topology), path])
                    # neighbours += path # Uncomment in case of considering nodes from user-paths

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
                    # print(routes)
                    # print(df[["TOPO.src","TOPO.dst"]])
                    for r in routes:
                        assert r[1][-1] == currentNode, "Last path node and source target are different"
                        n_user = r[1][0]
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
                            self.rules.and_rule("route", self.DES, path, r[0],n_messages)
            else:
                print("INFO - No messages among users and service")
                self.logger.warning("INFO - There are not new messages among users and service")

            self.logger.info("Performing problog model")

            # if PROBLOG:
            #     actions = self.run_problog_model(self.rules,self.DES,currentNode,experiment_path)
            # else:
            actions = self.run_swi_model(self.rules,self.DES,currentNode,experiment_path,sim)

            #Sending the rules to the app_operator, aka MARIO
            # print("Sending new rules to MARIO: %s",actions)
            self.app_operator.get_actions_from_agents((self.name,self.DES,currentNode,actions))

    def run_swi_model(self, rules, service_name, current_node, experiment_path,sim):
        # Load policy rules

        policy_rules = ""
        with open(self.rule_profile, "r") as f:
            policy_rules = f.read()

        modelrules = """
           :- discontiguous route/4.
           route(xxxxxx, path(xxxx, xxx, []), 10, 10).
           desiredUser(%s,2).
        %s

        """ % (service_name,policy_rules) + "\n" + str(rules)

        #Writing the model.pl
        rules_dir = Path(experiment_path + "results/models/")
        rules_dir.mkdir(parents=True, exist_ok=True)
        rules_dir = str(rules_dir)
        path_file = rules_dir + "/rules_swi_UID%i_n%i_s%s_%i_%i.pl" % (self.app_operator.UID + 1, current_node, service_name, self.action_on_render,sim.env.now)

        # print("Path_file")
        # print(path_file)
        with open(path_file, "w") as f:
            f.write(modelrules)

        self.action_on_render += 1

        try:
            from pyswip import Prolog
            # swipl - -dump - runtime - variables
            prolog = Prolog()
            prolog.consult(path_file)

            # nop = list(prolog.query("nop(%i)"%service_name))
            # suicide = list(prolog.query("suicide(%i)"%service_name))
            # replicate = list(prolog.query("replicate(%i,M)"%service_name))
            # migrate = list(prolog.query("migrate(%i,M)"%service_name))


            #Return the actions in priority order
            priority = list(prolog.query("priority(M)")) # Priority query does not work

            order_actions = [None] * len(priority[0]["M"])
            for e,act in enumerate(priority[0]["M"]):
                print(str(act))
                if act == b"suicide":
                    suicide = list(prolog.query("suicide(%i)"%service_name))
                    print("SUICIDE")
                    print(suicide)
                    if len(suicide) > 0:
                        order_actions[e] = 'suicide(%i)' % service_name

                if act == b"nop":
                    nop = list(prolog.query("nop(%i)" % service_name))
                    print("Nop")
                    print(nop)
                    if len(nop) > 0:
                        order_actions[e] = 'nop(%i)' % service_name

                if act == b"migrate":
                    migrate = list(prolog.query("migrate(%i,M)" % service_name))
                    # migrate predicate
                    # migrate(s42,X2,1) ...
                    ## NOTE: current swi-migrate rule return an array like replicate statement.
                    ##       in this case, we only get the first one value of that array
                    print("MIGRATE")
                    print(migrate)
                    # RETURNS a NUMBER
                    # tonodes = []
                    # for rep in migrate:
                    #     tonodes.append(rep["M"])
                    #     break
                    # if len(tonodes) > 0:
                    #     order_actions[e] = 'migrate(%i,X,%s)' % (service_name, tonodes[0])
                    # else:
                    #     order_actions[e] = None

                    #RETURN A LIST
                    if len(migrate) > 0:
                        tonodes = migrate[0]["M"]
                        order_actions[e] = 'migrate(%i,X,%s)' % (service_name, tonodes[0])
                    else:
                        order_actions[e] = None

                if act == b"replicate":
                    replicate = list(prolog.query("replicate(%i,M)" % service_name))
                    tonodes = set()
                    print("REPLICATE")
                    print(replicate)
                    if len(replicate)>0:
                        tonodes = replicate[0]["M"]
                        order_actions[e] = 'replicate(%i,%s)' % (service_name, list(tonodes))
                    else:
                        order_actions[e] = None

            #
            # # DEBUG INFO
            # print("Actions taken by service: %i"%service_name)
            # print("NOP %s",nop)
            # print("suicide %s",suicide)
            # print("replicate %s",replicate)
            # print("migrate %s",migrate)
            # print("priority %s",priority) # Priority query doesnot work
            #
            # print("HERE %s",priority[0]["M"])
            #
            #
            # #TODO exportar this function into json data.
            # order_actions = self.order_prot3(priority[0]["M"],migrate, nop, replicate, service_name, suicide)
            # # order_actions = self.order_prot4(migrate, nop, replicate, service_name, suicide)

            return order_actions

        except:
            raise "Error running PYSWIP model: %s" % path_file

    # def order_prot3(self, priority, migrate, nop, replicate, service_name, suicide):
    #     numberRules = 5  # TODO Fix this var as global
    #     order_actions = [None] * numberRules
    #     # the Order is defined by a sequential dealing of the results
    #     if len(suicide) == 0:  # nop(sX) is false => None %EXPECTED: boolean
    #         order_actions[0] = None
    #     else:
    #         order_actions[0] = 'suicide(%i)' % service_name
    #     if len(nop) == 0:  # nop(sX) is false => None %EXPECTED: boolean
    #         order_actions[1] = None
    #     else:
    #         order_actions[1] = 'nop(%i)' % service_name
    #     # replicate predicate
    #     # replicate(1,[3, 2])
    #     tonodes = set()
    #     for rep in replicate:
    #         tonodes.add(rep["M"])
    #     if len(tonodes) > 0:
    #         order_actions[2] = 'replicate(%i,%s)' % (service_name, list(tonodes))
    #     else:
    #         order_actions[2] = None
    #     # migrate predicate
    #     # migrate(s42,X2,1) ...
    #     ## NOTE: current swi-migrate rule return an array like replicate statement.
    #     ##       in this case, we only get the first one value of that array
    #     tonodes = []
    #     for rep in migrate:
    #         tonodes.append(rep["M"])
    #         break
    #     if len(tonodes) > 0:
    #         order_actions[3] = 'migrate(%i,X,%s)' % (service_name, tonodes[0])
    #     else:
    #         order_actions[3] = None
    #     order_actions[4] = None
    #     return order_actions
    #
    # def order_prot4(self, migrate, nop, replicate, service_name, suicide):
    #     #priority([migrate,nop,replicate,suicide]).
    #     numberRules = 5  # TODO Fix this var as global
    #     order_actions = [None] * numberRules
    #
    #     # migrate predicate
    #     # migrate(s42,X2,1) ...
    #     ## NOTE: current swi-migrate rule return an array like replicate statement.
    #     ##       in this case, we only get the first one value of that array
    #     tonodes = []
    #     for rep in migrate:
    #         tonodes.append(rep["M"])
    #         break
    #     if len(tonodes) > 0:
    #         order_actions[0] = 'migrate(%i,X,%s)' % (service_name, tonodes[0])
    #     else:
    #         order_actions[0] = None
    #
    #     if len(nop) == 0:  # nop(sX) is false => None %EXPECTED: boolean
    #         order_actions[1] = None
    #     else:
    #         order_actions[1] = 'nop(%i)' % service_name
    #
    #     # replicate predicate
    #     # replicate(1,[3, 2])
    #     tonodes2 = set()
    #     print("Replicate ")
    #     print(replicate)
    #     for rep in replicate:
    #         print ("REP",rep)
    #         if type(rep["M"]) == list:
    #             for r in rep["M"]:
    #                 tonodes2.add(r)
    #         else:
    #             tonodes2.add(rep["M"])
    #
    #     if len(tonodes2) > 0:
    #         order_actions[2] = 'replicate(%i,%s)' % (service_name, list(tonodes2))
    #     else:
    #         order_actions[2] = None
    #
    #     # the Order is defined by a sequential dealing of the results
    #     if len(suicide) == 0:  # nop(sX) is false => None %EXPECTED: boolean
    #         order_actions[3] = None
    #     else:
    #         order_actions[3] = 'suicide(%i)' % service_name
    #
    #     order_actions[4] = None
    #
    #     return order_actions

    # def run_problog_model(self, rules, service_name,current_node,experiment_path):
    #     """
    #     executes the model into the prolog engine.
    #
    #     :param rules:
    #     :param service_name:
    #     :param current_node:
    #     :param experiment_path:
    #     :return: a list order by agent preference with the highest probabilities
    #     """
    #     all_rules = ""
    #     with open(self.rule_profile, "r") as f:
    #         all_rules = f.read()
    #
    #     queries = "\nquery(nop(%s)).\n"%service_name
    #     queries += "query(migrate(%s, X, %s)).\n"%(service_name,current_node)
    #     queries += "query(replicate(%s, X)).\n"%service_name
    #     queries += "query(suicide(%s)).\n"%service_name
    #     queries += "query(fusion(X, Y)).\n"
    #     queries += "query(priority(X)).\n"
    #
    #
    #     modeltext = """
    #     :- use_module(library(lists)).
    #        route(xxxxxx, path(xxxx, xxx, []), 10, 10).
    #     %s
    #
    #     """%(all_rules+"\n"+str(rules)+queries)
    #
    #     # print(modeltext)
    #     try:
    #         model = PrologString(modeltext)
    #         result = get_evaluatable().create_from(model).evaluate()
    #
    #         # print("RE ",result)
    #         best_actions = self.__sort_results_rules(result)
    #         # print("BA ",best_actions[0])
    #         if self.render_action:
    #             self.render(service_name, current_node, modeltext, experiment_path)
    #
    #         return best_actions
    #     except:
    #         raise Exception(" A problem running problog ")
    #
    # def __sort_results_rules(self,result):
    #     """
    #     test file: test_proc_rules.py
    #
    #     :param result:
    #     :return:
    #     """
    #     pattern_priorities = r"priority\(\[(.*?)\]"
    #     pattern_probabilites = r"(:\ \d.\d,*)"
    #     numberRules = 5
    #
    #     matches = list(re.finditer(pattern_priorities, str(result)))
    #     assert len(matches) > 0, "Agent rules without rule priority"
    #     action_priorities = matches[0].group(1).replace(" ", "").split(",")
    #
    #     matches = re.finditer(pattern_probabilites, str(result))
    #     index = 1
    #     best_probability = -1.0
    #     order_actions = [None] * numberRules
    #     for matchNum, match in enumerate(matches):
    #         for groupNum in range(0, len(match.groups())):
    #             groupNum = groupNum + 1
    #             start, end = match.start(groupNum), match.end(groupNum)
    #             probability = float(match.group(groupNum).replace(": ", "").replace(",", ""))
    #             action = str(result)[index:start]
    #             name_action: str = action[0:action.index("(")]
    #             if name_action != "priority": # ignore priority rule
    #                 if probability > best_probability:
    #                     order_actions = [None] * numberRules
    #                     idx = [i for i, name in enumerate(action_priorities) if name == name_action]
    #                     order_actions[idx[0]] = action
    #                     best_probability = probability
    #                 elif probability == best_probability:
    #                     idx = [i for i, name in enumerate(action_priorities) if name == name_action]
    #                     order_actions[idx[0]] = action
    #
    #             # print(name_action)
    #             index = end + 1
    #     return order_actions

    def render(self,service_name,current_node,modeltext,experiment_path):
        """
        write the model into a file

        :param service_name:
        :param current_node:
        :param modeltext:
        :param experiment_path:
        :return:
        """
        rules_dir = Path(experiment_path + "results/models/")
        rules_dir.mkdir(parents=True, exist_ok=True)
        rules_dir = str(rules_dir)

        with open(rules_dir+"/rules_UID%i_n%i_s%s_%i.pl"%(self.app_operator.UID+1,current_node,service_name,self.action_on_render),"w") as f:
            f.write(modeltext)

        self.action_on_render +=1
