"""
    @author: Antonio Brogi & Stefano Forti & Carlos Guerrero & Isaac Lera
    # MARIO 3
    # send to Journal of Software: Evolution and Process
"""
import os
import time
import json
import logging.config
import networkx as nx

from configparser import ConfigParser

from yafs.core import Sim
from yafs.application import Application, Message
from yafs.topology import Topology
from yafs.distribution import *
from yafs.utils import fractional_selectivity
from yafs.placement import JSONPlacement


from environment.workload import DynamicWorkload
from environment.path_routing import DeviceSpeedAwareRouting
from environment.node_manager import NodeManager
from environment.problogRulesGenerator import Rules
from environment.userMovement import UserControlMovement

def create_applications_from_json(data):
    applications = {}
    for app in data:
        a = Application(name=app["name"])
        modules = [{"None": {"Type": Application.TYPE_SOURCE}}]
        for module in app["module"]:
            modules.append({module["name"]: {"level": app["level"],"Type": Application.TYPE_MODULE}})
        a.set_modules(modules)

        ms = {}
        for message in app["message"]:
            # print "Creando mensaje: %s" %message["name"]
            ms[message["name"]] = Message(message["name"], message["s"], message["d"],
                                          instructions=message["instructions"], bytes=message["bytes"])
            if message["s"] == "None":
                a.add_source_messages(ms[message["name"]])

        # print "Total mensajes creados %i" %len(ms.keys())
        for idx, message in enumerate(app["transmission"]):
            if "message_out" in message.keys():
                a.add_service_module(message["module"], ms[message["message_in"]], ms[message["message_out"]],
                                     fractional_selectivity, threshold=1.0)
            else:
                a.add_service_module(message["module"], ms[message["message_in"]])

        applications[app["name"]] = a

    return applications


def main(number_simulation_steps,
         time_in_each_step,
         experiment_path,
         conf_folder,
         temporal_folder,
         case,
         config,
         doExecutionVideo,
         it,
        id_cloud_node,
         radius,
         reversepath,
         policy_file=None,  # if None we use the policy defined in the APP
         nm_policy= None
         ):
    simulation_duration = number_simulation_steps * time_in_each_step

    """
    TOPOLOGY
    """
    t = Topology()

    print(experiment_path + conf_folder)
    dataNetwork = json.load(open(experiment_path + conf_folder+'topology.json'))
    t.load_all_node_attr(dataNetwork)
    nx.write_gexf(t.G,temporal_folder+"topology.gexf")

    cloudNode = id_cloud_node

    # NOTE: AP NODES OR EDGE NODES DO HAVE ONLY ONE DEGREE/one vertice
    # edgeNodes = [id for (id,degree) in t.G.degree() if degree == 1] #Second test
    # edgeNodes = edgeNodes[1:] # to avoid the cloud in this topology # Original test

    if config.get('simulation', 'typeScenario')=="L":
        edgeNodes = np.concatenate((np.arange(6, 14),np.arange(101, 106), np.arange(124, 136), np.arange(124, 136), np.arange(158, 170),
                                   np.arange(180, 186),np.arange(191, 193),np.arange(406, 414),np.arange(201, 206),np.arange(224, 236),
                                   np.arange(224, 236), np.arange(258, 270), np.arange(280, 286), np.arange(291, 293)))
    elif config.get('simulation', 'typeScenario')=="M":
        edgeNodes = np.concatenate((np.arange(6, 14),np.arange(101, 106), np.arange(124, 136), np.arange(124, 136), np.arange(158, 170),
                                   np.arange(180, 186),np.arange(191, 193)))

    else: #small scenario
        edgeNodes = np.array([6,7,8,9,10,11,12,13])

    # print(edgeNodes)
    #
    # import sys
    # sys.exit()

    """
    Global Rules for all services
    """
    globalrules = Rules()

    """
    APPLICATION
    """
    dataApp = json.load(open(experiment_path + conf_folder+ 'appDefinition.json'))
    apps = create_applications_from_json(dataApp)

    apps_level = {}
    for app in dataApp:
        level = app["level"]
        apps_level[app["name"]] = level
        str_level = "["
        for k in level:
            str_level+="(%s,%i,%i),"%(k,level[k][0],level[k][1])
        str_level = str_level[:-1]+"]"
        globalrules.and_rule("service", "app%i"%app["name"],str_level, app["MaxLatency"])


    service_rule_profile = {}
    for app in dataApp:
        if policy_file == None:
            service_rule_profile[app["name"]] = "policies/" + app[
                "profile_rules"]  # Global path to pl.file
        else:
            service_rule_profile[app["name"]] = "policies/" + policy_file

    """
    PLACEMENT algorithm
    """
    placementJson = json.load(open(experiment_path + conf_folder+'initialAllocation.json'))
    placement = JSONPlacement(name="Placement", json=placementJson)

    """
    Routing algorithm
    """
    routingPath = DeviceSpeedAwareRouting()

    """
    SIMULATION ENGINE
    """
    path_csv_files = temporal_folder + "/Results_%s_%i" % (case, it)
    s = Sim(t, default_results_path=path_csv_files)
    s.set_apps_levels(apps_level)

    """
    INITIAL DEPLOY OF SERVICES
    """
    for aName in apps.keys():
        print("Deploying app:",aName)
        s.deploy_app(apps[aName], placement, routingPath)

    """
    POPULATION algorithm
    """
    dataPopulation = json.load(open(experiment_path + conf_folder + 'usersDefinition.json'))
    # Each application has an unique population politic
    # For the original json, we filter and create a sub-list for each app politic
    for aName in apps.keys():
        data = []
        for element in dataPopulation["sources"]:
            # print("element-app", type(element["app"]))
            if element['app'] == aName:
                data.append(element)

        # distribution = exponential_distribution(name="Exp", lambd=random.randint(100,200), seed= int(aName)*100+it)
        distribution = deterministic_distribution(name="DET", time=10)
        pop_app = DynamicWorkload(name="Dynamic_%s" % aName, data=data, iteration=it, activation_dist=distribution)
        s.deploy_pop(apps[aName], pop_app)

    """
    MARIO app controller & Agent generator
    """
    time_activation = deterministic_distribution(time=int(config.get('nodeManager', 'activation_period')),
                                                 name="Deterministic")

    static_behaviour = False
    try:
        if bool(config.get('simulation',"notAllowedAllocation")):
            static_behaviour = True
    except:
        None


    nM = NodeManager(globalrules, service_rule_profile, path_csv_files,
                  app_number=len(dataApp),
                  period=int(config.get('service', 'activation_period')),
                  render=False,  # only snaps
                  path_results=temporal_folder,
                  cloud_node=cloudNode,
                  window_agent_size=int(config.get('service', 'window_agent_outcome')),
                  radius = radius,
                  reversepath = reversepath,
                  nm_policy = nm_policy,
                  static_behaviour= static_behaviour,
                     iteration = it
                  )

    s.deploy_monitor("App-Operator", nM, time_activation,
                     **{"sim": s,
                        "routing": routingPath,
                        "path": experiment_path+"configuration/",
                        }
                     )


    """
    Create the custom monitor that it manages the user movements according with random distributions
    The user operations are: CREATION, MOVEMENT (change associated node)
    """

    dStart = deterministicDistributionStartPoint(0, time_in_each_step, name="Deterministic")

    record_movements = open(temporal_folder+"/movements_%i.csv"%it,"w")
    record_movements.write("app,DES,time,nodeSRC,nodeDST\n")
    evol = UserControlMovement(
                        experiment_path = experiment_path,
                        appOp = nM,
                        record_movements = record_movements,
                        limit_steps = int(config.get('simulation', 'UsersSteps')),
                        limit_movements = int(config.get('simulation', 'limitMovements')),
                        edgeNodes = edgeNodes,
                        type_scenario = config.get('simulation', 'typeScenario'),
    )



    s.deploy_monitor("Traces_localization_update", evol, dStart,
                     **{"sim": s,
                        "routingAlgorithm": routingPath,
                        "case": case,
                        "stop_time": simulation_duration,
                        "it": it})

    s.set_movement_control(evol)




    """
    RUNNING
    """
    logging.info(" Performing simulation: %s %i " % (case, it))
    s.run(simulation_duration)  # To test deployments put test_initial_deploy a TRUE

    """
    Storing results from other monitors
    & Render the last movement
    """
    # nM.DEBUG_TEXT_ON_RENDER = False
    # nM.render(s,routingPath,"-1",0,0,"Nop",0,"")

    # evol.write_map_user_des(temporal_folder + "/MapUserDES_%s_%i.csv" % (case, it))

    # record_movements.close()
    nM.close()
    s.print_debug_assignaments()



if __name__ == '__main__':

    import logging.config
    logging.config.fileConfig(os.getcwd() + '/logging.ini')
    # import os
    # print(os.getcwd())

    #TODO get the last updated file from json file

    # fileName = "experiment_SSeed.json"
    # fileName = "experiment_LSeed.json"
    # fileName = "experiment_MSeedTEST.json"
    # fileName = "experiment_SSeed_1ms.json"
    # fileName = "experiment_LSeed_v2.json"
    fileName = "experiment_ML.json"
    with open(fileName) as f:
        experiments = json.load(f)

    for item in experiments:

        print(item)

        code = item["code"]
        name = item["scenario"]
        policy = item["policy"]
        nm_policy = item["n_policy"]
        radius = item["radius"]
        reversepath = item["reversepath"]

        experiment_path = "scenarios/%s/"%name

        # datestamp = time.strftime('%Y%m%d_%H%M')
        datestamp = "X"
        temporal_folder = "results/results_%s_%s" % (code, datestamp) + "/"

        config = ConfigParser()
        config.read(experiment_path + 'config.ini')

        number_simulation_steps = int(config.get('simulation', 'UsersSteps')) + 1
        time_in_each_step = int(config.get('simulation', 'time_in_each_step'))
        nSimulations = int(config.get('simulation', 'nSimulations'))

        id_cloud_node = int(config.get('simulation', 'idCloud'))

        try:
            os.makedirs(temporal_folder)
        except OSError:
            None

        print("Generating RESULTS at: %s " % temporal_folder)

        for iteration in range(nSimulations):
            # iteration = 1 #TODO test bugs
            random.seed(iteration)
            np.random.seed(iteration)
            print("Perform simulation...")
            start_time = time.time()
            main(number_simulation_steps=number_simulation_steps,
                 time_in_each_step=time_in_each_step,
                 experiment_path=experiment_path,
                 conf_folder="configuration/",
                 temporal_folder=temporal_folder,
                 case=name,
                 config=config,
                 doExecutionVideo=True,  # expensive task
                 it=iteration,
                 id_cloud_node = id_cloud_node,
                 radius=radius,
                 reversepath=reversepath,
                 policy_file=policy,
                 nm_policy = nm_policy
                 )

            print("\n--- %s seconds ---" % (time.time() - start_time))

            # try:
            #     os.system("ffmpeg -r 1 -i %simages/network_%%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p %svideo_%s.mp4"%(temporal_folder,temporal_folder,code))
            # except:
            #     print("Problems generating video")


    #end for experiments

    # try:
    #     import plot_actions
    #     plot_actions.run(fileName)
    # except:
    #     print("Problems generating actions runs")
    #
    # try:
    #     import plot_averageActionType
    #     plot_averageActionType.run(fileName)
    # except:
    #     print("Problems generating plot_averageActionType")

    try:
        import plot_multi_response_time
        plot_multi_response_time.run(fileName)
    except:
        print("Problems generating response ")

    try:
        import plot_multi_totalrequests
        plot_multi_totalrequests.run(fileName)
    except:
        print("Problems generating totalrequest")


    print("Simulation Done!")


# ffmpeg -r 1 -i results/images/network_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p results/out2.mp4
# ffmpeg -r 1 -i results/images/network_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p results/out2.mp4
# ffmpeg -r 1 -i multi-agent-policies/scenarios/TaxiRome/results_20201028/images/network_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p video.mp4
# ffmpeg -r 1 -i multi-agent-policies/scenarios/TaxiRome/results_P1_20201028/images/snap_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p P1_size6.mp4

# ffmpeg -r 1 -i network_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p video.mp4


# ffmpeg -framerate 10 -i multi-agent-policies/scenarios/TaxiRome/results_P1_20201028/images/snap_%05d.png -c:v libx264 -pix_fmt yuv420p -crf 23 P1_size6.mp4
# ffmpeg -framerate 10 -i multi-agent-policies/scenarios/TaxiRome/results_P1_20201120/images/snap_%05d.png -c:v libx264 -pix_fmt yuv420p -crf 23 P1_size3.mp4
# ffmpeg -framerate 10 -i multi-agent-policies/scenarios/TaxiRome/results_P12_20201028/images/snap_%05d.png -c:v libx264 -pix_fmt yuv420p -crf 23 P12_size6.mp4
# ffmpeg -framerate 10 -i multi-agent-policies/scenarios/TaxiRome/results_P12_20201120/images/snap_%05d.png -c:v libx264 -pix_fmt yuv420p -crf 23 P12_size3.mp4
# ffmpeg -t 20 -i P12_size6.mp4 -vf "fps=10,scale=520:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 output.gif
# ffmpeg -t 20 -i video.mp4 -vf "fps=10,scale=520:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 output.gif
#TODO

