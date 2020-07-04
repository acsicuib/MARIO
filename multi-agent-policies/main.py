"""
    @author: Antonio Brogi & Stefano Forti & Carlos Guerrero & Isaac Lera
"""
import os
import time
import json
import logging.config
from pathlib import Path
import sys
from yafs.core import Sim
from yafs.application import Application, Message
from yafs.topology import Topology
from yafs.distribution import *
from yafs.utils import fractional_selectivity

from yafs.placement import JSONPlacement

from environment.path_routing import DeviceSpeedAwareRouting
from environment.workload import DynamicWorkload
from environment.app_operator import Mario
from environment.problogRulesGenerator import Rules

def create_applications_from_json(data):
    applications = {}
    for app in data:
        a = Application(name=app["name"])
        modules = [{"None":{"Type":Application.TYPE_SOURCE}}]
        for module in app["module"]:
            modules.append({module["name"]: {"RAM": module["RAM"], "Type": Application.TYPE_MODULE}})
        a.set_modules(modules)

        ms = {}
        for message in app["message"]:
            #print "Creando mensaje: %s" %message["name"]
            ms[message["name"]] = Message(message["name"],message["s"],message["d"],instructions=message["instructions"],bytes=message["bytes"])
            if message["s"] == "None":
                a.add_source_messages(ms[message["name"]])

        #print "Total mensajes creados %i" %len(ms.keys())
        for idx, message in enumerate(app["transmission"]):
            if "message_out" in message.keys():
                a.add_service_module(message["module"],ms[message["message_in"]], ms[message["message_out"]], fractional_selectivity, threshold=1.0)
            else:
                a.add_service_module(message["module"], ms[message["message_in"]])

        applications[app["name"]]=a

    return applications

def main(simulated_time, experiment_path,case,it):

    stop_time = simulated_time
    results_dir = Path(experiment_path+"results/")
    results_dir.mkdir(parents=True, exist_ok=True)
    results_dir = str(results_dir)
    path_csv_files = results_dir + "/Results_%s_%i_%i" % (case, stop_time, it)

    """
    TOPOLOGY from a json
    """
    t = Topology()
    dataNetwork = json.load(open(experiment_path + 'networkDefinition.json'))
    t.load_all_node_attr(dataNetwork)
    # t.write(path +"network.gexf")

    """
    Global Rules for all services
    """
    globalrules = Rules()

    """
    APPLICATION
    """
    dataApp = json.load(open(experiment_path + 'appDefinition.json'))
    apps = create_applications_from_json(dataApp)

    for app in dataApp:
        globalrules.and_rule("service",app["name"],app["HwReqs"],app["MaxReqs"],app["MaxLatency"])

    service_rule_profile={}
    for app in dataApp:
        service_rule_profile[app["name"]]=experiment_path+app["profile_rules"] # Global path to pl.file



    """
    PLACEMENT algorithm
    """
    # In our model only initial cloud placements are enabled
    placementJson = json.load(open(experiment_path + 'allocDefinition.json'))
    placement = JSONPlacement(name="Placement", json=placementJson)

    """
    SELECTOR and Deploying algorithm
    """
    selectorPath = DeviceSpeedAwareRouting()


    """
    SIMULATION ENGINE
    """
    s = Sim(t, default_results_path=path_csv_files)


    """
    POPULATION algorithm
    """
    dataPopulation = json.load(open(experiment_path + 'usersDefinition.json'))
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

        s.deploy_app(apps[aName], placement, pop_app, selectorPath)

    """
    MARIO app controler & Agent generator
    """
    time_activation = deterministic_distribution(time=100, name="Deterministic")
    PROBLOG = False
    appOp = Mario(globalrules,service_rule_profile, path_csv_files, app_number=len(dataApp),period=1000,render=True)
    s.deploy_monitor("App-Operator", appOp, time_activation, **{"sim": s, "routing": selectorPath, "path":experiment_path})

    """
    RUNNING
    """
    logging.info(" Performing simulation: %s %i "%(case,it))
    s.run(stop_time)  # To test deployments put test_initial_deploy a TRUE

    """
    Storing results from other monitors
    & Render the last movement
    """
    appOp.render(s,experiment_path,selectorPath,["END",-1,-1,"NONE"])
    s.print_debug_assignaments()


if __name__ == '__main__':
    import logging.config
    logging.config.fileConfig(os.getcwd() + '/logging.ini')


    # experiment_path = "scenarios/policy_getcloser/"
    # experiment_path = "scenarios/policy_ecobalance/"
    experiment_path = "scenarios/FOCLASA2020/policy_getcloser/"

    print("Scenario definition: ",experiment_path)

    nSimulations = 1
    timeSimulation = 15000

    for i in range(nSimulations):
        start_time = time.time()
        random.seed(i)
        np.random.seed(i)
        logging.info("Running multi-agent-policies - %s" %experiment_path)

        main(simulated_time=timeSimulation, experiment_path=experiment_path, case='prot1',it=i)

        print("\n--- %s seconds ---" % (time.time() - start_time))

    print("All simulations done")

# ffmpeg -r 1 -i results/images/network_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p results/out2.mp4