"""
    @author: Antonio Brogi & Stefano Forti & Carlos Guerrero & Isaac Lera
"""
import os
import time
import json
import logging.config
import gpxpy.gpx
import datetime
import subprocess
import pandas as pd
import networkx as nx
from pathlib import Path

from yafs.core import Sim
from yafs.application import Application, Message
from yafs.coverage import CircleCoverage
from yafs.topology import Topology
from yafs.distribution import *
from yafs.utils import fractional_selectivity
from yafs.placement import JSONPlacement

import trackanimation

from environment.path_routing import DeviceSpeedAwareRouting
from environment.workload import DynamicWorkload
from environment.app_operator import Mario
from environment.problogRulesGenerator import Rules
from userMovement import UserControlMovement
from tiledTopology import TiledTopology


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

def do_video_from_execution_snaps(output_file, png_names, framerate):
    cmdstring = ('ffmpeg',
                 '-loglevel', 'quiet',
                 '-framerate', str(framerate),
                 '-i', png_names,
                 '-r', '25',
                 '-s', '1280x960',
                 '-pix_fmt', 'yuv420p',
                 output_file + '.mp4'
                 )

    subprocess.call(cmdstring)

def parser_CSVTaxiRome_toGPXfiles(inputCSVfile, temporalfolder):
    df = pd.read_csv(inputCSVfile, ",")
    df = df.rename(columns={"taxi id": "taxi"})
    dfg = df.groupby(["taxi"])

    try:
        os.makedirs(temporalfolder)
    except OSError:
        None

    for idx, (group_name, df_group) in enumerate(dfg):
        # Important taxi routes are sorted by time
        gpx = gpxpy.gpx.GPX()
        gpx_track = gpxpy.gpx.GPXTrack()
        gpx.tracks.append(gpx_track)
        gpx_segment = gpxpy.gpx.GPXTrackSegment()
        gpx_track.segments.append(gpx_segment)

        # Create points:
        for idx in df_group.index:
            date_time_obj = datetime.datetime.strptime(df_group.loc[idx]["date time"], '%Y-%m-%d %H:%M:%S')

            gpx_segment.points.append(gpxpy.gpx.GPXTrackPoint(df_group.loc[idx].latitude, df_group.loc[idx].longitude,
                                                              time=date_time_obj,
                                                              elevation=0
                                                              ))

        with open(temporalfolder+'taxi_%i.gpx' % group_name, 'w') as f:
            f.write(gpx.to_xml())

    return temporal_folder+'trajectories/'

def main(number_simulation_steps,time_in_each_step, experiment_path,policy_folder,temporal_folder,case, tracks,projection,doExecutionVideo, it):
                                                                              
    stop_time = number_simulation_steps * time_in_each_step

    # results_dir = Path(experiment_path+"results/")
    # results_dir.mkdir(parents=True, exist_ok=True)
    # results_dir = str(results_dir)
    path_csv_files = temporal_folder + "/Results_%s_%i" % (case,it)

    """
    TOPOLOGY
    """
    t = Topology()

    tiledTopo = TiledTopology(4)
    t.G = tiledTopo.TiledGraph(projection)


    # Definition of mandatory attributes
    ## on edges
    # PR and BW are 1 unit
    attPR_BW = {x:1 for x in t.G.edges()}
    nx.set_edge_attributes(t.G,name="PR",values=attPR_BW)
    nx.set_edge_attributes(t.G,name="BW",values=attPR_BW)
    ## on nodes
    # HwReqs = level + 2
    # attHW = {x:abs(tiledTopo.getNumberLayers()-int(x[0]))+2 for x in t.G.nodes()} # node name: "000"
    attHW = {x:abs(tiledTopo.getNumberLayers()-tiledTopo.getLevel(x))+2 for x in t.G.nodes()} #node name:n0lt0ln0
    attHW["n0lt0ln0"] = 10 #THE CLOUD Node capacity BIGGER NUMBER OF APPS
    # Shape": "(1,level+2)",
    attShape = {x:"(1,%i)"%attHW[x] for x in t.G.nodes()}
    attShape["n0lt0ln0"] = "(2,5)"
    # IPT
    attIPT = {x:100 for x in t.G.nodes()}
    nx.set_node_attributes(t.G,name="IPT",values=attIPT)
    nx.set_node_attributes(t.G,name="shape",values=attShape)
    nx.set_node_attributes(t.G,name="HwReqs",values=attHW)

    #for render
    tiledTopo.setPosPlot(t.G,[[0,0],[20,20]])


    # t.write(path +"network_%i.gexf"%tiledTopo.size)
    """
    Global Rules for all services
    """
    globalrules = Rules()

    """
    APPLICATION
    """
    dataApp = json.load(open(experiment_path + policy_folder + 'appDefinition.json'))
    apps = create_applications_from_json(dataApp)

    for app in dataApp:
        globalrules.and_rule("service",app["name"],app["HwReqs"],app["MaxReqs"],app["MaxLatency"])

    service_rule_profile={}
    for app in dataApp:
        service_rule_profile[app["name"]]=experiment_path+policy_folder+app["profile_rules"] # Global path to pl.file



    """
    PLACEMENT algorithm
    """
    placementJson = json.load(open(experiment_path + policy_folder + 'allocDefinition.json'))
    placement = JSONPlacement(name="Placement", json=placementJson)

    """
    SELECTOR and Deploying algorithm
    """
    selectorPath = DeviceSpeedAwareRouting()

    """
    SIMULATION ENGINE
    """
    s = Sim(t, default_results_path=path_csv_files)
    s.set_mobile_fog_entities(dict({}))
    s.load_user_tracks(tracks)
    s.set_coverage_class(CircleCoverage, radius=5)


    """
    INITIAL DEPLOY OF SERVICES
    """
    for aName in apps.keys():
        s.deploy_app(apps[aName], placement, selectorPath)

    """
    MARIO app controller & Agent generator
    """
    time_activation = deterministic_distribution(time=100, name="Deterministic")
    PROBLOG = False
    appOp = Mario(globalrules,service_rule_profile, path_csv_files, app_number=len(dataApp),period=1000,render=True,path_results=temporal_folder)
    s.deploy_monitor("App-Operator", appOp, time_activation,
                     **{"sim": s,
                        "routing": selectorPath,
                        "path": experiment_path+policy_folder,
                        }
                     )


    """
    Creating the custom monitor that manages the user movements in the simulator
    USER operations that this DES-process manages: CREATION, MOVEMENT & DROP
    """
    users = set(tracks.df.CodeRoute)
    listIdApps = [x["id"] for x in dataApp]

    dStart = deterministicDistributionStartPoint(0, time_in_each_step, name="Deterministic")
    evol = UserControlMovement(experiment_path, doExecutionVideo, tiledTopo,users,listIdApps)
    s.deploy_monitor("Traces_localization_update", evol, dStart,
                     **{"sim": s,
                        "routing": selectorPath,
                        "case": case,
                        "stop_time": stop_time,
                        "it": it})

    s.set_movement_control(evol)


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
    print("\nNumber of different connections for user movements: %i"%evol.total_diff_connections)


if __name__ == '__main__':
    import logging.config
    logging.config.fileConfig(os.getcwd() + '/logging.ini')


    experiments = [
        # Name , folderExperiment, folderPolicy , projection=None
        ("Rome","scenarios/TaxiRome/","policy_getcloser/",[[41.878037, 12.4462643], [41.919234, 12.5149603]])
    ]


    for name,experiment_path,policy_folder,projection in experiments:
        print("Scenario definition: ",experiment_path)

        # Generating a temporal folder to record results
        # datestamp = time.strftime('%Y%m%d')

        datestamp = "20201028" # fixed for testing
        temporal_folder = experiment_path + "results_" + datestamp + "/"
        try:
            os.makedirs(temporal_folder)
        except OSError:
            None


        ##
        # STEP1:
        # Initializing of the common and static context of each simulation
        ##

        # 1.1 Mobile entities trough GPX traces
        # The track normalization is an expensive computational task. A cached file is generated in each experiment path
        if os.path.isfile(temporal_folder + "normalized_trajectories.csv"):
            input_directory = temporal_folder + "normalized_trajectories.csv"  #
            logging.info("Loading trajectories from (cached file): %s" % input_directory)
            tracks = trackanimation.read_track(input_directory)
        else:
            pathGPXs = temporal_folder+"trajectories/"
            if not os.path.isdir(pathGPXs):
                logging.info("Generating trajectories from (raw CSV file)")
                input_directory = experiment_path + "data/raw_trajectories.csv"  #
                pathGPXs = parser_CSVTaxiRome_toGPXfiles(input_directory, pathGPXs)

            logging.info("Loading trajectories from (raw GPX files): %s" % pathGPXs)
            tracks = trackanimation.read_track(pathGPXs)
            tracks = tracks.time_video_normalize(time=number_simulation_steps, framerate=1)  # framerate must be one
            tracks.export(temporal_folder + "normalized_trajectories")


        if projection == None:
            ## default boundary projection - Using tracks limits
            projection = [[tracks.df.Latitude.min(), tracks.df.Longitude.min()],
                      [tracks.df.Latitude.max(), tracks.df.Longitude.max()]]
        ## else specific boundary


        total_movements_in_tracks = tracks.df.VideoFrame.max()
        print("Total movements in the tracks: %i"%total_movements_in_tracks)

        number_simulation_steps = total_movements_in_tracks
        time_in_each_step = 2000
        nSimulations = 1  # iteration for each experiment

        # Iteration for each experiment changing the seed of randoms
        for iteration in range(nSimulations):
            random.seed(iteration)
            np.random.seed(iteration)
            logging.info("Running multi-agent-policies - %s" %experiment_path)

            start_time = time.time()
            main(number_simulation_steps=number_simulation_steps,
                 time_in_each_step = time_in_each_step,
                 experiment_path=experiment_path,
                 policy_folder = policy_folder,
                 temporal_folder = temporal_folder,
                 case=name,
                 tracks=tracks,
                 projection=projection,
                 doExecutionVideo=True,  # expensive task
                 it=iteration)

            print("\n--- %s seconds ---" % (time.time() - start_time))
            do_video_from_execution_snaps(temporal_folder + "animation_snaps", 'snap_%05d.png', 10)

    print("Simulation Done!")



# ffmpeg -r 1 -i results/images/network_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p results/out2.mp4
# ffmpeg -r 1 -i results/images/network_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p results/out2.mp4