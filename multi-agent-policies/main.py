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
from configparser import ConfigParser

from yafs.core import Sim
from yafs.application import Application, Message
from yafs.coverage import CircleCoverage
from yafs.topology import Topology
from yafs.distribution import *
from yafs.utils import fractional_selectivity
from yafs.placement import JSONPlacement

import trackanimation

from environment.path_routing import DeviceSpeedAwareRouting
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

    # onlyRoutes = {2,37} #TODO TEST
    for idx, (group_name, df_group) in enumerate(dfg):
        # if group_name in onlyRoutes:  #TODO TEST
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

def main(number_simulation_steps,
         time_in_each_step,
         experiment_path,
         policy_folder,
         temporal_folder,
         case,
         tracks,
         projection,
         config,
         doExecutionVideo,
         it,
         policy_file = None # if None we use the policy defined in the APP
         ):


    simulation_duration = number_simulation_steps * time_in_each_step

    """
    TOPOLOGY
    """
    t = Topology()
    tiledTopo = TiledTopology(int(config.get('topology', 'size')))
    t.G = tiledTopo.TiledGraph(projection)
    cloudNode = "n0lt0ln0"
    # Definition of mandatory attributes
    ## on edges
    # PR and BW
    attBW = {x:int(config.get('topology', 'BW')) for x in t.G.edges()}
    nx.set_edge_attributes(t.G,name="BW",values=attBW)

    attPR = {}
    for (s,d) in t.G.edges():
        print(s,d)
        minV = min(tiledTopo.getLevel(d),tiledTopo.getLevel(s))
        pr = (tiledTopo.getNumberLayers()-minV)
        attPR[(s,d)] = pr-1

    nx.set_edge_attributes(t.G,name="PR",values=attPR)

    ## on nodes
    # HwReqs = level + 2
    # WAYS TO DEFINE node HW capacity
    attHW = {}
    # attHW = {x:abs(tiledTopo.getNumberLayers()-int(x[0]))+2 for x in t.G.nodes()} # node name: "000"
    # attHW = {x:abs(tiledTopo.getNumberLayers()-tiledTopo.getLevel(x))+6 for x in t.G.nodes()} #node name:n0lt0ln0
    # attHW = {x:abs(tiledTopo.getNumberLayers()-tiledTopo.getLevel(x))+6for x in t.G.nodes()} #node name:n0lt0ln0
    # attHW = {x: abs(tiledTopo.getNumberLayers() - tiledTopo.getLevel(x)) * 1 for x in t.G.nodes()}  # node name:n0lt0ln0

    for x in t.G.nodes():
        l = abs(tiledTopo.getNumberLayers() - tiledTopo.getLevel(x))
        attHW[x]=2*l
        if l == 1:
            attHW[x] = 1

    attHW[cloudNode] = int(config.get('topology', 'HwReqs_cloud_node')) #THE CLOUD Node capacity BIGGER NUMBER OF APPS

    # Shape": "(1,level+2)",
    ### attShape = {x:"(1,%i)"%attHW[x] for x in t.G.nodes()} #OLD
    attShape = {}
    for x in t.G.nodes():
        # if attHW[x]%2==0:
        #     attShape[x]="(2,%i)"%(attHW[x]//2)
        # else:
            attShape[x]="(1,%i)"%(attHW[x])

    attShape[cloudNode] = config.get('topology', 'shape_cloud_node')
    # IPT
    attIPT = {x:int(config.get('topology', 'IPT')) for x in t.G.nodes()}
    nx.set_node_attributes(t.G,name="IPT",values=attIPT)
    nx.set_node_attributes(t.G,name="HwReqs",values=attHW)

    t.write(temporal_folder + "network_%i.gexf" % tiledTopo.size)


    nx.set_node_attributes(t.G,name="shape",values=attShape) #attr. shape is not supported by gexf format - before write()-
    #for render
    tiledTopo.setPosPlot(t.G,[[0,0],[20,20]])

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
        if policy_file == None:
            service_rule_profile[app["name"]]=experiment_path+policy_folder+app["profile_rules"] # Global path to pl.file
        else:
            service_rule_profile[app["name"]] = experiment_path + policy_folder + policy_file

    """
    PLACEMENT algorithm
    """
    placementJson = json.load(open(experiment_path + policy_folder + 'allocDefinition.json'))
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
    s.set_mobile_fog_entities(dict({}))
    s.load_user_tracks(tracks)
    s.set_coverage_class(CircleCoverage, radius=5)


    """
    INITIAL DEPLOY OF SERVICES
    """
    for aName in apps.keys():
        s.deploy_app(apps[aName], placement, routingPath)

    """
    MARIO app controller & Agent generator
    """
    time_activation = deterministic_distribution(time=int(config.get('appOperator', 'activation_period')), name="Deterministic")
    appOp = Mario(globalrules,service_rule_profile, path_csv_files,
                  app_number=len(dataApp),
                  period=int(config.get('agent', 'activation_period')),
                  render=True, # only snaps
                  path_results=temporal_folder,
                  cloud_node = cloudNode,
                  window_agent_size=int(config.get('agent', 'window_agent_outcome')))

    s.deploy_monitor("App-Operator", appOp, time_activation,
                     **{"sim": s,
                        "routing": routingPath,
                        "path": experiment_path+policy_folder,
                        }
                     )

    """
    Create the custom monitor that it manages the user movements according with the traces
    The user operations are: CREATION, MOVEMENT (change associated node)
    """
    AppsIDs = [x["id"] for x in dataApp] #get a list with all ID-apps

    dStart = deterministicDistributionStartPoint(0, time_in_each_step, name="Deterministic")

    record_movements = open(temporal_folder+"/movements.csv","w")
    record_movements.write("taxi,DES,time,nodeSRC,nodeDST\n")
    evol = UserControlMovement(
                        experiment_path = experiment_path,
                        doExecutionVideo = doExecutionVideo,
                        tiledTopo = tiledTopo,
                        listIdApps = AppsIDs,
                        appOp = appOp,
                        record_movements = record_movements,
                        limit_steps = int(config.get('simulation', 'stopSteps')),
                        ratio_message = int(config.get('agent', 'message_period'))
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
    logging.info(" Performing simulation: %s %i "%(case,it))
    s.run(simulation_duration)  # To test deployments put test_initial_deploy a TRUE

    """
    Storing results from other monitors
    & Render the last movement
    """
    # appOp.render(s,experiment_path,routingPath,["END",-1,-1,"NONE"])
    # evol.write_map_user_des(temporal_folder + "/MapUserDES_%s_%i.csv" % (case, it))
    record_movements.close()
    s.print_debug_assignaments()
    appOp.close()
    print("\nNumber of different connections for user movements: %i"%evol.total_diff_connections)


if __name__ == '__main__':
    import logging.config
    logging.config.fileConfig(os.getcwd() + '/logging.ini')

    ### Control the simulation with monit
    pid = str(os.getpid())
    with open("/tmp/yafssimulation.pid", "w") as f:
        f.write(pid)

    # Case, Name , folderExperiment, folderPolicy , projection=None, policy_file = None
    experiments = [
        ("P1_s3","Rome","scenarios/TaxiRome/","policy/",[[41.878037, 12.4462643], [41.919234, 12.5149603]],"policy1.pl"),
        # ("P2_s3","Rome","scenarios/TaxiRome/","policy/",[[41.878037, 12.4462643], [41.919234, 12.5149603]],"policy2.pl"),
        # ("P3_s3","Rome","scenarios/TaxiRome/","policy/",[[41.878037, 12.4462643], [41.919234, 12.5149603]],"policy3.pl"),
        # ("P4_s3","Rome","scenarios/TaxiRome/","policy/",[[41.878037, 12.4462643], [41.919234, 12.5149603]],"policy4.pl")
    ]

    for ncase, name,experiment_path,policy_folder,projection,policy_file in experiments:
        print("Experiment definition: ",experiment_path)
        config = ConfigParser()
        config.read(experiment_path+'config.ini')

        # Generating a temporal folder to record results
        # datestamp = time.strftime('%Y%m%d')
        datestamp = "20201124" # fixed for testing

        temporal_folder = experiment_path + "results_%s_"%ncase + datestamp + "/"
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
            tracks = tracks.time_video_normalize(time=int(config.get('simulation', 'trackSteps')), framerate=1)  # framerate must be one
            tracks.export(temporal_folder + "normalized_trajectories")


        if projection == None:
            ## default boundary projection - Using tracks limits
            projection = [[tracks.df.Latitude.min(), tracks.df.Longitude.min()],
                      [tracks.df.Latitude.max(), tracks.df.Longitude.max()]]
        ## else specific boundary

        total_movements_in_tracks = tracks.df.VideoFrame.max()
        # print("Total movements in the tracks: %i"%total_movements_in_tracks)
        # number_simulation_steps = total_movements_in_tracks+1 #+1 to enable the last movement
        number_simulation_steps = int(config.get('simulation', 'stopSteps'))+1 #+1 to enable the last movement

        time_in_each_step = int(config.get('simulation', 'time_in_each_step'))
        nSimulations = int(config.get('simulation', 'nSimulations'))

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
                 config = config,
                 doExecutionVideo=True,  # expensive task
                 it=iteration,
                 policy_file = policy_file)

            print("\n--- %s seconds ---" % (time.time() - start_time))
            do_video_from_execution_snaps(temporal_folder + "animation_snaps", 'snap_%05d.png', 10)

    print("Simulation Done!")



# ffmpeg -r 1 -i results/images/network_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p results/out2.mp4
# ffmpeg -r 1 -i results/images/network_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p results/out2.mp4
# ffmpeg -r 1 -i multi-agent-policies/scenarios/TaxiRome/results_20201028/images/network_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p video.mp4
# ffmpeg -r 1 -i multi-agent-policies/scenarios/TaxiRome/results_P1_20201028/images/snap_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p P1_size6.mp4


#ffmpeg -framerate 10 -i multi-agent-policies/scenarios/TaxiRome/results_P1_20201028/images/snap_%05d.png -c:v libx264 -pix_fmt yuv420p -crf 23 P1_size6.mp4
#ffmpeg -framerate 10 -i multi-agent-policies/scenarios/TaxiRome/results_P1_20201120/images/snap_%05d.png -c:v libx264 -pix_fmt yuv420p -crf 23 P1_size3.mp4
#ffmpeg -framerate 10 -i multi-agent-policies/scenarios/TaxiRome/results_P12_20201028/images/snap_%05d.png -c:v libx264 -pix_fmt yuv420p -crf 23 P12_size6.mp4
#ffmpeg -framerate 10 -i multi-agent-policies/scenarios/TaxiRome/results_P12_20201120/images/snap_%05d.png -c:v libx264 -pix_fmt yuv420p -crf 23 P12_size3.mp4
# ffmpeg -t 20 -i P12_size6.mp4 -vf "fps=10,scale=520:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 output.gif