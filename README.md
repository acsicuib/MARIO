# Description

MARIOII Scholar.



## Acknowledge

```text
PENDING
```

In the article you can understand the behavior implemented in this project. Here we briefly comment on those aspects taken into account for its implementation:


# Implementation of MARIO and Users(Taxis) in YAFS 
YAFS is a skeleton for simulating Fog environments, and it accepts specific behaviors that allow it to adapt to any scenario. In this case, there is a DES process to implement MARIO; another DES process that controls the movement of users, and finally, as many DES processes as instances of applications are deployed.

The next files provide this behaviour to the simulator. 
```bash
enviroment folder
├── app_operator.py
├── userMovement.py
├── agent.py
├── path_routing.py
├── tiledTopology.py
├── problogRulesGenerator.py
```

- Mario is implemented in app_operator.py file. It controls the service(agent) actions (migrate, replicate, undeploy,...). For that, it is able to generates new instances and remove instance from the simulator (DES processes)
- The movements of the users is managed by userMovement.py. In each activation changes the position of the users in the topology. 
- Each instance of the application deployed in the simulator is based on agent.py. This agent.py uses Prolog to take a decision (migrate, replicate,...)
- path_routing.py is the algorithm to route user requests to the instances. It chooses the most suitable path in the topology and it orchestra the service. It has a routing cahge to optimize the simulation performace. This cache is reset when MARIO performs any action.
- tiledTopology.py generates the topology used in this experiment. It is explained later.
- problogRulesGenerator.py, it is a simple parser to generate the simulator facts into Prolog facts.


# Definition of a scenario

A scenario has the following folder structure:
```bash
TaxiRome
├── config.ini
├── data
│   ├── raw_trajectories.csv
│   └── traces.json
├── policy
│   ├── agentRequests.pl
│   ├── allocDefinition.json
│   ├── appDefinition.json
│   ├── lib.pl
│   ├── policy1.pl
│   ├── policy2.pl
│   ├── policy3.pl
│   └── policy4.pl
├── Cesium_viewer
│   ├── CZML\ Path.html
│   ├── all_tracks.txt
│   ├── generate_CZML_fromCSV_toCESIUM.py
│   └── index.html
├── test_data_on_topology.py
├── test_traces_info.py
|
```

Files: 
- config.ini has a list of variables regarding with the parametrization of the simulations, mainly event periods
```text
[simulation]
nSimulations = 1
time_in_each_step = 2000
trackSteps = 100
stopSteps = 25

[topology]
size = 8
HwReqs_cloud_node = 14
shape_cloud_node = (2,7)
IPT = 100
BW = 1
# HwReqs = level + 2
# PR = level * 2

[appOperator]
activation_period = 100

[agent]
activation_period = 200
message_period = 40
window_agent_outcome = 5
```
-  File data/raw_trajectories.csv contains the traces of the RomeTaxis. YAFS uses trackAnimation library to normalize the movements in periods. Each taxi trajectory 
is normalized (same initial time and same speed). It is necessary transform this data:
```python
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
```
-  policy/--.pl are the prolog files with different policies 1--4
-  policy/allocDefinition.json describes the initial allocation of the instances
```json
  "initialAllocation": [
    {
      "module_name": "1_01",
      "app": 1,
      "id_resource": "n0lt0ln0"
    },
```
-  policy/appDefinition.json defines the apps
```json
[
  {
    "id": 1,
    "name": 1,
    "HwReqs": 1,
    "MaxReqs": 5,
    "MaxLatency": 10,
    "profile_rules": "policy1.pl",  

    "transmission": [
      {
        "message_in": "M.USER.APP.1",
        "module": "1_01"           
      }
    ],
    "module": [
      {
        "id": 1,
        "name": "1_01",
        "type": "MODULE",
        "RAM": 1
      }
    ],
    "message": [
      {
        "id": 0,
        "name": "M.USER.APP.1",
        "s": "None",
        "d": "1_01",
        "bytes": 1,
        "instructions": 1
      }
    ]
  },

```
- Cesium_viewer folder contains HTML files to view the 3D topology on its real map projection using [Cesium](https://cesium.com/)
- test_data_on_topology.py generates JSON data of the topology to be included in Cesium_viewer/index.html (it is a manual process)
- test_traces_info.py generates JSON data of the traces to be included in Cesium_viewer/CZML Path.html (it is a manual process)

A scenario has other definitions such as topology, routing algorithm, MARIO app and movement manager. All of them are defined in the main.py file.

# Topology structure

In YAFS the network topology is defined using NetworkX library. In our case, we define a Tiled topology to cover a surface.

You have some examples of the TiledTopology using Cesium_viewer/index.html and test_data_on_topology.py

- Topology with a size of 4 x 4 edge nodes 
<img src="https://github.com/acsicuib/MARIO/raw/MarioII/media/Topology_4_Rome.gif" width="630" height="316"/></a>
- Topology with a size of 16 x 16 edge nodes 
<img src="https://github.com/acsicuib/MARIO/raw/MarioII/media/Topology_16_Rome.gif" width="630" height="316"/></a>


The topology is defined in main.py
```python
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
#...
nx.set_edge_attributes(t.G,name="PR",values=attPR)

## Mandatory attr. on nodes
# HwReqs = level + 2
attHW = {}
#...
attHW[cloudNode] = int(config.get('topology', 'HwReqs_cloud_node')) #THE CLOUD Node capacity BIGGER NUMBER OF APPS

#SHAPE attributes are for visualization purpose. It is related to attHW capacity
# ...
# IPT
attIPT = {x:int(config.get('topology', 'IPT')) for x in t.G.nodes()}
nx.set_node_attributes(t.G,name="IPT",values=attIPT)
nx.set_node_attributes(t.G,name="HwReqs",values=attHW)

t.write(temporal_folder + "network_%i.gexf" % tiledTopo.size)

nx.set_node_attributes(t.G,name="shape",values=attShape) #attr. shape is not supported by gexf format - before write()-
#for render inside matplotlib
tiledTopo.setPosPlot(t.G,[[0,0],[20,20]])
```


# main.py

The experiments in main.py are automated:

```python
experiments = [
    ("P1_s3","Rome","scenarios/TaxiRome/","policy/",[[41.878037, 12.4462643], [41.919234, 12.5149603]],"policy1.pl"),
    ("P2_s3","Rome","scenarios/TaxiRome/","policy/",[[41.878037, 12.4462643], [41.919234, 12.5149603]],"policy2.pl"),
    ("P3_s3","Rome","scenarios/TaxiRome/","policy/",[[41.878037, 12.4462643], [41.919234, 12.5149603]],"policy3.pl"),
    ("P4_s3","Rome","scenarios/TaxiRome/","policy/",[[41.878037, 12.4462643], [41.919234, 12.5149603]],"policy4.pl")
]
```
- "P1_s3" is the code of the experiment
- "Rome" is the another name
- "scenarios/TaxiRome" is the folder with the previous structure
- "policy" is the folder with some information: policy, apps, ...
- "[..]" is the projection of the topology in a coordinate system. This value can be none and the simulator takes the users' trajectories boundaries.
- "policyX.pl" is the mandatory policy that all apps will use in the simulator. This value can be none and the simulator uses the value defined in the json.  
    

# Simulation results

In YAFS simulations you obtain two default files: Result_.csv and Results_link.csv. Both files contains the simulation traces.
In this simulation, we added more data gathers in MARIO to get movements, actions, etc. The extra files are:
 
```bash
├── actions_stats.txt
├── movements.csv
├── moves_stats.txt
```

- actions_stats.txt is a CSV file. It contains the number of operations performed by MARIO in each activation where there are service(agent) requests.
```csv 
time,undeploy,nop,migrate,replicate,none
300,0,1,0,5,0
500,0,5,0,6,0
700,0,7,1,9,0
```

- movements.txt is a CSV file. It has all the handovers of each user(taxi). DES represents the identifier of the user in the simulation. Thus, you can link this information of the user (DES) with the requests traces in the simulator ( *Results* files). 
```csv 
taxi,DES,time,nodeSRC,nodeDST
taxi_122,9,0,none,n3lt7ln6
taxi_260,10,0,none,n3lt0ln2
taxi_351,11,0,none,n3lt6ln5
taxi_197,12,0,none,n3lt1ln2
```
- moves_stats.txt is a CSV file.  It relates service request (idService) of a app type (app) with its action in a specific time.
```csv 
idService,app,time,action
7,6,300,nop
6,5,300,replicate
5,4,300,replicate
4,3,300,replicate
3,2,300,replicate
2,1,300,replicate
```

# Analysing the results

The complexity of analyzing the YAFS results is just as complex as the complexity of the scenario. 
**Keep calm and learn PANDAS**

There is three files to generate information about the results:
- plot_actions.py. It generates a bar plot with the number of actions in each MARIO activation. 
<img src="https://github.com/acsicuib/MARIO/raw/MarioII/media/plot_action.png" width="630" height="316"/></a>
- plot_pearsonCoefficient.py. It generates a plot about the relationship between actions and users movements.
<img src="https://github.com/acsicuib/MARIO/raw/MarioII/media/plot_pearson.png" width="630" height="316"/></a>
- plot_averageActionType.py. It generates a text description about the average number of action by each user movement.


# Snapshots
<img src="https://github.com/acsicuib/MARIO/raw/MarioII/media/snap_example.png" width="330" height="266"/></a>

There are two different snapshots: one capture each service(agent) decision, another capture all service(agent) decisions. The first gives an exact idea of each action and at each moment. It is most useful for debugging.
The second alleviates this highly detailed build and captures all the movements made with each activation of MARIO.
Taking snapshots slows down the simulation.

The first one is controlled by an argument to MARIO constructor:
```python
appOp = Mario(globalrules,service_rule_profile, path_csv_files,
    ...
    render=False, # HERE 
```

The second one is a global variable inside MARIO class:
```python
RENDERSNAP = False
class Mario():
    ...
```


#  How to run the project
This is the recipe to run the project under a UBUNTU server. Each Policy simulation lasts about 1 hour on a MacBookPro. This time is longer if you take captures of each movement or group of movements to make the video. 
Be aware of your system and paths.

[First step is install Prolog:](https://www.swi-prolog.org/build/PPA.html)
Please, be sure that this command works in your system:
```bash
$ swipl 
Welcome to SWI-Prolog (threaded, 64 bits, version 8.2.1)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- 
```

Ok, thus... you should clone the project and configure Python third libraries (tested on Ubuntu Server 20.0) 
```
git clone --branch MarioII https://github.com/acsicuib/MARIO

#git reset --hard HEAD
#git pull
git pull origin MarioII

apt-get install python3-venv
apt install pkg-config 
python3 -m pip install virtualenv
python3 -m venv marioenv
source marioenv/bin/activate
(mariovenv - configure a virtual env.)
 
python -m pip install -r requirements.txt 

(on MARIO/multi-agent-policies/ folder)
export PYTHONPATH="/home/isaac/projects/MARIO:/home/isaac/projects/MARIO/multi-agent-policies/environment:$PYTHONPATH"
python3 main.py

(video generation - or -)
ffmpeg -r 1 -i multi-agent-policies/scenarios/TaxiRome/results_20201028/images/network_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p video.mp4
ffmpeg -framerate 10 -i multi-agent-policies/scenarios/TaxiRome/results_P12_20201120/images/snap_%05d.png -c:v libx264 -pix_fmt yuv420p -crf 23 P12_size3.mp4

ffmpeg -framerate 10 -i multi-agent-policies/scenarios/TaxiRome/results_P12_20201122/images/snap_%05d.png -c:v libx264 -pix_fmt yuv420p -crf 23 P1_size3.mp4
ffmpeg -framerate 10 -i multi-agent-policies/scenarios/TaxiRome/results_P1_s3_20201124/images/network_%05d.png -c:v libx264 -pix_fmt yuv420p -crf 23 P1_size3.mp4
(This lines are for me ;) -  copypaste)
scp isaac@deepblue:/home/isaac/projects/MARIO/video.mp4 .
scp isaac@deepblue:/home/isaac/projects/MARIO/multi-agent-policies/scenarios/TaxiRome/results_20201028/models/rules_swi_UID96_nn0lt0ln0_s0_X_9700.pl .
```
