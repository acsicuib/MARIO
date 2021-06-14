 

# Description
 
### Osmotic Application and Infrastructure Management:A Decentralised Declarative Solution
 
Managing next-gen applications over Fog computing infrastructures is challenging and calls for new methodologies and tools that enable flexible, scalable and application-specific management along the Cloud-IoT continuum. This article proposes a novel approach for realising a fully decentralised application management in opportunistic Fog computing infrastructures. We present a Prolog implementation of the approach and assess it over some motivating scenarios by experimenting with three different management policies via simulation.
 
- Extending the initial idea of: https://github.com/acsicuib/MARIO/tree/gauss2020
- Journal of Software: Evolution and Process
- Status: sending 

## Acknowledge

```text
PENDING
```

In the article you can understand the behavior implemented in this project. Here we briefly comment on those aspects taken into account for its implementation:


# Implementation of Application and Node Managers 

YAFS is a skeleton for simulating Fog environments, and it accepts specific behaviors that allow it to adapt to any scenario. In this case, there is a DES process to implement MARIO; another DES process that controls the movement of users, and finally, as many DES processes as instances of applications are deployed.
The next files provide this behaviour to the simulator. 
```bash
enviroment folder
├── agent.py 
├── node_manager.py
├── userMovement.py
├── path_routing.py
├── workload.py
├── problogRulesGenerator.py
```

- Application Manager is implemented in agent.py file. It controls the service(agent) actions (migrate, replicate, undeploy,...). For that, it is able to generates new instances and remove instance from the simulator (DES processes)
- Node Manager is implemented in node_manager.py. Each NM supervises the actions requested in itfself node.  
- path_routing.py is the algorithm to route user requests to the instances. It chooses the most suitable path in the topology and it orchestra the service. It has a routing cahge to optimize the simulation performace. This cache is reset when MARIO performs any action.
- workload.py generates the generation of users in the system.
- userMovement.py controls the random movement of the users.
- problogRulesGenerator.py, it is a simple parser to generate the simulator facts into Prolog facts.

# Rules or policies

```bash
policies folder
├── am.py
├── am_noadapt.py
├── nm.py
├── nm_withProfiles.py
```

- am.py Application manager rules
- am_noadapt.py another application manager rules
- nm.py Node manager rules
- nm_withProfiles.py another node manager rules.

# Scenario definition

A scenario has the following folder structure:
```bash
scenarios

├── Model
│   ├── configuration
│       ├── appDefinition.json
│       ├── allocDefinition.json
│       ├── topology.json
│       └── usersDefinition.pl
│   └── config.ini
├── ...
```
Files:
- AppDefinition.json contains the structure of the application 
- allocDefinition.json, the initial deployment of services in the topology
- topology.json the topology structure: nodes, links, attributes...
- userDefinition.pl the users and related attributes.
 - config.ini has a list of variables regarding with the parametrization of the simulations, mainly event periods
```text
[simulation]
nSimulations = 1
time_in_each_step = 4000
UsersSteps = 10
limitMovements = 4

notAllowedAllocation = true

[nodeManager]
activation_period = 600

[service]
activation_period = 500
message_period = 40
window_agent_outcome = 5
```
notAllowedAllocation parameter is optional and it indicates that node manager block all the operations. We can obtain a static deployment of services.



# main.py

A scenario has other definitions such as topology, routing algorithm, MARIO app and movement manager. All of them are defined in the main.py file.

The experiments in main.py are automated in experiments.json file. The results are generated in a temporal folder.


# Simulation results

In YAFS simulations you obtain two default files: Result_.csv and Results_link.csv. Both files contains the simulation traces.
In this simulation, we added more data gathers in MARIO to get movements, actions, etc. The extra files are:
 
```bash
├── specific_actions.txt
├── movements.csv
├── agg_operations.txt
```

- specific_actions.txt is a CSV file. It contains the number of operations performed by MARIO in each activation where there are service(agent) requests.
```csv 
NM,Service,App,Time,Action,OldLevel,NewLevel,Status
1,2,1,1200,migrate,,large,Accept
0,3,2,1200,adapt,medium,large,Accept
4,4,3,1200,replicate,,medium,Accept
1,3,2,1800,migrate,,large,Accept
```

- movements.txt is a CSV file. It has all the handovers of each user(taxi). DES represents the identifier of the user in the simulation. Thus, you can link this information of the user (DES) with the requests traces in the simulator ( *Results* files). 
```csv 
taxi,DES,time,nodeSRC,nodeDST
2,32,4000,9,10
2,14,8000,6,7
2,40,8000,9,10
```
- agg_operations.txt is a CSV file.  It relates service request (idService) of a app type (app) with its action in a specific time.
```csv 
time,undeploy,nop,migrate,replicate,shrink,evict,reject,adapt,DES,app,nodeManager
1200,0,0,1,0,0,0,0,0,2,1,1
1200,0,0,0,0,0,0,0,1,3,2,0
1200,0,0,0,1,0,0,0,0,4,3,4
```

# Analysing the results

The complexity of analyzing the YAFS results is just as complex as the complexity of the scenario. 
**Keep calm and learn PANDAS**

There is three files to generate information about the results. They are called by main.py:

- plot_actions.py. It generates a bar plot with the number of actions in each MARIO activation. 
<img src="https://github.com/acsicuib/MARIO/raw/SI-SoSs/media/example_actions.png" width="630" height="316"/></a>
- plot_averageActionType.py. It generates stats from different actions taken along the simulation.
- plot_response_time.py. It analyses the response time.
- plot_totalrequests. It analyses the service usage.
- plot_responses and plot_bars are not automatized and they represent some values from previous data.


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

# Video
From the snapshots we can generate an animation with the service and user movements. The video is generated from the main.py but also we can create using these commands:
```
ffmpeg -r 1 -i network_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p video.mp4
ffmpeg -t 20 -i video.mp4 -vf "fps=10,scale=520:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 output.gif
```

<img src="https://github.com/acsicuib/MARIO/raw/SI-SoSs/media/output.gif" width="330" height="266"/></a>



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
git clone --branch SI-SoSs https://github.com/acsicuib/MARIO

#git reset --hard HEAD
#git pull
#git pull origin SI-SoSs

apt-get install python3-venv
apt install pkg-config 
python3 -m pip install virtualenv
python3 -m venv marioenv
source marioenv/bin/activate
 
python -m pip install -r requirements.txt 

#on MARIO/multi-agent-policies/ folder)
export PYTHONPATH="/home/isaac/projects/SIMARIO/MARIO:/home/isaac/projects/SIMARIO/MARIO/multi-agent-policies/environment:$PYTHONPATH"
python3 main.py

# The next lines are for me, sorry - copypaste

rsync -rav -e ssh --include '*.txt' --include '*.pl' --include '*.csv' --include='*.mp4' --include="*.pdf" --exclude='*.*' --exclude='images/*.pdf' isaac@cloudlab:/home/isaac/projects/SIMARIO/MARIO/multi-agent-policies/results/ \ 

On multi-agent-policies isaaclera$ 

rsync -rav -e ssh --include '*.txt' --include '*.csv' --include='*.mp4' --include="*.pdf" --exclude='*.*' --exclude='images/*.pdf' isaac@cloudlab:/home/isaac/projects/SIMARIO/MARIO/multi-agent-policies/results/ results/
rsync -rav -e ssh --include '*.txt' --include="*.pdf" --exclude='*.*' --exclude='images/*.pdf' isaac@cloudlab:/home/isaac/projects/SIMARIO/MARIO/multi-agent-policies/results/ results/


```


