# MARIOII Scholar



## Experiments 
They are defined in a json file in multi-agent-policies/experiment.py
```json
[
  {
    "code": "RomeP2",
    "scenario":"Rome",
    "policy":"policy2.pl",
    "radius": 1,
    "reversepath": 1
  },
  {
    "code": "P2_grid",
    "scenario": "Grid",
    "policy":"policy2.pl",
    "radius": 3,
    "reversepath": 3
  }
]
```

Two configuration types in the experiments:
- Grid. 8x8 
<img src="https://github.com/acsicuib/MARIO/raw/scholar/media/grid.png" width="330" height="266"/></a>
- Tier. 
<img src="https://github.com/acsicuib/MARIO/raw/scholar/media/tier.png" width="330" height="266"/></a>


# Experiment results

Each experiment generate a new folder in multi-agent-policies/ called *results_<code_json>_timestamp*:
In that folder there is:
- images folder. It contains a snapshot of each action.
- models folder. It contains all service Prolog executions 

In addition, in YAFS simulations you obtain two default files: Result_.csv and Results_link.csv. Both files contains the simulation traces.
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

There is three files to generate information about the results:
- plot_actions.py. It generates a bar plot with the number of actions in each MARIO activation. In order to execute this script (and the other two) , you should update the variable that contains the results in the plot_actions.py (line 74) Here you have an example:
```
experiments = [
    ("P1_s3", "Results_%s_20201122w5",)
]
```
<img src="https://github.com/acsicuib/MARIO/raw/MarioII/media/plot_action.png" width="630" height="316"/></a>
- plot_pearsonCoefficient.py. It generates a plot about the relationship between actions and users movements.
<img src="https://github.com/acsicuib/MARIO/raw/MarioII/media/plot_pearson.png" width="630" height="316"/></a>
- plot_averageActionType.py. It generates a text description about the average number of action by each user movement.


# Snapshots
<img src="https://github.com/acsicuib/MARIO/raw/MarioII/media/snap_example.png" width="330" height="266"/></a>

To generate a video from the snapshots:

```bash
ffmpeg -framerate 10 -i multi-agent-policies/results_YOURFOLDER_/images/network_%05d.png -c:v libx264 -pix_fmt yuv420p -crf 23 video.mp4 
```



#  How to run the project

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

Next step, create a virtual environment, 
```
apt-get install python3-venv pkg-config 
python3 -m pip install virtualenv
python3 -m venv marioenv
source marioenv/bin/activate

```

Clone the project and configure Python third libraries (tested on Ubuntu Server 20.0)
```
git clone --branch scholar https://github.com/acsicuib/MARIO
cd MARIO
python -m pip install -r requirements.txt 
```

Run the program 
```
cd multi-agent-policies
export PYTHONPATH="~/MARIO:~/MARIO/multi-agent-policies/environment:$PYTHONPATH"
python3 main.py
```
