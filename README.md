# Description
This repository contains several incremental and novelty version os MARIO. MARIO is a new fully decentralised and declarative approach for Managing Applications Running In Opportunistic Fog scenarios. 

The proposed management solution, MARIO, has been prototyped and open-sourced by means of the Prolog logic programming language.

Based on the assumption that nodes can monitor contextually available resources and resources along request routes towards each application instance, MARIO moves a step towards the self-adaptive management of next-gen applications in the Fog. A basic set of simple operations (viz. migrate, replicate, undeploy) is identified and proposed to manage applications according to customised policies, declared as Prolog rules. 
 
The MARIO prototype is assessed over a lifelike example by leveraging the YAFS simulation environment and by showcasing its functioning with three management policies aimed at keeping deployed applications suitably close to their end-users.

# Branches and Projects

The master branch is only a default structure of this whole project. Different versions are in the branches of this repo, so the versions of this project can be replicated.

## MARIO in GAUSS2020

[BRANCH](https://github.com/acsicuib/MARIO/tree/gauss2020).
 
This version is published in the 2nd International Workshop on Governing Adaptive and Unplanned Systems of Systems [GAUSS2020](http://gauss2020.disim.univaq.it/)  Co-located with the 31st International Symposium on Software Reliability Engineering (ISSRE 2020).

```latex
@inproceedings{MARIOonGAUSS,
  title={Towards Declarative Decentralised Application Management in the Fog},
  author={Antonio Brogi and Stefano Forti and Carlos Guerrero and Isaac Lera},
  booktitle={Proceedings at 2020 IEEE International Symposium on Software Reliability Engineering Workshops (ISSREW) },
  pages={223--230},
  year={2020},
  address = {Coimbra, Portugal},
  doi = {10.1109/ISSREW51248.2020.00077}
}
```


# Installation Steps
0. Requires: Python 3.6

1. Clone the project in your local folder:

```bash
    $ git clone https://github.com/acsicuib/DistributedPolicies

```

2. Create a Python virtual environment (you can follow this tutorial: https://uoa-eresearch.github.io/eresearch-cookbook/recipe/2014/11/26/python-virtual-env/)

```bash
   (your environment)$  pip install -r requirements.txt
```
3. Install and Configure SWI-Prolog
- https://www.swi-prolog.org/
- https://github.com/yuce/pyswip/blob/master/INSTALL.md


4. Update your python path according with your OS and run: 
```bash
cd multi-agent-policies
PYTHONPATH=~/PycharmProjects/DistributedPolicies/
export PYTHONPATH=$PYTHONPATH/multi-agent-policies/environment:$PYTHONPATH
python main.py
```

# Prolog Simulation Syntax

The policy rules and the facts are generated and performed  by each service instance controller (agent.py)
Finally, MARIO (our app_operator.py) periodically performs each action in order from all the service instance controller.


## Facts from the simulation

The facts come from a simulation window. A period of time that includes the previous execution and the current moment. Some facts may not exist at any given time like routes.

- A service instance: serviceInstance(ServiceIdentifier, AppIdentifier, current_deployed_node)
````text
serviceInstance(s1, meteo, n1).
serviceInstance(s2, meteo, n4).
````

- A service: service(ServiceIdentifier, RequiredHW, MaxUserMessages, MaxLatency)
````text
service(s1,10,5,50).
````

- A node: node(Identifier,HwCapacity,Neighbors)
````text
node(n1, 6, [n2,n4]).
node(n2, 1, [n1,n3]).
```` 

- A link: link(src node, dst node, latency, bandwidth)
````text
link(n1, n2, 10, 300).
```` 

- A route represent a communication between the service instance and a set of users : route(serviceIdentifier, path([L]), latency, number of messages)
The path represents the nodes between the service instance and a set of users 
````text
serviceInstance(s1, meteo, n1).
route(s1,path([n1,n2,n3,n4]),10,200).
```` 

## Actions from the simulation

The environment modeller has to define the rules in a specific file inside the scenario folder. These rules controls the service instance actions.

- Priority rule indicates the order of preference in the execution of various possible rules.
````text
priority(["nop","undeploy","replicate","migrate"]).
```` 

- Action: nop - do nothing
- Action: undeploy(Si) - undeploy the service instance
- Action: migrate(Si,M) - migrate the service in a specific node
- Action: replicate(Si,[M]) - deploy more service in a specific list of nodes

## How it works the simulation:
Take a look to [YAFS](https://github.com/acsicuib/YAFS) simulator to undestand how it works.
