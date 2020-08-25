# Description

TODO: describe the project

This is an animated gif of the movement of instances along the simulation. It combines three different policies. Source: /scenarios/FOCLASA2020/policy_getclosers_I_II_III/

<img src="https://github.com/acsicuib/DistributedPolicies/raw/gauss2020/multi-agent-policies/scenarios/FOCLASA2020/policy_getclosers_I_II_III/results/out.gif" width="550" height="350"/></a>

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


# Main simulation entities
TODO items:
- Figure legend and text is created by appOperator.py manually. It should be automatic.



#Prolog Simulation Syntax

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
priority(["nop","suicide","replicate","migrate"]).
```` 

- Action: nop - do nothing
- Action: undeploy(Si) (old name in the code is `suicide(Si)`) - undeploy the service instance
    Note: this implementation keeps the original name of this action (suicide). In future reviews, we rename this action to `undeploy`
- Action: migrate(Si,M) - migrate the service in a specific node
- Action: replicate(Si,[M]) - deploy more service in a specific list of nodes

# Definition of a scenario
- Application
- Network
- Service allocations
- User allocations
- Rules file
