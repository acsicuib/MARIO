# Description
TODO

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
TODO
- main.py
- environment/agent.py
- environment/app_operator.py


# Prolog Simulation Syntax

## Facts from the simulation
- A service: service(ServiceIdentifier, AppIdentifier, MaxUserRequests, MaxLatency)
````text
service(s1,app1,5,50).
````
- A service instance: serviceInstance(ServiceIdentifier, AppIdentifier, current_deployed_node)
````text
serviceInstance(s1, meteo, n1).
serviceInstance(s2, meteo, n4).
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

- A route represent a communication between the service instance and a set of users : route(serviceIdentifier,path([L]),latency,number of requests)
The path represent the nodes between the service instance and a set of users 
````text
serviceInstance(s1, meteo, n1).
route(s1,path([n1,n2,n3,n4]),10,200).
```` 

## Actions from the simulation
- Priority rule indicates the order of preference in the execution of various possible rules.
````text
priority(["nop","suicide","replicate","migrate"]).
```` 

- nop
- suicide(Si)
- migrate(Si,[M])
- replicate(Si,[M])

# Definition of a scenario
- Application
- Network
- Service allocations
- User allocations
- Rules file