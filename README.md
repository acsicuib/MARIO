# DistributedPolicies


Installation
------------
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


TODO
----
- Integrate priority rule in JSON: agent.py -> self.order_prot3,
- Include fact: "desiredUser(%s,2)." in JSON
