from problog.program import PrologString
from problog.core import ProbLog
from problog import get_evaluatable
import re

def sort_results_rules(result):
    """
    test file: test_proc_rules.py

    :param result:
    :return:
    """
    pattern_priorities = r"priority\(\[(.*?)\]"
    pattern_probabilites = r"(:\ \d.\d,*)"
    numberRules = 5

    matches = list(re.finditer(pattern_priorities, str(result)))
    assert len(matches) > 0, "Agent rules without rule priority"
    action_priorities = matches[0].group(1).replace(" ", "").split(",")

    matches = re.finditer(pattern_probabilites, str(result))
    index = 1
    best_probability = -1.0
    order_actions = [None] * numberRules
    for matchNum, match in enumerate(matches):
        for groupNum in range(0, len(match.groups())):
            groupNum = groupNum + 1
            start, end = match.start(groupNum), match.end(groupNum)
            probability = float(match.group(groupNum).replace(": ", "").replace(",", ""))
            action = str(result)[index:start]
            name_action: str = action[0:action.index("(")]
            if name_action != "priority":  # ignore priority rule
                if probability > best_probability:
                    order_actions = [None] * numberRules
                    idx = [i for i, name in enumerate(action_priorities) if name == name_action]
                    order_actions[idx[0]] = action
                    best_probability = probability
                elif probability == best_probability:
                    idx = [i for i, name in enumerate(action_priorities) if name == name_action]
                    order_actions[idx[0]] = action

            # print(name_action)
            index = end + 1
    return order_actions


"""
************************
VARIABLES 
"""
mario_step = 2
service = 9
node = 2
agent_step = 1
"""
************************
"""

experiment_path = "scenarios/prototype1/"
model_path = "results/models/"
file_model ="rules_step%i_%i_n%i_%i.pl"%(mario_step,service,node,agent_step)
pathfile =experiment_path+model_path+file_model
model_text =""
with open(pathfile, "r") as f:
    model_text = f.read()

print(model_text)
model = PrologString(model_text)
result = get_evaluatable().create_from(model).evaluate()
print("Model raw result:")
print(result)
best_actions = sort_results_rules(result)
print("Actions with highest probability, ordered by agent model")
for i,action in enumerate(best_actions):
    print("#%i - %s"%(i,action))

