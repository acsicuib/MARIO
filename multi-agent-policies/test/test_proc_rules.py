import re

pattern_priorities = r"priority\(\[(.*?)\]"

test_str = "{nop(1): 1.0, migrate(1,X2,1): 0.0, replicate(1,[3, 2]): 1.0, suicide(1): 1.0, fusion(X3,X4): 0.0, priority([nop, migrate, replicate, suicide]): 1.0}"
# test_str = "{nop(1): 1.0, migrate(1,X2,1): 0.0, replicate(1,[3, 2]): 1.0, suicide(1): 1.0, fusion(X3,X4): 0.0"

matches = list(re.finditer(pattern_priorities, test_str))
assert len(matches)>0, "Agent rules without priority"
print(len(matches))
priorities = (matches[0].group(1).replace(" ","").split(","))

# for matchNum, match in enumerate(matches):
#     # for groupNum in range(0, len(match.groups())):
#     print(matchNum)
#     try:
#         print(match.group(1))
#         order = match.group(1).replace(" ","").split(",")
#         print(order)
#     except:
#         assert False, "Agent rules without priority"
#
#


import re

regex = r"(:\ \d.\d,*)"

result = "{nop(1): 1.0, migrate(1,X2,1): 0.0, replicate(1,[3, 2]): 1.0, suicide(1): 1.0, fusion(X3,X4): 0.0, priority([nop, migrate, replicate, suicide]): 1.0}"

matches = re.finditer(regex, result)
index = 1
best_probability = -1.0
order_actions = [None]*5
for matchNum, match in enumerate(matches):
    for groupNum in range(0, len(match.groups())):
        groupNum = groupNum + 1
        start, end = match.start(groupNum),match.end(groupNum)
        probability = float(match.group(groupNum).replace(": ","").replace(",",""))
        print(probability)
        action = result[index:start]
        print(action)
        name_action: str = action[0:action.index("(")]
        if name_action != "priority":
            if probability > best_probability:
                order_actions = [None] * 5
                idx = [i for i,name in enumerate(priorities) if name == name_action]
                order_actions[idx[0]] = action
                best_probability = probability
            elif probability == best_probability:
                idx = [i for i, name in enumerate(priorities) if name == name_action]
                order_actions[idx[0]] = action

        print(name_action)
        index = end+1

print("BEST PROB:%f"%best_probability)
print("best_actions :%s"%order_actions)
        # print("Group {groupNum} found at {start}-{end}: {group}".format(groupNum=groupNum, start=match.start(groupNum),
        #                                                                 end=match.end(groupNum),
        #                                                                 group=match.group(groupNum)))

# Note: for Python 2.7 compatibility, use ur"" to prefix the regex and u"" to prefix the test string and substitution.
