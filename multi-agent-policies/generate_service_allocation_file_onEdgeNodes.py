import numpy as np
import random
random.seed(10)

def parser_service_json(kind,id_node, level):
    module_name = "%i_01"%(kind)

    json =  "{\"module_name\":\"%s\",\"app\":%i,\"level\":\"%s\",\"id_resource\":%i}"%(module_name,kind,level,id_node)
    return json


# M_ap_nodes = np.concatenate((np.arange(158,170),np.arange(180,186), #classes and offices
#                                   np.arange(124,136), #classes
#                                   np.arange(101,106), #library
#                                   np.arange(191,193), #subway
#                                   np.arange(6,14) #original building
#                                   ))

M_ap_nodes = [7, 8, 9, 10, 11, 12, 13, 191, 192, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 180, 181, 182, 183, 184, 185, 101, 102, 103, 104, 105, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135]
print("Total number of AP nodes",len(M_ap_nodes))


f = open("initAllocation.json","w")
f.write("{\"initialAllocation\":[\n")

apps = [1,2,3]
for id_resource in M_ap_nodes:
    for kind in apps:
        level = "small"
        if kind == 3:
            level = "medium"
        j = parser_service_json(kind,id_resource,level)
        if id_resource==M_ap_nodes[-1] and kind==apps[-1]:
            f.write(j+"\n")
        else:
            f.write(j+",\n")

f.write("]}")
f.close()


