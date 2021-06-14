import numpy as np
import random
random.seed(10)

def parser_user_json(i,kind,lamd, start):
    mes = "M.USER.APP.%i"%kind
    json =  "{\"id_resource\":%i,\"app\":%i,\"message\":\"%s\",\"lambda\":%i,\"start\":%i}"%(i,kind,mes,lamd,start)
    return json

M_ap_nodes = np.concatenate((np.arange(158,170),np.arange(180,186), #classes and offices
                                  np.arange(124,136), #classes
                                  np.arange(101,106), #library
                                  np.arange(191,193), #subway
                                  np.arange(6,14) #original building
                                  ))

M_ap_nodes = [6, 7, 8, 9, 10, 11, 12, 13, 191, 192, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 180, 181, 182, 183, 184, 185, 101, 102, 103, 104, 105, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135]
print("Total number of AP nodes",len(M_ap_nodes))


users_students_type_2 = 200
users_professors_type_1 = 50
users_iot_type_3 = 20

# users_students_type_2 = 50
# users_professors_type_1 = 25
# users_iot_type_3 = 10

f = open("users_Definition.json","w")
f.write("{\"sources\":[\n")

for i in range(users_students_type_2):
    lamb = 200
    start = random.randint(10, 120)
    kind = 2
    id_res = np.random.choice(M_ap_nodes,1)[0]
    j = parser_user_json(id_res,kind,lamb,start)
    f.write(j+",\n")    

for i in range(users_iot_type_3):
    lamb = 200
    start = random.randint(10, 120)
    kind = 3
    id_res = np.random.choice(M_ap_nodes,1)[0]
    j = parser_user_json(id_res,kind,lamb,start)
    f.write(j+",\n")  
    

for i in range(users_professors_type_1):
    lamb = 200
    start = random.randint(10, 120)
    kind = 1
    id_res = np.random.choice(M_ap_nodes,1)[0]
    j = parser_user_json(id_res,kind,lamb,start)
    if i==users_professors_type_1-1:
        f.write(j+"\n")    
    else:
        f.write(j+",\n")    






f.write("]}")
f.close()