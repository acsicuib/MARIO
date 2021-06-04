
#Generate buildings.

import numpy as np

case_M = np.concatenate((np.arange(158,170),np.arange(180,186), #classes and offices
                                  np.arange(124,136), #classes
                                  np.arange(101,106), #library
                                  np.arange(191,193), #subway
                                  np.arange(6,14) #original building
                                  ))

real_M = [6, 7, 8, 9, 10, 11, 12, 13, 191, 192, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 180, 181, 182, 183, 184, 185, 101, 102, 103, 104, 105, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135]


a = set(case_M)
b = set(real_M)
assert((a-b)==set()) # RIGHT: both sets are equal


bM = {"b0":np.arange(6,14),
      "b1":np.arange(101,106),
      "b2":np.arange(124,136), #classes
      "b3":np.arange(124,136), #classes
      "b4":np.concatenate((np.arange(158,170),np.arange(180,186))),
      "b5":np.arange(191,193) #subway
      }


def getNeighbourdNode(node):
    for k in bM.keys():
        if node in bM[k]:
            return bM[k]

        
getNeighbourdNode(102)




reaL_L =  [6, 7, 8, 9, 10, 11, 12, 13, 191, 192, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 180, 181, 182, 183, 184, 185, 101, 102, 103, 104, 105, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 406, 407, 408, 409, 410, 411, 412, 413, 291, 292, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 280, 281, 282, 283, 284, 285, 201, 202, 203, 204, 205, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235]


bL = {"b0":np.arange(6,14),
      "b1":np.arange(101,106),
      "b2":np.arange(124,136), #classes
      "b3":np.arange(124,136), #classes
      "b4":np.concatenate((np.arange(158,170),np.arange(180,186))),
      "b5":np.arange(191,193), #subway

      "b6":np.arange(406,414),
      "b7":np.arange(201,206),
      "b8":np.arange(224,236), #classes
      "b9":np.arange(224,236), #classes
      "b10":np.concatenate((np.arange(258,270),np.arange(280,286))),
      "b11":np.arange(291,293) #subway
}