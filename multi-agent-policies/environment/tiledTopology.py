#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

Tiled Topology generator 

Created on Wed Oct 28 2020

@author: isaaclera
@email: isaac.lera@uib.es
"""

import math
import numpy as np
import networkx as nx


def getAbbrNodeName(node):
    level = node.split("lt")[0].split("n")[1]
    lat = node.split("lt")[1].split("ln")[0]
    lng = node.split("lt")[1].split("ln")[1]
    return "%s-%s-%s" % (level, lat, lng)

def getAbbrNodeNameSnap(node):
    level = node.split("lt")[0].split("n")[1]
    lat = node.split("lt")[1].split("ln")[0]
    lng = node.split("lt")[1].split("ln")[1]
    return "%s(%s,%s)" % (level, lat, lng)

class TiledTopology():
    
    def __init__(self,size,method="pow"):
        self.size = size
        self.method = method #not yet implemented #TODO 
        self.levels = self.__generateAllLevels()        
        
    def getNumberLayers(self):
        return len(self.levels)

    def TiledGraph(self,projection=None):
        edges,levelnode = list(self.__getListEdges())
        G = nx.Graph()
        G.add_edges_from(edges)
        nx.set_node_attributes(G,name="level",values=levelnode)
        if projection != None:
            self.setProjection(G,projection)
        return G
    
    def setProjection(self,G,projection):
        self.projection = projection
        lat,lng,t = self.generateProjection(projection)
        nx.set_node_attributes(G,name="latitude",values=lat)
        nx.set_node_attributes(G,name="longitude",values=lng)

    def setPosPlot(self,G,posprojection):
        x, y,level = self.generateProjection(posprojection)
        pos = {}
        for k in x.keys():
            shify = 0                                   # LIGHT MOVEMENT ON INTERMEDIATE NODES
            if level[k]!=self.size:# and level[k]!=0:   # FOR RENDER THE CANVAS - MATPLOTLIB
                shify = .25                             #
            pos[k]=(y[k],x[k]+shify) #we swap lat/long vars since matplotlib changes the axis orientation

        nx.set_node_attributes(G, name="pos", values=pos)


    def generateAllLevels(self):
        """
        Generate a list with the number of nodes in all tiled zoom layers
        It uses the method based on math.sqrt

        Returns
        -------
        None.

        """
        ls = [self.size]
        v = self.size // 2
        while v >= 2:
            ls.append(v)
            v = v // 2
        ls.append(0)
        return ls

    def namenode(self,level,px,py):
        return "n%ilt%iln%i"%(level,px,py)

    def getLevel(self,namenode):
        return int(namenode.split("l")[0].split("n")[1])



    def getClosedNode(self,point):
        """

        :param point:
        :return:
        The name of most closed node of the last level
        """
        pn = [(point[0] - self.projection[0][0]) / (self.projection[1][0] - self.projection[0][0]),
              (point[1] - self.projection[0][1]) / (self.projection[1][1] - self.projection[0][1])]

        level = self.levels[:-1][0]
        for posX,x in enumerate(np.linspace(0, 1, int(level)+1)):
            if x>pn[0]:
                if posX == 0: posX = 1
                break
        for posY,x in enumerate(np.linspace(0, 1, int(level)+1)):
            if x>pn[1]:
                if posY == 0: posY = 1
                break

        return self.namenode(len(self.levels)-1,posX-1,posY-1) #lastlevel


    def isInHier(self,point):
        """
        Generate a iterable with information about the tile where the point is.

        Parameters
        ----------
        point : Array
            Represent a coordinte point.

        Yields
        ------
        Tuple
            with the next structure (level,cellX,cellY)

        """
        for level in self.levels[:-1]:
            for posX,x in enumerate(np.linspace(0, 1, int(level)+1)):
                if x>point[0]: 
                    break
            for posY,x in enumerate(np.linspace(0, 1, int(level)+1)):
                if x>point[1]: 
                    break
            yield (level,posX-1,posY-1)
        yield (0,0,0)
    
    
    def getListEdges(self):
        """
        Generate the edges of the tiled hierarquical structure

        Returns
        -------
        edges : set
            all the edges of the graph
            
        levelNode : dict
            contains the level of each node

        """
        levels = np.array(self.levels)
        edges = set()
        levelNode = {}
        
        for idx in np.linspace(0,1,levels[0]+1):
            for idy in np.linspace(0,1,levels[0]+1):
               ed = list(self.isInHier([idx,idy]))
               p = 0
               levelC,nodeCX,nodeCY = ed[p]
               levelC = np.where(levelC == levels[::-1])[0][0]
               while p<len(ed)-1:
                   p+=1
                   levelD,nodeDX,nodeDY = ed[p]
                   levelD = np.where(levelD == levels[::-1])[0][0]
                   nameC = self.namenode(levelC,nodeCX,nodeCY)
                   nameD = self.namenode(levelD,nodeDX,nodeDY)
                   
                   levelNode[nameC]=levelC
                   edges.add((nameC,nameD))      
                   
                   levelC,nodeCX,nodeCY = levelD,nodeDX,nodeDY 
     
        return edges,levelNode

        
    def generateProjection(self,projection):
        """

        Parameters
        ----------
        projection : a array of 2d
            A square with min.lat,min.long point, below max.lat,max.long point [[min lat,min longL][max lat,max long]]

        Returns
        -------
        lat : dict
            Contains the latitude value of each node
        lng : dict
            Contains the longitude value of each node.

        """
        lat,lng,lvl = {},{},{}
        minLat,maxLat = projection[0][0],projection[1][0]
        minLng,maxLng = projection[0][1],projection[1][1]
        for il,l in enumerate(self.levels[::-1]):
            level = l+l+1
            for ix,posX in enumerate(np.linspace(minLat,maxLat,level)[1::2]):
                 for iy,posY in enumerate(np.linspace(minLng,maxLng,level)[1::2]):
                       name = self.namenode(il,ix,iy)
                       lat[name]=posX
                       lng[name]=posY
                       lvl[name]=l
    
      
        name = self.namenode(0,0,0)
        lat[name]=minLat+(maxLat-minLat)/2
        lng[name]=minLng+(maxLng-minLng)/2
        lvl[name]=0

        return lat,lng,lvl


    
    # Internal inmutable methods
    __generateAllLevels = generateAllLevels
    __getListEdges = getListEdges
    __generateProjection = generateProjection