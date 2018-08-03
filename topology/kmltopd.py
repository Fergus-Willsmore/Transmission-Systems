#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 10 12:07:20 2018

@author: ferguswillsmore
"""

import numpy as np
import re

Variables = ['OBJECTID','FEATURETYPE','CLASS','FID','NAME','OPERATIONALSTATUS','CAPACITYKV','STATE','SPATIALCONFIDENCE','REVISED','SHAPE_Length']

lines = []                  
with open ('kml/NEM-geoscience.kml', 'rt') as in_file:  
    for line in in_file:  
        lines.append(line.rstrip('\n'))  

sp = []        
i = 0
for s in lines:
    var = s[s.find(">")+1:s.find("<",2)]
    if var in Variables:
        t = lines[i+1]
        val = t[t.find(">")+1:t.find("<",2)]
        if val == '':
            val = ' '
        sp.append(val)
    if s[s.find("<")+1:s.find(">")] == 'coordinates':
        coord = s[s.find(">")+1:s.find("<",2)]
        coord = re.sub('0.0 ', '', coord)
        coord = np.array([x.strip() for x in coord.split(',')])
        sp.append(coord[0])
        sp.append(coord[1])
        sp.append(coord[len(coord)-3])
        sp.append(coord[len(coord)-2])

    i = i+1

sp = list(filter(None, sp))       

thefile = open('kml.txt', 'w')
for item in sp:
  thefile.write("%s\n" % item)
