#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 10 12:07:20 2018

@author: ferguswillsmore
"""

Variables = ['OBJECTID','FEATURETYPE','CLASS','FID','NAME','OPERATIONALSTATUS','CAPACITYKV','STATE','SPATIALCONFIDENCE','REVISED','SHAPE_Length']

lines = []                  
with open ('kml/doc.kml', 'rt') as in_file:  
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
    i = i+1

sp = list(filter(None, sp))       

thefile = open('kml.txt', 'w')
for item in sp:
  thefile.write("%s\n" % item)