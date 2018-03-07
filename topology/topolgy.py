#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar  6 16:16:06 2018

@author: ferguswillsmore
"""

# Import packages

import pandas as pd
import numpy as np
import re

# Read and prepare data for string manipulation

df = pd.read_csv('~/Documents/GitHub/Transmission-Systems/topology/PUBLIC_NOSDAILY_2018030515000037 copy.csv', skiprows = 1)

# create a bus database (not neccessarily complete)

bus = df['Station Name']
bus = bus.astype('str')
bus = bus.str.lower()

# Remove voltage,units, special characters whilst retaining spaces from station name

i = 1
for w in bus:
    x[i] = re.sub(r'\w*\d\w*', '', w).strip()
    x[i] = re.sub('[^A-z0-9 -]', '', x[i]).lower().replace(" ", " ")
    x[i] = x[i].strip()
    i=i+1 
stations = list(set(x))

# find buses that the lines go between

line = df['Equipment Name']
index = df['Equipment Type'] == 'line'
line = line[index]
line = line.astype('str')
line = line.str.lower()

i = 1
for w in line:
    y[i] = re.sub(r'\d*kv', '', w).strip()
    i=i+1 
line = sorted(list(set(y)))

 abrevs = {'n':'north','e':'east','s':'south','w':'west','ck':'creek','rd':'road'}
 
 re.sub(r'\b' + '|'.join(d.keys()) + r'\b', lambda m: d[m.group(0)], s)

string = 'south-east'

string = string.split('[0-9]', 1)[0]

if '-' in string:
    string = string.split(' ', 1)[0]

if string in stations:
    a = 1
else: 
    a=0