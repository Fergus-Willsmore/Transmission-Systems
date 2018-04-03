#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar 14 09:58:09 2018

@author: ferguswillsmore
"""

# Import packages

import pandas as pd
import numpy as np
import re

# Define remove duplicates function

def remove_duplicates(values):
    output = []
    seen = set()
    for value in values:
        # If value has not been encountered yet,
        # ... add it to both list and set.
        if value not in seen:
            output.append(value)
            seen.add(value)
    return output

# Preallocate the final line list
    
line_final = []

# Read and prepare data for string manipulation

df = pd.read_csv('~/Documents/GitHub/Transmission-Systems/topology/PUBLIC_NOSDAILY_2018030515000037 copy.csv', skiprows = 1)

# Retrive only equipment of type line

line_df = df.loc[df['Equipment Type'] == 'line']
line_df = line_df['Equipment Name'].astype('str')
line_df = line_df.str.lower() 

#%% Create a Bus Database

# create a bus database (not neccessarily complete)
    
bus = df['Station Name']
bus = bus.astype('str')
bus = bus.str.lower() 

# Remove voltage,units, special characters whilst retaining spaces from station name

y = []
for w in bus:
    temp = re.sub(r'\w*\d\w*', '', w).strip()
    temp = re.sub('[^A-z0-9 -]', '', temp).lower().replace(" ", " ")
    temp = temp.strip()
    y.append(temp)
stations = set(y)
bus = np.asarray(stations)

#%% String Manipulation

# Create list of equipment names and remove duplicates

line = remove_duplicates(line_df)

# remove voltage capacities and line numbers and general tidying

line_tmp_1 = []

for w in line:
    # remove 330kv from string
    temp = re.sub(r'\d\w*', '', w).strip()
    # search for the word line
    ind = re.search('line',temp)
    # remove the word line
    temp = temp[0:ind.start()]
    # remove extra whitespace and special characters
    temp = temp.strip()
    # remove special characters
    temp = temp.replace('/','')
    temp = temp.replace('.','')
    temp = ' '.join(temp.split())
    line_tmp_1.append(temp)

# Complete names with '-' go to line_final otherwise '-' goes to '_'    

line_tmp_2 = []

for w in line_tmp_1:
    if w.count('-')==1:
        w = w.replace(' ','_')
        line_final.append(w)
    else:
        line_tmp_2.append(w)
        
# Two word string goes to line_final with '-' otherwise 
        
line_tmp_3 = []
# Delete a 3 link transmission line
del line_tmp_2[line_tmp_2.index('south_east-mayura-snuggery')]

for w in line_tmp_2:
    w = w.replace('-','_')
    ind = re.search(' ',w)
    ind = ind.start()
    if ind < 4:
        w = w[:ind]+'_'+w[(ind+1):]
    if w.count(' ') == 1:
        w = w.replace(' ','-')
        w = w.replace(' ','_')
        line_final.append(w)
    else:
        line_tmp_3.append(w)
#%% Do a pattern match search to identify buses from the database



        
        
#%% Write line_final to .txt file
      
f = open('top_test.txt','w')
f.write("\n".join(line_final))
        
        