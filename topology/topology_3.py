#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 15 15:05:37 2018

@author: ferguswillsmore
"""

# Import packages

import pandas as pd
import numpy as np
import re

# Read and prepare data for string manipulation

df = pd.read_csv('~/Documents/GitHub/Transmission-Systems/topology/PUBLIC_NOSDAILY_2018030515000037 copy.csv', skiprows = 1)

# Retrive entries of equipment type = line

line_df = df.loc[df['Equipment Type'] == 'line']
line_df = line_df[['Region','Station Name','Equipment Name','KV']]
line_df = line_df.drop_duplicates()
#line_df = line_df.str.lower()

#%% 

# Initial sweep through strings to remove clutter from Station Names and Equipment Names

for i in line_df.index:
    stmp = line_df.at[i,'Station Name']
    stmp = re.sub(r'\w*\d\w*', '', stmp).strip()
    stmp = re.sub('[^A-z0-9 -]', '', stmp).replace(" ", " ")
    stmp = stmp.strip()
    line_df.at[i,'Station Name'] = stmp
    etmp = line_df.at[i,'Equipment Name']
    etmp = re.sub(r'\d\w*', '', etmp).strip()
    # search for the word line
    ind = re.search('LINE',etmp)
    # remove the word line
    etmp = etmp[0:ind.start()]
    # remove extra whitespace and special characters
    etmp = etmp.strip()
    # remove special characters
    etmp = etmp.replace('/','')
    etmp = etmp.replace('.','')
    etmp = ' '.join(etmp.split())
    # spaces in completed names become underscores
    if etmp.count('-')==1:
        etmp = etmp.replace(' ','_')
    # two worderd strings become separated
    if etmp.count(' ') == 1:
        etmp = etmp.replace(' ','-')
    line_df.at[i,'Equipment Name'] = etmp

#%%

# Use strings of capital letters to identify where to separate the two Station Names with the Equipment Name.
    
for i in line_df.index:
    stmp = line_df.at[i,'Station Name']
    caps = ''.join([c for c in stmp if c.isupper()]) # get Upper case letters of Station name
    etmp = line_df.at[i,'Equipment Name']
    space_ind = np.array([i for i, ltr in enumerate(etmp) if ltr == ' ']) # get indices of spaces within the string
    caps_ind = [i for i, ltr in enumerate(etmp) if ltr == caps[-1]] # get index of last caps in station name in the equipment name
    if len(space_ind) == 0:
        a = 0 # do nothing
    else:
        if len(caps_ind) == 0:
            caps_ind = 0
            hind = space_ind[caps_ind<space_ind]
        else:
            hind = space_ind[caps_ind[0]<space_ind]
        etmp = etmp[:hind[0]] + '-' + etmp[(hind[0]+1):]
    etmp = etmp.replace('_',' ')
    line_df.at[i,'Equipment Name'] = etmp


np.savetxt('top_test.txt', line_df, fmt='%s', delimiter="\t", header="Region\tStation\tEquipment\tKV")
    