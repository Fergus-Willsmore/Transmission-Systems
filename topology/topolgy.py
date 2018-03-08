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

# Read and prepare data for string manipulation

df = pd.read_csv('~/Documents/GitHub/Transmission-Systems/topology/PUBLIC_NOSDAILY_2018030515000037 copy.csv', skiprows = 1)

# Retrive only equipment of type line

line_df = df.loc[df['Equipment Type'] == 'line']
line_df = line_df['Equipment Name'].astype('str')
line_df = line_df.str.lower() 

# Create list of equipment names and remove duplicates

line = remove_duplicates(line_df)

# strip everything from the first number (in a couple of cases this is wrong)

x = []
for w in line:
    # remove 330kv from string
    temp = re.sub(r'\d\w*', '', w).strip()
    # search for the word line
    ind = re.search('line',temp)
    # remove the word line
    temp = temp[0:ind.start()]
    # remove extra whitespace and special characters
    temp = temp.strip()
    temp = temp.replace('-',' ')
    temp = temp.replace('/','')
    x.append(temp)

# create a bus database (not neccessarily complete)
    
bus = df['Station Name']
bus = bus.astype('str')
bus = bus.str.lower() 

# Remove voltage,units, special characters whilst retaining spaces from station name

y = []
i = 1
for w in bus:
    temp = re.sub(r'\w*\d\w*', '', w).strip()
    temp = re.sub('[^A-z0-9 -]', '', temp).lower().replace(" ", " ")
    temp = temp.strip()
    y.append(temp)
stations = set(y)

