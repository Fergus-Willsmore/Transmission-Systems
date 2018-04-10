#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr  5 10:56:44 2018

@author: ferguswillsmore
"""

# Import packages

import csv
import pandas as pd
import math

line_df = pd.read_csv('~/Documents/GitHub/Transmission-Systems/topology/ElectricityTransmissionLines_v2.csv')
line_df = line_df['Name']

rem = [1301, 1362, 1367, 1368, 1506, 1677, 1862, 1876, 1894, 1897, 2063, 2075, 2077, 2079, 2216, 2217, 2218, 2219, 2220, 2222, 2223, 2224, 2225, 2228,
2229, 2230, 2231, 2232, 2233, 2239, 2322,2375]

rem[:] = [x - 1 for x in rem]

line_df = line_df.drop(line_df.index[rem])

name = []
for i in line_df:
    w1, w2 = i.split(' to ')
    name.extend([w1,w2])

name = list(set(name))

#from time import sleep

from geopy.geocoders import Nominatim

geolocator = Nominatim(country_bias="Australia", view_box)

for bus in name:
    location = geolocator.geocode(bus)
    print(location)

