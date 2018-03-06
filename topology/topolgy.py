#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar  6 16:16:06 2018

@author: ferguswillsmore
"""

import pandas as pd
import numpy as np
import re

df = pd.read_csv('~/Documents/GitHub/Transmission-Systems/topology/PUBLIC_NOSDAILY_2018030515000037.csv', skiprows = 1)

line_df = df['Equipment Name']

string = 'This string 22 is not yet perfect1234 and 123pretty but it can be.'

string = re.sub(r'\d+', '', string)