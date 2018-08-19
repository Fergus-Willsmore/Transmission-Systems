#make the code as Python 3 compatible as possible
from __future__ import print_function, division, absolute_import

import pypsa

import numpy as np

import pandas as pd

import networkx as nx

import re

import scipy

import os

import matplotlib.pyplot as plt

from pyutilib.services import register_executable, registered_executable

### Preamble for wdir

os.environ['PATH'] = os.pathsep.join((os.environ['PATH'], '/usr/local/bin'))
register_executable( name='cbc')

# Laptop
csv_folder_name = "/Users/Fergus/Downloads/pypsa-master/examples/scigrid-de/scigrid-with-load-gen-trafos"
# Desktop
#csv_folder_name = "/Users/ferguswillsmore/Downloads/pypsa-master/examples/scigrid-de/scigrid-with-load-gen-trafos"

network = pypsa.Network(import_name=csv_folder_name)

### Plot the distribution of the load and of generating tech

fig,ax = plt.subplots(1,1)

fig.set_size_inches(6,6)

load_distribution = network.loads_t.p_set.loc[network.snapshots[0]].groupby(network.loads.bus).sum()

network.plot(bus_sizes=0.2*load_distribution,ax=ax,title="Load distribution")

fig.tight_layout()
#fig.savefig('load-distribution.png')

network.generators.groupby("carrier")["p_nom"].sum()

network.storage_units.groupby("carrier")["p_nom"].sum()

techs = ["Gas","Brown Coal","Hard Coal","Wind Offshore","Wind Onshore","Solar"]

n_graphs = len(techs)

n_cols = 3

if n_graphs % n_cols == 0:
    n_rows = n_graphs // n_cols
else:
    n_rows = n_graphs // n_cols + 1

    
fig, axes = plt.subplots(nrows=n_rows, ncols=n_cols)

size = 4

fig.set_size_inches(size*n_cols,size*n_rows)

for i,tech in enumerate(techs):
    i_row = i // n_cols
    i_col = i % n_cols
    
    ax = axes[i_row,i_col]
    
    gens = network.generators[network.generators.carrier == tech]
    
    gen_distribution = gens.groupby("bus").sum()["p_nom"].reindex(network.buses.index,fill_value=0.)
    
    network.plot(ax=ax,bus_sizes=0.2*gen_distribution)
    
    ax.set_title(tech)
    
    

###### Run Linear Optimal Power Flow on the first day of 2011 ######

#to approximate n-1 security and allow room for reactive power flows,
#don't allow any line to be loaded above 70% of their thermal rating

contingency_factor = 0.7

network.lines.s_nom = contingency_factor*network.lines.s_nom

#There are some infeasibilities without small extensions                                                                                 
for line_name in ["316","527","602"]:
    network.lines.loc[line_name,"s_nom"] = 1200


#the lines to extend to resolve infeasibilities can
#be found by
#uncommenting the lines below to allow the network to be extended

#network.lines["s_nom_original"] = network.lines.s_nom
#
#network.lines.s_nom_extendable = True
#network.lines.s_nom_min = network.lines.s_nom

#Assume 450 EUR/MVA/km
network.lines.capital_cost = 450*network.lines.length

group_size = 4

solver_name = "cbc"

print("Performing linear OPF for one day, {} snapshots at a time:".format(group_size))

network.storage_units.state_of_charge_initial = 0.

for i in range(int(24/group_size)):
    #set the initial state of charge based on previous round
    if i>0:
        network.storage_units.state_of_charge_initial = network.storage_units_t.state_of_charge.loc[network.snapshots[group_size*i-1]]
    network.lopf(network.snapshots[group_size*i:group_size*i+group_size],
                 solver_name=solver_name)
    network.lines.s_nom = network.lines.s_nom_opt

#if lines are extended, look at which ones are bigger
#network.lines[["s_nom_original","s_nom"]][abs(network.lines.s_nom - contingency_factor*network.lines.s_nom_original) > 1]

# Set the snapshot time

now = network.snapshots[12]

###### Nesti and Zwart method ######

## DC Model
# Susceptance matrix
B = scipy.sparse.diags(network.lines.b_pu)
# Weights matrix (Line ratings)
W = scipy.sparse.diags(network.lines.s_nom)
# Edge-vertex incidence matrix
el = network.lines[['bus0','bus1']].as_matrix().tolist()
G = nx.MultiGraph(el)
C = nx.incidence_matrix(G).transpose()
# Weighted Laplacian Matrix
L = C.transpose()*B*C

## Power injections
# Load
load_distribution
# generation output at each time
opt_gen = network.generators_t.p

opt_gen_now = opt_gen.loc[now]


###### Stochastic and Deterministic injections

# Partition of nodes
bus = []
for name in opt_gen.loc[now].index:
    bus.append(name.split(' ',1)[0])
opt_gen_now['bus'] = bus


bus_s = []
bus_s.extend(network.generators.index[network.generators['carrier']=='Solar'].tolist())
bus_s.extend(network.generators.index[network.generators['carrier']=='Wind Onshore'].tolist())
bus_s.extend(network.generators.index[network.generators['carrier']=='Wind Offshore'].tolist())

n_s = []
for b in bus_s:
    b = b.replace(' Solar','')
    b = b.replace(' Wind Onshore','')
    b = b.replace(' Wind Offshore','')
    n_s.append(b)

n_s = sorted(list(set(n_s)))
n_d = sorted(list(set(network.buses.index).difference(n_s)))

# nominal power injections

