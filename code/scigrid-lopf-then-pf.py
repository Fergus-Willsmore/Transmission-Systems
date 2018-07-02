## LOPF then non-linear power flow with SciGRID
#
#This Jupyter Notebook is also available to download at: <http://www.pypsa.org/examples/scigrid-lopf-then-pf.ipynb> and can be viewed as an HTML page at: <http://pypsa.org/examples/scigrid-lopf-then-pf.html>.
#
#In this example, the dispatch of generators is optimised using the linear OPF, then a non-linear power flow is run on the resulting dispatch.
#
#The data files for this example are in the examples folder of the github repository: <https://github.com/PyPSA/PyPSA>.
#
### Data sources
#
#The data is generated in a separate notebook at <http://www.pypsa.org/examples/add_load_gen_trafos_to_scigrid.ipynb>.
#
#
#Grid: based on [SciGRID](http://scigrid.de/) Version 0.2 which is based on [OpenStreetMap](http://www.openstreetmap.org/).
#
#Load size and location: based on Landkreise (NUTS 3) GDP and population.
#
#Load time series: from ENTSO-E hourly data, scaled up uniformly by factor 1.12 (a simplification of the methodology in Schumacher, Hirth (2015)).
#
#Conventional power plant capacities and locations: BNetzA list.
#
#Wind and solar capacities and locations: EEG Stammdaten, based on  http://www.energymap.info/download.html, which represents capacities at the end of 2014. Units without PLZ are removed.
#
#Wind and solar time series: REatlas, Andresen et al, "Validation of Danish wind time series from a new global renewable energy atlas for energy system analysis," Energy 93 (2015) 1074 - 1088.
#
#NB:
#
#All times in the dataset are UTC.
#
#Where SciGRID nodes have been split into 220kV and 380kV substations, all load and generation is attached to the 220kV substation.
#
### Warning
#
#This dataset is ONLY intended to demonstrate the capabilities of PyPSA and is NOT (yet) accurate enough to be used for research purposes.
#
#Known problems include:
#
#i) Rough approximations have been made for missing grid data, e.g. 220kV-380kV transformers and connections between close sub-stations missing from OSM.
#
#ii) There appears to be some unexpected congestion in parts of the network, which may mean for example that the load attachment method (by Voronoi cell overlap with Landkreise) isn't working, particularly in regions with a high density of substations.
#
#iii) Attaching power plants to the nearest high voltage substation may not reflect reality.
#
#iv) There is no proper n-1 security in the calculations - this can either be simulated with a blanket e.g. 70% reduction in thermal limits (as done here) or a proper security constrained OPF (see e.g.  <http://www.pypsa.org/examples/scigrid-sclopf.ipynb>).
#
#v) The borders and neighbouring countries are not represented.
#
#vi) Hydroelectric power stations are not modelled accurately.
#
#viii) The marginal costs are illustrative, not accurate.
#
#ix) Only the first day of 2011 is in the github dataset, which is not representative. The full year of 2011 can be downloaded at <http://www.pypsa.org/examples/scigrid-with-load-gen-trafos-2011.zip>.
#
#x) The ENTSO-E total load for Germany may not be scaled correctly; it is scaled up uniformly by factor 1.12 (a simplification of the methodology in Schumacher, Hirth (2015), which suggests monthly factors).
#
#xi) Biomass from the EEG Stammdaten are not read in at the moment.
#
#xii) Power plant start up costs, ramping limits/costs, minimum loading rates are not considered.

#make the code as Python 3 compatible as possible
from __future__ import print_function, division, absolute_import

import pypsa

import numpy as np

import pandas as pd

import scipy

import os

import matplotlib.pyplot as plt

from pyutilib.services import register_executable, registered_executable

### Preamble for wdir

os.environ['PATH'] = os.pathsep.join((os.environ['PATH'], '/usr/local/bin'))
register_executable( name='cbc')
#csv_folder_name = "/Users/Fergus/Downloads/pypsa-master/examples/scigrid-de/scigrid-with-load-gen-trafos"
csv_folder_name = "/Users/ferguswillsmore/Downloads/pypsa-master/examples/scigrid-de/scigrid-with-load-gen-trafos"

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
    
    

### Run Linear Optimal Power Flow on the first day of 2011

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

#network.lines.s_nom_extendable = True
#network.lines.s_nom_min = network.lines.s_nom

#Assume 450 EUR/MVA/km
#network.lines.capital_cost = 450*network.lines.length

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

now = network.snapshots[4]

print("With the linear load flow, there is the following per unit loading:")
loading = network.lines_t.p0.loc[now]/network.lines.s_nom
print(loading.describe())

fig,ax = plt.subplots(1,1)
fig.set_size_inches(6,6)

network.plot(ax=ax,line_colors=abs(loading),line_cmap=plt.cm.jet,title="Line loading")

fig.tight_layout()
#fig.savefig("line-loading.png")

## DC Model

B = scipy.sparse.diags(network.lines.b_pu)
W = network.lines.s_nom
C = network.incidence_matrix()

