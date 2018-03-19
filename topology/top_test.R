# Create a Connected Network
# Test topology algorithm using line data in 'bus-bus' format
# The line data is from planned outage data in PUBLIC_NOSDAILY_2018030515000037 copy.csv, 
# processed via python script topology.py to obtain 'bus-bus' format.
# Created by F.R. Willsmore 6/2/2018

# Import packages

library(tidyverse)
library(stringr)
library(igraph)
library(ggraph)
library(ggplot2)
library(statnet)

components <- igraph::components

# Read in outage data if needed

outage.df <- read.csv('~/Documents/GitHub/Transmission-Systems/topology/PUBLIC_NOSDAILY_2018030515000037 copy.csv', 
                      header = TRUE, sep = ",", skip = 1)

# Read .txt file containing the line names processed by topology.py

lines <- scan('~/Documents/GitHub/Transmission-Systems/topology/top_test.txt', character(), quote = "")

# Create edge-list for network

name.ls <- strsplit(lines,'-')
el <- matrix(unlist(name.ls), ncol = 2, byrow = TRUE)

# Create network from edglist

net <- graph_from_edgelist(el,directed = F)

# Plot the initial network

net.graph <- ggraph(net) + 
  geom_edge_link() + 
  geom_node_point(col = "indianred1", size = 1) +
  theme_graph()

ggsave('net.pdf', plot = net.graph, 
       path = '~/Documents/GitHub/Transmission-Systems/figures', 
       width = 8, height = 8 )

# plot(net,layout=layout_with_fr, vertex.size=4,
#      vertex.label.dist=0.5, vertex.color="red",vertex.label=NA)

####### Connectivity ##########

set.seed(560)

con.net <- rand.net.top(net)

con.net.graph <- ggraph(con.net) + 
                    geom_edge_link() + 
                    geom_node_point(col = "indianred1", size = 1) +
                    theme_graph()

ggsave('con_net.pdf', plot = con.net.graph, 
       path = '~/Documents/GitHub/Transmission-Systems/figures', 
       width = 8, height = 8 )

# plot(con.net,layout=layout_with_fr, vertex.size=3,
#      vertex.label.dist=0.5, vertex.color="indianred1",vertex.label=NA)







