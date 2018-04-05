# Create a Connected Network
# Test topology algorithm using line data in 'bus-bus' format
# The line data is from planned outage data in PUBLIC_NOSDAILY_2018030515000037 copy.csv, 
# processed via python script topology.py to obtain 'bus-bus' format.
# Created by F.R. Willsmore 6/2/2018

# Import packages

library(tidyverse)
library(stringr)
library(igraph)
library(ggplot2)
library(ggraph)
library(statnet)

components <- igraph::components

# Read in outage data if needed

outage.df <- read.csv('~/Documents/GitHub/Transmission-Systems/topology/PUBLIC_NOSDAILY_2018030515000037 copy.csv', 
                      header = TRUE, sep = ",", skip = 1)

# Read .txt file containing the line names processed by topology.py

line.df <- read.delim('~/Documents/GitHub/Transmission-Systems/topology/top_test.txt', header = TRUE, sep = '\t', character(), quote = "")

# Create edge-list for network

name.ls <- strsplit(as.character(line.df$Equipment),'-') # separate by the hyphen
name.length <- lapply(name.ls, function(x) length(x)) %>% as.matrix() # find length of each entry
line.df <- line.df[-which(name.length>2),]
name.ls <- name.ls[-which(name.length>2)] # discards entries with more than two entries

el <- matrix(unlist(name.ls), ncol = 2, byrow = TRUE)

# Create network from edglist

net <- graph_from_edgelist(el,directed = F)

# Plot the initial network

net.graph <- ggraph(net,layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(col = "indianred1", size = 1) +
  theme_graph()

plot(net.graph)

ggsave('net.pdf', plot = net.graph, 
       path = '~/Documents/GitHub/Transmission-Systems/figures', 
       width = 8, height = 8 )

#### Connect the network: By Region ####

source('~/Documents/GitHub/Transmission-Systems/topology/reg.net.top.R')

E(net)$Region = as.character(line.df$X..Region)

reg.net <- reg.net.top(net,levels(line.df$X..Region))

reg.net.graph <- ggraph(reg.net,layout = 'kk') + 
  geom_edge_link(aes(color = factor(E(reg.net)$Region))) + 
  theme_graph()

plot(reg.net.graph)

ggsave('reg_net.jpeg', plot = reg.net.graph, 
       path = '~/Documents/GitHub/Transmission-Systems/figures', 
       width = 8, height = 8 )
 
#### Connect the network: Random algorithm ####

set.seed(560)

source('~/Documents/GitHub/Transmission-Systems/topology/rand.net.top.R')

rand.net <- rand.net.top(net)

rand.net.graph <- ggraph(rand.net, layout = 'kk') + 
                    geom_edge_link() + 
                    geom_node_point(col = "indianred1", size = 1) +
                    theme_graph()

plot(rand.net.graph)

ggsave('rand_net.pdf', plot = rand.net.graph, 
       path = '~/Documents/GitHub/Transmission-Systems/figures', 
       width = 8, height = 8 )









