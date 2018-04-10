

# Import packages

library(tidyverse)
library(stringr)
library(igraph)
library(ggplot2)
library(ggraph)
library(rgdal)
library(stplanr)
components <- igraph::components

# Import google earth file

test <- readLines('~/Documents/GitHub/Transmission-Systems/topology/kml.txt')
tmp <- matrix(test[12:length(test)],ncol = 11, byrow = TRUE)
sp <- data.frame(tmp)
colnames(sp) <- c('OBJECTID','FEATURETYPE','CLASS','FID','NAME','OPERATIONALSTATUS','CAPACITYKV','STATE','SPATIALCONFIDENCE','REVISED','SHAPE_Length')

line <- as.character(sp$NAME)

# # Read in data from spatial location .csv file 
# 
# line.df <- read.csv('~/Documents/GitHub/Transmission-Systems/topology/ElectricityTransmissionLines_v2.csv')

# line <- as.character(line.df$Name)

# Split names by the separator ' to ' to obtain bus names

test.ls <- strsplit(line, ' to ')

# Check entries that are of length greater than 2

fix.ls <- line.df$Name[which(lapply(test.ls, length)!=2)]

which(lapply(test.ls, length)!=2)

line[1677] = "Canning Vale to Canning Vale Tee"
line[1862] = "Duaringa to Baralaba"
sp[nrow(sp)+1,] = sp[1862,]
line[1862] = "Baralaba to Blackwater Tee"
line[1876] = "Woden to Civic"
line[1894] = "Arndell Park to Blacktown"
sp[nrow(sp)+2,] = sp[1894,]
line[1894] = "Blacktown to Quakers"
line[1897] = "Arndell Park to Blacktown"
sp[nrow(sp)+3,] = sp[1897,]
line[1897] = "Blacktown to Rooty Hill"
line[2063] = "Blyth West to Para"
sp[nrow(sp)+4,] = sp[2063,]
line[2063] = "Para to Bungama Tee"
line[2223] = "SOTO to TOCE"
line[2224] = "MORA to BROD"
line[2225] = "Bulli Creek to Waggamba"
line[2228] = "Mica Creek B to Duchess Road 1"
line[2229] = "Mica Creek B to Duchess Road 2"
line[2230] = "Gunpowder Mine to Century Mine 1"
line[2231] = "Mica Creek C to Ernesthenry/Chumvale"
line[2232] = "Mica Creek C to Gunpowder"
line[2233] = "BR to ER"
line[2275] = "Copeton to Borthwick"

# Update line information

sp$NAME = line

# Split names by the separator ' to ' to obtain bus names

name.ls <- strsplit(sp$NAME, ' to ')

el <- matrix(unlist(name.ls), ncol = 2, byrow = TRUE)

# Create network from edglist

net <- graph.data.frame(el, directed = FALSE)

net <- graph_from_edgelist(el,directed = F)

# Plot the initial network

net.graph <- ggraph(net,layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(col = "indianred1", size = 0.2) +
  theme_graph()

plot(net.graph)

#### Connect the network: Random algorithm ####

set.seed(560)

source('~/Documents/GitHub/Transmission-Systems/topology/rand.net.top.R')

rand.net <- rand.net.top(net)

rand.net.graph <- ggraph(rand.net, layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(col = "indianred1", size = 0.5) +
  theme_graph()

plot(rand.net.graph)

ggsave('spatial_net.png', plot = rand.net.graph, 
       path = '~/Documents/GitHub/Transmission-Systems/figures', 
       width = 8, height = 8 )
