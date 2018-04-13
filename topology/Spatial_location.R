# This script contains detailed construction of the initial Australian National Electricity Network.
# This script reads in a file kml.txt which was produced by kmltopd.py that extracts information from a kml file obtained from https://data.gov.au/dataset/electricity-transmission-lines.
# Created by F.R. Willsmore 10/4/18

# Import packages

library(tidyverse)
library(stringr)
library(igraph)
library(ggplot2)
library(ggraph)
library(intergraph)
library(network)
library(plyr)

components <- igraph::components

# Read in kml.txt which contains information from Google Earth kml file.

test <- readLines('~/Documents/GitHub/Transmission-Systems/topology/kml.txt')
tmp <- matrix(test[12:length(test)],ncol = 15, byrow = TRUE)
sp <- data.frame(tmp)
colnames(sp) <- c('OBJECTID','FEATURETYPE','CLASS','FID','NAME','OPERATIONALSTATUS','CAPACITYKV','STATE','SPATIALCONFIDENCE','REVISED','SHAPE_Length','x1','y1','x2','y2')

# Construct temporary variable of line names

line <- as.character(sp$NAME)

# Split names by the separator ' to ' to obtain bus names

test.ls <- strsplit(line, ' to ')

# Check entries that are of length greater than 2 (separation may not have worked properly)

fix.ls <- sp$NAME[which(lapply(test.ls, length)!=2)]

which(lapply(test.ls, length)!=2)

# Edit entries manually 

line[1677] = "Canning Vale to Canning Vale Tee"
line[1862] = "Duaringa to Baralaba"
sp[nrow(sp)+1,] = sp[1862,]
line[nrow(sp)] = "Baralaba to Blackwater Tee"
line[1876] = "Woden to Civic"
line[1894] = "Arndell Park to Blacktown"
sp[nrow(sp)+1,] = sp[1894,]
line[nrow(sp)] = "Blacktown to Quakers"
line[1897] = "Arndell Park to Blacktown"
sp[nrow(sp)+1,] = sp[1897,]
line[nrow(sp)] = "Blacktown to Rooty Hill"
line[2063] = "Blyth West to Para"
sp[nrow(sp)+1,] = sp[2063,]
line[nrow(sp)] = "Para to Bungama Tee"
line[2223] = "SOTO to TOCE"
line[2224] = "MORA to BROD"
line[2225] = "Bulli Creek to Waggamba"
line[2228] = "Mica Creek B to Duchess Road 1"
line[2229] = "Mica Creek B to Duchess Road 2"
line[2230] = "Gunpowder Mine to Century Mine 1"
line[2231] = "Mica Creek C to Ernesthenry/Chumvale"
line[2232] = "Mica Creek C to Gunpowder"
line[2233] = "BR to ER"
line[2375] = "Copeton to Borthwick"

# Update line information

sp$NAME = line

# Split names by the separator ' to ' to obtain bus names

name.ls <- strsplit(sp$NAME, ' to ')

# Remove those entries that are not length 2 (some lines only had one bus name)

index <- which(lapply(name.ls, length)!=2)
name.ls <- name.ls[-index]
sp <- sp[-index,]

# Create edgelist of lines

el <- matrix(unlist(name.ls), ncol = 2, byrow = TRUE)

# Create network from edglist

net <- graph_from_edgelist(el,directed = FALSE)
net.d <- graph_from_edgelist(el,directed = TRUE) # directed graph used for node-arc incidence

# node-arc incidence matrix

N <- as.matrix(asNetwork(net.d),matrix.type="incidence")

# Edge parameters Capacity and State

E(net)$Capacity = as.numeric(sp$CAPACITYKV)
E(net)$State = as.character(sp$STATE)

## Map coordinates to the buses

# coord <- as.matrix(cbind(el, sp[c('x1','y1','x2','y2')]))
# colnames(coord) <- NULL
# coord <- rbind(coord[,c(1,3,4)], coord[,c(2,5,6)])
# coord <- data.frame(Name = coord[,1], x = as.numeric(coord[,2]), y = as.numeric(coord[,3]))
# 
# V(net)$Long <- rep(0,length(V(net)))
# V(net)$Lat <- rep(0,length(V(net)))
# 
# i <- 1
# for (bus in coord$Name){
#   index <- which(sapply(V(net)$name, function(x) x==bus))
#   V(net)$Long[index] <- coord[i,2]
#   V(net)$Lat[index] <- coord[i,3]
#   i <- i+1
# }
# 
# lo <- layout.norm(as.matrix(cbind(V(net)$Long,V(net)$Lat)),-1, 1, -1, 1)
# 
# plot.igraph(net,layout = lo,vertex.size = 0.5, label.cex = 0.)

# Plot the initial network

net.graph <- ggraph(net,layout = 'kk') + 
  geom_edge_link(aes(color = E(net)$State)) + 
  theme_graph()

plot(net.graph)

ggsave('spatial_net.png', plot = net.graph, 
       path = '~/Documents/GitHub/Transmission-Systems/figures', 
       width = 8, height = 8 )

# extract main component

main.net <- decompose(net)[[1]]

main.net.graph <- ggraph(main.net,layout = 'kk') + 
  geom_edge_link(aes(color = E(main.net)$State)) + 
  theme_graph()

plot(main.net.graph)

ggsave('spatial_net_main.png', plot = main.net.graph, 
       path = '~/Documents/GitHub/Transmission-Systems/figures', 
       width = 8, height = 8 )

#### Connect the network: Region algorithm #### (Not working properly)

source('~/Documents/GitHub/Transmission-Systems/topology/reg.net.top.R')

reg.net <- reg.net.top.E(net,1:8)

reg.net.graph <- ggraph(reg.net, layout = 'kk') + 
  geom_edge_link(aes(color = factor(E(reg.net)$State))) + 
  theme_graph()

plot(reg.net.graph)

ggsave('spatial_net.png', plot = reg.net.graph, 
       path = '~/Documents/GitHub/Transmission-Systems/figures', 
       width = 8, height = 8 )
