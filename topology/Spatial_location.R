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
sp$NAME <- as.character(sp$NAME)

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

#### Edit line connections ####

# This highlights Bus names that are in different states that are being treated as one

pairs <- combn(levels(sp$STATE),2,simplify = FALSE)
for (i in pairs){
  i_el = el[which(sp$STATE == i[1]),]
  j_el = el[which(sp$STATE == i[2]),]
  I <- intersect(i_el,j_el)
  if (length(I)>0){
    print(i)
    print(I)
  }
}

### Do stuff with Tee ###
library(geosphere)

index <- which(el == 'Tee') %% nrow(el)
tmp <- sp[index,]
tmp[tmp$SPATIALCONFIDENCE=="5",]

# set epsilon such that the coordinates are within 110 m of each other
eps <- 0.001

# Need to remove Tee as this is a connection type and not a bus

index <- which(el == 'Tee') %% nrow(el)
el <- el[-index,] 
sp <- sp[-index,]

# Some bus names occur in different states we fix by adding a unique state indicator
# T - Tasmania, Q - Queensland, etc.

names <- c('Guildford','Meadowbank','Palmerston', 'Belmont', 'Mount Barker', 'Victoria Park')

for (name in names){
  tmp <- sp[grepl(name,sp$NAME),]
  tmp$NAME <- sapply(1:nrow(tmp), function(x) sub(name, paste(name,gsub("[^::A-Z::]","", tmp$STATE[x])), tmp$NAME[x]))
  sp[row.names(tmp),] <- tmp
}

## Other line connection issues ##

# The Basslink connection between Tasmania and Victoria was not connected
sp[grepl('Basslink',sp$NAME),]
sp$NAME[grepl('Loy Yang Power Station to Basslink',sp$NAME)] = 'Loy Yang Power Station to Basslink-Loy Yang'
sp$NAME[grepl('Basslink George Town to George Town',sp$NAME)] = 'Basslink-George Town to George Town'

# It should be consistently Le Fevre
tmp <- sp[grepl('LeFevre',sp$NAME),]
tmp$NAME <- sapply(1:nrow(tmp), function(x) gsub('LeFevre','Le Fevre',tmp$NAME[x]))
sp[row.names(tmp),] <- tmp

# loops in the network
index <- which(el[,1]==el[,2])
el <- el[-index,]
sp <- sp[-index,]

# Torrens island changes into Torrens island A and Torrens island B. We need to include this in the graph

#### Line editing for connectedness ####

# The use of the capacity in the name is not consistent

# Update names of middle ridge to have capacity
name = 'Middle Ridge'
tmp <- sp[grepl(name,sp$NAME),]
index <- which(grepl('\\d',tmp$NAME))
tmp = tmp[-index,]
tmp$NAME <- sapply(1:nrow(tmp), function(x) sub(name, paste(name,tmp$CAPACITYKV[x]), tmp$NAME[x]))
sp[row.names(tmp),] = tmp

# Update names of bannaby to have capacity
name = 'Bannaby'
tmp <- sp[grepl(name,sp$NAME),]
index <- which(grepl('\\d',tmp$NAME))
tmp = tmp[-index,]
tmp$NAME <- sapply(1:nrow(tmp), function(x) sub(name, paste(name,tmp$CAPACITYKV[x]), tmp$NAME[x]))
sp[row.names(tmp),] = tmp

# Lines that are not connected

# Mica Creek power station and lines are not connected to the grid
index <- grepl('Mica Creek',sp$NAME)
sp <- sp[-index]

# Power plant in condabri is closed
index <- grepl('Condabri',sp$NAME)
sp <- sp[-index]

# Capital wind farms should be Capital wind farm
sp$NAME[grepl('Capital Wind Farms',sp$NAME)][1:2] <- "Capital Wind Farm to Capital Wind Farm Substation"


### Missing Lines

sp[grepl('Torrens',sp$NAME),]

# Missing line Dumaresq to Armidale 330kv

# Split names by the separator ' to ' to obtain bus names

name.ls <- strsplit(sp$NAME, ' to ')

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

coord <- as.matrix(cbind(el, sp[c('x1','y1','x2','y2')]))
colnames(coord) <- NULL
coord <- rbind(coord[,c(1,3,4)], coord[,c(2,5,6)])
coord <- data.frame(Name = coord[,1], x = as.numeric(coord[,2]), y = as.numeric(coord[,3]))

V(net)$Long <- rep(0,length(V(net)))
V(net)$Lat <- rep(0,length(V(net)))

i <- 1
for (bus in coord$Name){
  index <- which(sapply(V(net)$name, function(x) x==bus))
  V(net)$Long[index] <- coord[i,2]
  V(net)$Lat[index] <- coord[i,3]
  i <- i+1
}

lo <- layout.norm(as.matrix(cbind(V(net)$Long,V(net)$Lat)),-1, 1, -1, 1)

plot.igraph(net,layout = lo,vertex.size = 0.5, vertex.label = NA)

# Plot the initial network

net.graph <- ggraph(net,layout = 'kk') + 
  geom_edge_link(aes(color = E(net)$State)) + 
  theme_graph()

plot(net.graph)

ggsave('spatial_net.png', plot = net.graph, 
       path = '~/Documents/GitHub/Transmission-Systems/figures', 
       width = 8, height = 8 )

# Remove WA and NT from the network

g <- subgraph.edges(net, E(net)[State!="Northern Territory"])
g <- subgraph.edges(g, E(g)[State!="Western Australia"])

g.graph <- ggraph(g,layout = 'kk') + 
  geom_edge_link(aes(color = E(g)$State)) + 
  theme_graph()

plot(g.graph)

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
