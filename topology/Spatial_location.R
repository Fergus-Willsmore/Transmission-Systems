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
library(geosphere)

components <- igraph::components

#### Read Data ####

# Read in kml.txt which contains information from Google Earth kml file.

test <- readLines('~/Documents/GitHub/Transmission-Systems/topology/kml.txt')
tmp <- matrix(test[12:length(test)],ncol = 15, byrow = TRUE)
sp <- data.frame(tmp)
colnames(sp) <- c('OBJECTID','FEATURETYPE','CLASS','FID','NAME','OPERATIONALSTATUS','CAPACITYKV','STATE','SPATIALCONFIDENCE','REVISED','SHAPE_Length','x1','y1','x2','y2')
sp$NAME <- as.character(sp$NAME)

#### Line Editing ####

### Initially check for lines that do no have correct name specification ###

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

### Overall name editing ###

## Remove capacity values from bus names

index <- which(grepl(' \\d{3}',sp$NAME))
tmp <- sp[index,]
tmp$NAME <- gsub(" \\d{3}","", tmp$NAME)
sp[index,] <- tmp

## Find bus names that belong to different states

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

# Separate Tee connections from geographical data

index <- which(el == 'Tee') %% nrow(el)
tmp <- sp[index,]
t.coord <- as.matrix(sp[index,c(12:15)])
t.coord <- rbind(t.coord[1:6,1:2],t.coord[7:nrow(tmp),3:4]) %>% as.numeric() %>% matrix(ncol=2)

dist.thresh <- 500

count = 1
for (i in 1:nrow(t.coord)){
  
  if (grepl("Tee \\d", tmp$NAME[i])){
    
  } else{
  p <- do.call(rbind, replicate(nrow(t.coord), t.coord[i,], simplify=FALSE))
  d <- distHaversine(p,t.coord)
  dis <- which(d<dist.thresh)
  tmp$NAME[dis] <- sub('Tee', paste('Tee',count),tmp$NAME[dis])
  count = count+1
  }
  
}

sp[row.names(tmp),] <- tmp

# Some bus names occur in different states we fix by adding a unique state indicator
# T - Tasmania, Q - Queensland, etc.

names <- c('Guildford','Meadowbank','Palmerston', 'Belmont', 'Mount Barker', 'Victoria Park')

for (name in names){
  tmp <- sp[grepl(name,sp$NAME),]
  tmp$NAME <- sapply(1:nrow(tmp), function(x) sub(name, paste(name,gsub("[^::A-Z::]","", tmp$STATE[x])), tmp$NAME[x]))
  sp[row.names(tmp),] <- tmp
}

### Other line connection issues ###

# Torrens island changes into Torrens island A and Torrens island B without connection. Also Torrens B power station to Torrens island B.

# The Basslink connection between Tasmania and Victoria was not connected
sp[grepl('Basslink',sp$NAME),]
sp$NAME[grepl('Loy Yang Power Station to Basslink',sp$NAME)] = 'Loy Yang Power Station to Basslink-Loy Yang'
sp$NAME[grepl('Basslink George Town to George Town',sp$NAME)] = 'Basslink-George Town to George Town'

# It should be consistently Le Fevre
tmp <- sp[grepl('LeFevre',sp$NAME),]
tmp$NAME <- sapply(1:nrow(tmp), function(x) gsub('LeFevre','Le Fevre',tmp$NAME[x]))
sp[row.names(tmp),] <- tmp

# The terminal stations in Melbourne (connected to Rowville) vary between terminal and terminal station
index <- which(grepl('Rowville',sp$NAME))
sp$NAME[index] <- gsub(' Station', '',sp$NAME[index])


### Lines that aren't connected ###

# The use of the capacity in the name is not consistent

# # Update names of middle ridge to have capacity
# name = 'Middle Ridge'
# tmp <- sp[grepl(name,sp$NAME),]
# index <- which(grepl('\\d',tmp$NAME))
# tmp = tmp[-index,]
# tmp$NAME <- sapply(1:nrow(tmp), function(x) sub(name, paste(name,tmp$CAPACITYKV[x]), tmp$NAME[x]))
# sp[row.names(tmp),] = tmp
# 
# # Update names of bannaby to have capacity
# name = 'Bannaby'
# tmp <- sp[grepl(name,sp$NAME),]
# index <- which(grepl(paste(name,'\\d'),tmp$NAME))
# tmp = tmp[-index,]
# tmp$NAME <- sapply(1:nrow(tmp), function(x) sub(name, paste(name,tmp$CAPACITYKV[x]), tmp$NAME[x]))
# sp[row.names(tmp),] = tmp


# Mica Creek power station and lines are not connected to the grid
index <- which(grepl('Mica Creek',sp$NAME))
sp <- sp[-index,]

# Power plant in condabri is closed
index <- which(grepl('Condabri',sp$NAME))
sp <- sp[-index,]

# Capital wind farms should be Capital wind farm
sp$NAME[grepl('Capital Wind Farms',sp$NAME)][1:2] <- "Capital Wind Farm to Capital Wind Farm Substation"

# There is a spelling mistake of Tangkam cause the Oakey power station to be disconnected
sp$NAME[grepl('Tankham',sp$NAME)][1:2] <- 'Middle Ridge to Tangkam'


### Missing Lines

sp[grepl('Tankham',sp$NAME),]

# Missing line Dumaresq to Armidale 330kv

# Split names by the separator ' to ' to obtain bus names

name.ls <- strsplit(sp$NAME, ' to ')

# Create edgelist of lines

el <- matrix(unlist(name.ls), ncol = 2, byrow = TRUE)

# loops in the network
index <- which(el[,1]==el[,2])
el <- el[-index,]
sp <- sp[-index,]

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

## Identify Mistakes based on same coordinates

for (i in 1:nrow(coord)){
    p <- do.call(rbind, replicate(nrow(coord), coord[i,2:3], simplify=FALSE))
    d <- distHaversine(p,coord[,2:3])
    dis <- which(d<10)
    print(coord[dis,])
}


## Plot

lo <- layout.norm(as.matrix(cbind(V(net)$Long,V(net)$Lat)),-1, 1, -1, 1)

NEM <- plot.igraph(net,layout = lo,vertex.size = 0.5, vertex.label = NA)

# Remove WA and NT from the network

g <- subgraph.edges(net, E(net)[State!="Northern Territory"])
g <- subgraph.edges(g, E(g)[State!="Western Australia"])

g.graph <- ggraph(g,layout = 'kk') + 
  geom_edge_link(aes(color = E(g)$State)) + 
  theme_graph()

plot(g.graph)

ggsave('spatial_net.png', plot = g.graph, 
       path = '~/Documents/GitHub/Transmission-Systems/figures', 
       width = 8, height = 8 )

# Buses that aren't connected

list <- sapply(2:length(comp), function(x) V(comp[[x]]))
attributes(unlist(list))

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
