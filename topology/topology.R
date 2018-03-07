# Script: creates the network topology from planned network outage data
# Data: PUBLIC_NOSDAILY_2018030515000037.csv available at https://www.aemo.com.au/Electricity/National-Electricity-Market-NEM/Data/Network-Data/Network-Outage-Schedule
# Created by F.R. Willsmore 6/2/2018

# Import packages

library(tidyverse)
library(stringr)

# Read csv data with headers and one intro line

outage.df <- read.csv('~/Documents/GitHub/Transmission-Systems/topology/PUBLIC_NOSDAILY_2018030515000037 copy.csv', 
                      header = TRUE, sep = ",", skip = 1) 

# We are only interested in the line outages, but might use the bus information for validation.
# In Equipment.Type there is a tline, which seems to extend between 3 buses. (???)

ls <- c(which(outage.df$Equipment.Type == "line"))
line.df <- outage.df$Equipment.Name[ls]

# Obtain minimal dataframe by removing duplicates in Equipment.Type

index <- which(duplicated(line.df))
line.df <- as.character(line.df[-index])

bus <- outage.df$Station.Name
index <- which(duplicated(bus))
bus <- as.character(bus[-index])

# Equipment.Name data is not nice: abbreviations are used for Station.Name and highphens occaissionally.

line.ls <- unlist(lapply(line.df,function(x) strsplit(x," ")))
bus.ls <- unlist(lapply(bus,function(x) strsplit(x," ")))

line.ls <-unlist(line.ls)


