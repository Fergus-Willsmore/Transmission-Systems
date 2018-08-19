# initial exploration of TER rating data

ter <- read.csv('~/downloads/PUBLIC_TER_DAILY.csv',skip = 1,header = F)
ter <- ter[ter$V6=='LINE',]

ter2 <- data.frame(ter$V5,ter$V7)
ter2 <- ter2[!duplicated(ter2),]

nrow(ter2)
