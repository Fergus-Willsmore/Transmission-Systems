

gal <- read.csv('~/downloads/PUBLIC_DISPATCHSCADA_201808041245_0000000297681856.csv')
sum(gal[-1,7])
gen<-as.numeric(levels(gal[-1,7]))[gal[-1,7]]
gen<-gen[-304]
sum(gen)
