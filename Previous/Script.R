rm(list=ls())

setwd("X:/Postcode xy coords/")

# Create function which,
#    for each group type, 
#       for each point in group type, 
#          identifies other points in a finite radius
#          calculates the squared distance from each point
#          calculates ingroup/outgroup weight based on squared distance

# indat: input data
# i : index of individual of interest 
# radiusofinterst : radius of interest - data points outside of this radius will be 
#    excluded
# xvar : name of variable giving x axis coordinates
# yvar : name of variable giving y axis coordinates

# groupvariable : name of variable giving group identity category

# k : amount of decay in influence as a function of distance 
#    default 2: influence decays with square of distance

PushPull <- function(indat, 
                     i,
                     xvar="x", yvar="y",
                     radiusofinterest=500, groupvariable="minority", k=2){
  
  # first find x and y position of individual i
  
  my.x <- indat[i, xvar]
  my.y <- indat[i, yvar]
  
  my.group <- indat[i, groupvariable]
  
  indat <- indat[-i,] # not interested in self in calculating
   # mean distances from same/other
  
  x <- indat[,xvar]
  y <- indat[,yvar]
  
  keep <- ((my.x - x)^2 + (my.y - y)^2)^0.5 < radiusofinterest
  
  indat <- subset(indat, subset=keep)
  
  x <- indat[,xvar]
  y <- indat[,yvar]
  
  # need to add all ingroups at the same location
  # and subtract all outgroups at the same location
  
  # then add weighted ingroups at other locations
  # then subtract weighted outgroups at other locations

  ingroup <- indat[,groupvariable] == my.group
  
  indat.ingroup <- subset(indat, subset=ingroup==T)
  indat.outgroup <- subset(indat, subset=ingroup==F)
  
  x.in <- indat.ingroup[,xvar]
  y.in <- indat.ingroup[,yvar]
  
  x.out <- indat.outgroup[,xvar]
  y.out <- indat.outgroup[,yvar]
  
  # Need to include catch for if there are no 
  # ingroups or no outgroups in the area of interest
  
  d.in <- ((my.x - x.in) ^ 2 + (my.y - y.in)^2)^0.5
  d.out <- ((my.x - x.out) ^ 2 + (my.y - y.out) ^ 2) ^ 0.5
  
  weight.in <- sum(exp(-k * d.in))
  weight.out <- sum(exp(-k * d.out))
  
  dweight <- weight.in - weight.out
  
  return(dweight) 
}
# Load all data points
Dta <- read.csv("SC_AllAddresses_pcode_xy_ONLY.csv")

names(Dta)
N <- dim(Dta)[1]
tmp <- runif(N)
minority <- ifelse(tmp < 0.10, 1, 0)

Dta <- data.frame(Dta, minority=minority)
names(Dta)[1:2] <- c("x", "y")

pushpulls <- rep(NA, N)

for (i in 1:N){
  pushpulls[i] <- PushPull(Dta, i)
}


# plot all points (n.b. takes a while...)
#plot(AddressPoint_y ~ AddressPoint_x, data=Data, pch=".")

# Smoothed version:
#smoothScatter(x=Data$AddressPoint_x, y=Data$AddressPoint_y, nbin=256)
#smoothScatter(x=Data$AddressPoint_x, y=Data$AddressPoint_y, nbin=512)
#smoothScatter(x=Data$AddressPoint_x, y=Data$AddressPoint_y, nbin=1024)

# take a sample of addresses to work with
N <- dim(Dta)[1]
Data.subset <- Data[sample(1:N, 10000),]

plot(AddressPoint_y ~ AddressPoint_x, data=Data.subset, pch=".")

# randomly assign races to households



Data.subset <- data.frame(Data.subset, minority=minority)

plot(AddressPoint_y ~ AddressPoint_x, data=subset(Data.subset, subset=minority==0), pch=".", col="blue")
points(AddressPoint_y ~ AddressPoint_x, data=subset(Data.subset, subset=minority==1), pch=".", col="red")


Data.subset <- subset(Data, subset=AddressPoint_x > 2070932 & AddressPoint_x < 3271446 & AddressPoint_y> 5778564 & AddressPoint_y < 7148397 )

plot(AddressPoint_y ~ AddressPoint_x, data=Data.subset, pch=".")

Data.subset <- subset(Data, subset=AddressPoint_x > 2400000 & AddressPoint_x < 2900000 & AddressPoint_y> 6400000 & AddressPoint_y < 6900000 )

plot(AddressPoint_y ~ AddressPoint_x, data=Data.subset, pch=".")


N.ss <- dim(Data.subset)[1]

rnd <- runif(N.ss)
minority <- ifelse(rnd < 0.10, 1,0)

Data.subset <- data.frame(Data.subset, minority=minority)

names(Data.subset)
names(Data.subset) <- c("x", "y", "pcode", "minority")

plot(y ~ x, data=Data.subset, col=ifelse(Data.subset$minority==1, "red" ,"blue"), pch=".")

require(MASS)

Data.maj <- subset(Data.subset, subset=minority==0)
Data.min <- subset(Data.subset, subset=minority==1)

# 1000 by 1000 data points
# need to find common minimums and maximums

x.min <- max(c(min(Data.min$x, Data.maj$x)))
x.max <- min(c(max(Data.min$x, Data.maj$x)))

y.min <- max(c(min(Data.min$y, Data.maj$y)))
y.max <- min(c(max(Data.min$y, Data.maj$y)))

dens.maj <- kde2d(Data.maj$x, Data.maj$y, n=2000, lims=c(x.min, x.max, y.min, y.max))
dens.min <- kde2d(Data.min$x, Data.min$y, n=2000, lims=c(x.min, x.max, y.min, y.max))


