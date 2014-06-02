# Main script


rm(list=ls())


require(httr)
require(MASS)
require(plyr)


source("Scripts/Functions.R")
source("Scripts/Manage_Data.R")


minority <- Make_Fake_Minorities(
  dim(Postcode_Data)[1],
  0.1
  )

Data <- data.frame(
  Postcode_Data,
  minority=minority
  )
rm(minority)
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
