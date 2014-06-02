
# Functions for Ethnic Balance project
# Jon Minton
# 2/6/2014




Make_Fake_Minorities <- function(
  N,
  prob=0.1
  ){

  output <- ifelse(
    runif(N),
    1,
    0
    )
  return(output)  
}

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
                     radiusofinterest=500, 
                     groupvariable="minority", 
                     k=2
                     ){
  
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


Calc_PushPull <- function(
  Data,
  ...
  ){
  N <- dim(Data)[1]
  
  output <- vector("numeric", N)
  
  # STILL TO COMPLETE
  
  
  
#   pushpulls <- rep(NA, N)
#   
#   for (i in 1:N){
#     pushpulls[i] <- PushPull(Dta, i)
#   }
  
  
  
}

Unpack_Zip <- function(
  zipfile,
  outlocation,
  wait.val=F
){
  dir.create(outlocation, recursive=T)
  
  command_for_system <- paste0(
    "unzip ", 
    zipfile,
    " -d ",
    outlocation
  )
  
  system(
    command_for_system, 
    wait=wait.val 
  )
}


