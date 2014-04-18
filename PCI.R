# Read Me ####
# This code analyzes data from FST, the City's pavement consultant
# It models the cost and PCI degredation in order to optimize pavement decisions
# OCI value = PCI.  Note these scores are from November 2012.
# Created By Daniel Hadley

# Considerations: 
# 1. The knapsack approximates an optimal outcome, but needs to be inspected in cases where the 
# limit is low,E.g., http://oucsace.cs.ohiou.edu/~razvan/courses/cs4040/lecture16.pdf
# 2. The model makes assumptions about the differences in PCI reset of various treatment types.
# It assumes that patching only resets to PCI + 6, for example. 

library("plyr")


# Import data ####
setwd("K:/Somerstat/Common/Data/2014 StreetStat/PCI_Code")
# setwd ("~/Documents/Git/PCI_Code") #at home
d <- read.csv("PCI.csv")


# Create new variables #### 
d$sq.ft <- d$PavementWi * d$Length # Sq. Feet
d$sq.yd <- d$sq.ft * 0.111111 # Sq. Yards


# Functions needed for the model: ####
# f(Age) = PCI, f(PCI) = AGE, f(Age, Pave, Old PCI) = New PCI 
# f(PCI) = AGE, f(PCI) = Cost, f(knapsack) = pave 

# f(Age) = PCI
# Referenes below
# http://onlinepubs.trb.org/onlinepubs/conferences/2012/assetmgmt/presentations/Data-A-Ramirez-Flores-Chang-Albitres.pdf
# https://repository.tamu.edu/bitstream/handle/1969.1/ETD-TAMU-2009-05-317/DESHMUKH-THESIS.pdf?sequence=2
# http://www.mylongview.com/modules/showdocument.aspx?documentid=631
# See AnalyzePCI.R
PCIf <- function(AGE){ 
  PCI <- ifelse(d$Functional == "CO - Collector", 100 - (106/((log(79/AGE))^(1/.48))),
                ifelse(d$Functional == "AR - Arterial", 100 - (109/((log(88/AGE))^(1/.58))),
                       100 - (97/((log(110/AGE))^(1/.61)))
                ))
  return(PCI)
}


# f(PCI, Function, Sq.yd) = Cost 
Costf <- function(OCI, Functional, sq.yd){ 
  Cost <- ifelse((OCI >= 68) & (OCI < 88), 1.8,
         ifelse((OCI >= 47) & (OCI < 68), 18.50,
                ifelse((OCI >= 25) & (OCI < 47) & (Functional == "RT - Residential Local"), 76.80,
                       ifelse((OCI >= 25) & (OCI < 47) & (Functional == "RE - Residential Dead End"), 76.80,
                              ifelse((OCI >= 25) & (OCI < 47) & (Functional == "CO - Collector" ), 91.10,
                                     ifelse((OCI >= 25) & (OCI < 47) & (Functional == "AR - Arterial"), 91.10,
                                            ifelse((OCI >= 0) & (OCI < 25) & (Functional == "RT - Residential Local"), 139.80,
                                                   ifelse((OCI >= 0) & (OCI < 25) & (Functional == "RE - Residential Dead End"), 139.80,
                                                          ifelse((OCI >= 0) & (OCI < 25) & (Functional == "CO - Collector"), 147.70,
                                                                 ifelse((OCI >= 0) & (OCI < 25) & (Functional == "AR - Arterial"), 162.10,
                                                                        ifelse(OCI >= 88, 0, 360)))))))))))
  return(Cost*sq.yd)
}


#f(Pave, Previous Year's Moritorium) = Moritorium
Moritoriumf <- function(Pave, MoritoriumX){
  Moritorium <- ifelse(Pave == 1, 1, 
                       ifelse((MoritoriumX > 0) & (MoritoriumX < 3), 1 + MoritoriumX,
                              0 
                              ))
  return(Moritorium)
}


# f(PCI) = delta
# This takes the current PCI and calculates the difference between that and the PCI post treatment
# This is essentially the "value" in the knapsack algo
# The penultimate line ensures that during the moratorium (3 years after work is done), that
# street is not selected for maintenance yet
Deltaf <- function(OldOCI, sq.yd, Moratorium){ 
   Delta <- ifelse((OldOCI >= 68) & (OldOCI < 88) & (Moratorium == 0), ((OldOCI + 8) * sq.yd) - (OldOCI * sq.yd),
                   ifelse((OldOCI >= 47) & (OldOCI < 68) & (Moratorium == 0), ((OldOCI + 7) * sq.yd) - (OldOCI * sq.yd),
                   # why + 8 and +9: http//www.ci.san-ramon.ca.us/engr/pavement.html
                   ifelse((OldOCI >= 25) & (OldOCI < 47) & (Moratorium == 0), (96 * sq.yd) - (OldOCI * sq.yd),
                          ifelse(Moratorium != 0, 0, 
                          (100 * sq.yd) - (OldOCI * sq.yd)
                                 ))))                      
  return(Delta)
}


# f(value, weight, limit ) = pave. This is the basic Pave function
# This is a greedy approximation to the Knapsack algorithm
knapsack <- function(value, weight, limit){
  benefit.to.cost <- value / weight #Create ratio
  df = data.frame(value, weight, benefit.to.cost) # turn it into a DF
  df$ID <- (1:nrow(df)) # add ID to resort later
  df <- df[with(df, order(-benefit.to.cost)), ] # Sort by benefit.to.cost
  rownames(df) <- NULL # Reset the row names for easier indexing
  df$total.weight <- ifelse(cumsum(df$weight) <= limit, cumsum(df$weight), 0) # Add first items that fit
  # I need to add a break here if nothing fits in the bag on the first pass
  for(i in 2:nrow(df)){ #Start in row 2 because some values have been added above
    df$total.weight[i] <- ifelse(df$weight[i] + df$total.weight[i-1] <= limit, # If adding won't go over limit
                                 df$weight[i] + df$total.weight[i-1], df$total.weight[i-1]) # If it will, keep Weight the same
  }
  df$add <- 0
  df$add[1] <- ifelse(df$total.weight[1] > 0, 1, 0)
  for(i in 2:nrow(df)){ #Start in row 2 
    df$add[i] <- ifelse(df$total.weight[i] > df$total.weight[i-1], 1, 0) # 1 if it has been added
  }
  df$add <- ifelse(df$benefit.to.cost == 0, 0, df$add) #To avoid paving streets under moratorium
  df$add <- ifelse(df$value == 0, 0, df$add)
  df <- df[with(df, order(ID)), ] # Resort by ID
  rownames(df) <- NULL # Reset the row names for easier indexing
  return(df$add)
}


# f(Age, Pave, Old PCI) = New PCI
# See AnalyzePCI.R
NewPCIf <- function(AGE, Pave, OldOCI){
  NewPCI <- ifelse((OldOCI >= 68) & (OldOCI < 88) & (Pave == 1), OldOCI + 8,
                   # why + 7: http//www.ci.san-ramon.ca.us/engr/pavement.html
                   ifelse((OldOCI >= 47) & (OldOCI < 68) & (Pave == 1), OldOCI + 7,
                          ifelse((OldOCI >= 25) & (OldOCI < 47) & (Pave == 1), 96, 
                                 # http://www.mylongview.com/modules/showdocument.aspx?documentid=631
                                 ifelse((OldOCI >= 0) & (OldOCI < 25) & (Pave == 1), 100,
                                        ifelse((d$Functional == "CO - Collector") & (Pave == 0), 100 - (106/((log(79/AGE))^(1/.48))),
                                               ifelse((d$Functional == "AR - Arterial") & (Pave == 0), 100 - (109/((log(88/AGE))^(1/.58))),
                                                      100 - (97/((log(110/AGE))^(1/.61)))
                                 ))))))                      
  return(NewPCI)
}


# f(NewPCI) = New Age
Agef <- function(NewPCI){
  NewAge <- ifelse((d$Functional == "CO - Collector"), 79*(2.71828^(-9.37879/(100-NewPCI)^0.48)),
                   ifelse((d$Functional == "AR - Arterial"), 88*(2.71828^(-15.1952/(100-NewPCI)^0.58)),
                          110*(2.71828^(-16.2904/(100-NewPCI)^0.61))
                   ))
                   
  return(NewAge)
}


# Final model ####

d$Age <- Agef(d$OCI) # Estimated age in Nov 2012
  d$backlog <- Costf(d$OCI, d$Functional, d$sq.yd) # Backlog in Nov 2012
  d$Moritorium <- 0 # Hold for three years between routine maintenance 
  d$Delta.a <- Deltaf(d$OCI, d$sq.yd, d$Moritorium) # Difference between old OCI and potential OCI
  d$Pave.a <- knapsack(d$Delta.a, d$backlog, 1500000) # Pave or not in spring 2013
  d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Moritorium.a <- Moritoriumf(d$Pave.a, d$Moritorium) #if work has been done, hold for 3 years
  d$Age.Tmp <- (1 + d$Age) #Temporary to do the new PCI calculation
  d$PCI.a <-  NewPCIf(d$Age.Tmp, d$Pave.a, d$OCI)
  d$Age.a <- Agef(d$PCI.a) # The new "age" in Nov 2013. E.g., crack and seal streets are not brand new.
d$backlog.a <- Costf(d$PCI.a, d$Functional, d$sq.yd) #Backlog in Nov 2013
  d$Delta.b <- Deltaf(d$PCI.a, d$sq.yd, d$Moritorium.a)
  d$Pave.b <- knapsack(d$Delta.b, d$backlog.a, 1500000)
  d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$PCI.a, d$Functional, d$sq.yd),0)
  d$Moritorium.b <- Moritoriumf(d$Pave.b, d$Moritorium.a)
  d$Age.Tmp <- (1 + d$Age.a) 
  d$PCI.b <-  NewPCIf(d$Age.Tmp, d$Pave.b, d$PCI.a)
  d$Age.b <- Agef(d$PCI.b)
d$backlog.b <- Costf(d$PCI.b, d$Functional, d$sq.yd)
  d$Delta.c <- Deltaf(d$PCI.b, d$sq.yd, d$Moritorium.b)
  d$Pave.c <- knapsack(d$Delta.c, d$backlog.b, 1500000)
  d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$PCI.b, d$Functional, d$sq.yd),0)
  d$Moritorium.c <- Moritoriumf(d$Pave.c, d$Moritorium.b)
  d$Age.Tmp <- (1 + d$Age.b) 
  d$PCI.c <-  NewPCIf(d$Age.Tmp, d$Pave.c, d$PCI.b)
  d$Age.c <- Agef(d$PCI.c)
d$backlog.c <- Costf(d$PCI.c, d$Functional, d$sq.yd)
  d$Delta.d <- Deltaf(d$PCI.c, d$sq.yd, d$Moritorium.c)
  d$Pave.d <- knapsack(d$Delta.d, d$backlog.c, 1500000)
  d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$PCI.c, d$Functional, d$sq.yd),0)
  d$Moritorium.d <- Moritoriumf(d$Pave.d, d$Moritorium.c)
  d$Age.Tmp <- (1 + d$Age.c) 
  d$PCI.d <-  NewPCIf(d$Age.Tmp, d$Pave.d, d$PCI.c)
  d$Age.d <- Agef(d$PCI.d)
d$backlog.d <- Costf(d$PCI.d, d$Functional, d$sq.yd)
  d$Delta.e <- Deltaf(d$PCI.d, d$sq.yd, d$Moritorium.d)
  d$Pave.e <- knapsack(d$Delta.e, d$backlog.d, 1500000)
  d$cost.e <- ifelse(d$Pave.e == 1, Costf(d$PCI.d, d$Functional, d$sq.yd),0)
  d$Moritorium.e <- Moritoriumf(d$Pave.e, d$Moritorium.d)
  d$Age.Tmp <- (1 + d$Age.d) 
  d$PCI.e <-  NewPCIf(d$Age.Tmp, d$Pave.e, d$PCI.d)
  d$Age.e <- Agef(d$PCI.e)
d$backlog.e <- Costf(d$PCI.e, d$Functional, d$sq.yd)


# Final model wrapped in a function ####

# f(n) = output
Modelf <- function(n){
  d$Age <- Agef(d$OCI) # Estimated age in Nov 2012
  d$backlog <- Costf(d$OCI, d$Functional, d$sq.yd) # Backlog in Nov 2012
  d$Moritorium <- 0 # Hold for three years between routine maintenance 
  d$Delta.a <- Deltaf(d$OCI, d$sq.yd, d$Moritorium) # Difference between old OCI and potential OCI
  d$Pave.a <- knapsack(d$Delta.a, d$backlog, 1500000) # Pave or not in spring 2013
  d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Moritorium.a <- Moritoriumf(d$Pave.a, d$Moritorium) #if work has been done, hold for 3 years
  d$Age.Tmp <- (1 + d$Age) #Temporary to do the new PCI calculation
  d$PCI.a <-  NewPCIf(d$Age.Tmp, d$Pave.a, d$OCI)
  d$Age.a <- Agef(d$PCI.a) # The new "age" in Nov 2013. E.g., crack and seal streets are not brand new.
  d$backlog.a <- Costf(d$PCI.a, d$Functional, d$sq.yd) #Backlog in Nov 2013
  d$Delta.b <- Deltaf(d$PCI.a, d$sq.yd, d$Moritorium.a)
  d$Pave.b <- knapsack(d$Delta.b, d$backlog.a, 1500000)
  d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$PCI.a, d$Functional, d$sq.yd),0)
  d$Moritorium.b <- Moritoriumf(d$Pave.b, d$Moritorium.a)
  d$Age.Tmp <- (1 + d$Age.a) 
  d$PCI.b <-  NewPCIf(d$Age.Tmp, d$Pave.b, d$PCI.a)
  d$Age.b <- Agef(d$PCI.b)
  d$backlog.b <- Costf(d$PCI.b, d$Functional, d$sq.yd)
  d$Delta.c <- Deltaf(d$PCI.b, d$sq.yd, d$Moritorium.b)
  d$Pave.c <- knapsack(d$Delta.c, d$backlog.b, 1500000)
  d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$PCI.b, d$Functional, d$sq.yd),0)
  d$Moritorium.c <- Moritoriumf(d$Pave.c, d$Moritorium.b)
  d$Age.Tmp <- (1 + d$Age.b) 
  d$PCI.c <-  NewPCIf(d$Age.Tmp, d$Pave.c, d$PCI.b)
  d$Age.c <- Agef(d$PCI.c)
  d$backlog.c <- Costf(d$PCI.c, d$Functional, d$sq.yd)
  d$Delta.d <- Deltaf(d$PCI.c, d$sq.yd, d$Moritorium.c)
  d$Pave.d <- knapsack(d$Delta.d, d$backlog.c, 1500000)
  d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$PCI.c, d$Functional, d$sq.yd),0)
  d$Moritorium.d <- Moritoriumf(d$Pave.d, d$Moritorium.c)
  d$Age.Tmp <- (1 + d$Age.c) 
  d$PCI.d <-  NewPCIf(d$Age.Tmp, d$Pave.d, d$PCI.c)
  d$Age.d <- Agef(d$PCI.d)
  d$backlog.d <- Costf(d$PCI.d, d$Functional, d$sq.yd)
  d$Delta.e <- Deltaf(d$PCI.d, d$sq.yd, d$Moritorium.d)
  d$Pave.e <- knapsack(d$Delta.e, d$backlog.d, 1500000)
  d$cost.e <- ifelse(d$Pave.e == 1, Costf(d$PCI.d, d$Functional, d$sq.yd),0)
  d$Moritorium.e <- Moritoriumf(d$Pave.e, d$Moritorium.d)
  d$Age.Tmp <- (1 + d$Age.d) 
  d$PCI.e <-  NewPCIf(d$Age.Tmp, d$Pave.e, d$PCI.d)
  d$Age.e <- Agef(d$PCI.e)
  d$backlog.e <- Costf(d$PCI.e, d$Functional, d$sq.yd)
#   Now create the outputs
backlog <- sum(d$backlog.e)
backlog.reduction <- (sum(d$backlog)) - (sum(d$backlog.e))
total.cost <- sum(d$cost.a, d$cost.b, d$cost.c, d$cost.d, d$cost.e)
benefit.to.cost <- backlog.reduction / total.cost
average.annual.cost <- ((sum(d$cost.a)) + (sum(d$cost.b)) + (sum(d$cost.c)) + 
                          (sum(d$cost.d)) + (sum(d$cost.e))) / 5
first.year <- sum(d$cost.a)
output <- list(backlog, backlog.reduction, total.cost, benefit.to.cost, average.annual.cost, first.year)
return(output)
}

Modelf(1)


# Stochastic Model for comparison ####
# f(value, weight, limit ) = pave. 
# This is a greedy approximation to the Knapsack algorithm
knapsack <- function(value, weight, limit){
  benefit.to.cost <- sample(1:573, 573, replace=F) #Instead of real ratio, make it random
  df = data.frame(value, weight, benefit.to.cost) # turn it into a DF
  df$ID <- (1:nrow(df)) # add ID to resort later
  df <- df[with(df, order(-benefit.to.cost)), ] # Sort by benefit.to.cost
  rownames(df) <- NULL # Reset the row names for easier indexing
  df$total.weight <- ifelse(cumsum(df$weight) <= limit, cumsum(df$weight), 0) # Add first items that fit
  # I need to add a break here if nothing fits in the bag on the first pass
  for(i in 2:nrow(df)){ #Start in row 2 because some values have been added above
    df$total.weight[i] <- ifelse(df$weight[i] + df$total.weight[i-1] <= limit, # If adding won't go over limit
                                 df$weight[i] + df$total.weight[i-1], df$total.weight[i-1]) # If it will, keep Weight the same
  }
  df$add <- 0
  df$add[1] <- ifelse(df$total.weight[1] > 0, 1, 0)
  for(i in 2:nrow(df)){ #Start in row 2 
    df$add[i] <- ifelse(df$total.weight[i] > df$total.weight[i-1], 1, 0) # 1 if it has been added
  }
  df <- df[with(df, order(ID)), ] # Resort by ID
  rownames(df) <- NULL # Reset the row names for easier indexing
  return(df$add)
}


Modelf <- function(n){
  d$Age <- Agef(d$OCI) # Estimated age in Nov 2012
  d$backlog <- Costf(d$OCI, d$Functional, d$sq.yd) # Backlog in Nov 2012
  d$Moritorium <- 0 # Hold for three years between routine maintenance 
  d$Delta.a <- Deltaf(d$OCI, d$sq.yd, d$Moritorium) # Difference between old OCI and potential OCI
  d$Pave.a <- knapsack(d$Delta.a, d$backlog, 1500000) # Pave or not in spring 2013
  d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Moritorium.a <- Moritoriumf(d$Pave.a, d$Moritorium) #if work has been done, hold for 3 years
  d$Age.Tmp <- (1 + d$Age) #Temporary to do the new PCI calculation
  d$PCI.a <-  NewPCIf(d$Age.Tmp, d$Pave.a, d$OCI)
  d$Age.a <- Agef(d$PCI.a) # The new "age" in Nov 2013. E.g., crack and seal streets are not brand new.
  d$backlog.a <- Costf(d$PCI.a, d$Functional, d$sq.yd) #Backlog in Nov 2013
  d$Delta.b <- Deltaf(d$PCI.a, d$sq.yd, d$Moritorium.a)
  d$Pave.b <- knapsack(d$Delta.b, d$backlog.a, 1500000)
  d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$PCI.a, d$Functional, d$sq.yd),0)
  d$Moritorium.b <- Moritoriumf(d$Pave.b, d$Moritorium.a)
  d$Age.Tmp <- (1 + d$Age.a) 
  d$PCI.b <-  NewPCIf(d$Age.Tmp, d$Pave.b, d$PCI.a)
  d$Age.b <- Agef(d$PCI.b)
  d$backlog.b <- Costf(d$PCI.b, d$Functional, d$sq.yd)
  d$Delta.c <- Deltaf(d$PCI.b, d$sq.yd, d$Moritorium.b)
  d$Pave.c <- knapsack(d$Delta.c, d$backlog.b, 1500000)
  d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$PCI.b, d$Functional, d$sq.yd),0)
  d$Moritorium.c <- Moritoriumf(d$Pave.c, d$Moritorium.b)
  d$Age.Tmp <- (1 + d$Age.b) 
  d$PCI.c <-  NewPCIf(d$Age.Tmp, d$Pave.c, d$PCI.b)
  d$Age.c <- Agef(d$PCI.c)
  d$backlog.c <- Costf(d$PCI.c, d$Functional, d$sq.yd)
  d$Delta.d <- Deltaf(d$PCI.c, d$sq.yd, d$Moritorium.c)
  d$Pave.d <- knapsack(d$Delta.d, d$backlog.c, 1500000)
  d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$PCI.c, d$Functional, d$sq.yd),0)
  d$Moritorium.d <- Moritoriumf(d$Pave.d, d$Moritorium.c)
  d$Age.Tmp <- (1 + d$Age.c) 
  d$PCI.d <-  NewPCIf(d$Age.Tmp, d$Pave.d, d$PCI.c)
  d$Age.d <- Agef(d$PCI.d)
  d$backlog.d <- Costf(d$PCI.d, d$Functional, d$sq.yd)
  d$Delta.e <- Deltaf(d$PCI.d, d$sq.yd, d$Moritorium.d)
  d$Pave.e <- knapsack(d$Delta.e, d$backlog.d, 1500000)
  d$cost.e <- ifelse(d$Pave.e == 1, Costf(d$PCI.d, d$Functional, d$sq.yd),0)
  d$Moritorium.e <- Moritoriumf(d$Pave.e, d$Moritorium.d)
  d$Age.Tmp <- (1 + d$Age.d) 
  d$PCI.e <-  NewPCIf(d$Age.Tmp, d$Pave.e, d$PCI.d)
  d$Age.e <- Agef(d$PCI.e)
  d$backlog.e <- Costf(d$PCI.e, d$Functional, d$sq.yd)
  #   Now create the outputs
  backlog <- sum(d$backlog.e)
  backlog.reduction <- (sum(d$backlog)) - (sum(d$backlog.e))
  total.cost <- sum(d$cost.a, d$cost.b, d$cost.c, d$cost.d, d$cost.e)
  benefit.to.cost <- backlog.reduction / total.cost
  average.annual.cost <- ((sum(d$cost.a)) + (sum(d$cost.b)) + (sum(d$cost.c)) + 
                            (sum(d$cost.d)) + (sum(d$cost.e))) / 5
  first.year <- sum(d$cost.a)
  output <- list(backlog, backlog.reduction, total.cost, benefit.to.cost, average.annual.cost, first.year)
  return(output)
}

Modelf(1)

# Now run the function X number of times
# http://stats.stackexchange.com/questions/7999/how-to-efficiently-repeat-a-function-on-a-data-set-in-r
library("plyr")
l <- alply(cbind(rep(100,100),rep(20,10)),1,Modelf)
backlog <- data.frame(matrix(unlist(l), nrow=100, byrow=T))
colnames(backlog) <- c("backlog", "backlog.reduction", "total.cost", "benefit.to.cost", 
                       "average.annual.cost", "first.year")

hist(backlog$benefit.to.cost)
hist(backlog$average.annual.cost)
hist(backlog$total.cost)
hist(backlog$first.year)


# Final model for comparison to consultant: cap = $2m + worst streets  ####
# Backlog after 5 years for consultant spending $4.5 m is $81,115,018 m.

# f(n) = output
Modelf <- function(n){
  d$Age <- Agef(d$OCI) # Estimated age in Nov 2012
  d$backlog <- Costf(d$OCI, d$Functional, d$sq.yd) # Backlog in Nov 2012
  d$Moritorium <- 0 # Hold for three years between routine maintenance 
  d$Delta.a <- Deltaf(d$OCI, d$sq.yd, d$Moritorium) # Difference between old OCI and potential OCI
  d$Pave.a <- knapsack(d$Delta.a, d$backlog, 1500000) # Pave or not in spring 2013
  d$Pave.a <- ifelse(d$OCI < 10, 1, d$Pave.a) # Pave the worst streets
  d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Moritorium.a <- Moritoriumf(d$Pave.a, d$Moritorium) #if work has been done, hold for 3 years
  d$Age.Tmp <- (1 + d$Age) #Temporary to do the new PCI calculation
  d$PCI.a <-  NewPCIf(d$Age.Tmp, d$Pave.a, d$OCI)
  d$Age.a <- Agef(d$PCI.a) # The new "age" in Nov 2013. E.g., crack and seal streets are not brand new.
  d$backlog.a <- Costf(d$PCI.a, d$Functional, d$sq.yd) #Backlog in Nov 2013
  d$Delta.b <- Deltaf(d$PCI.a, d$sq.yd, d$Moritorium.a)
  d$Pave.b <- knapsack(d$Delta.b, d$backlog.a, 1500000)
  d$Pave.b <- ifelse(d$PCI.a < 10, 1, d$Pave.b) 
  d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$PCI.a, d$Functional, d$sq.yd),0)
  d$Moritorium.b <- Moritoriumf(d$Pave.b, d$Moritorium.a)
  d$Age.Tmp <- (1 + d$Age.a) 
  d$PCI.b <-  NewPCIf(d$Age.Tmp, d$Pave.b, d$PCI.a)
  d$Age.b <- Agef(d$PCI.b)
  d$backlog.b <- Costf(d$PCI.b, d$Functional, d$sq.yd)
  d$Delta.c <- Deltaf(d$PCI.b, d$sq.yd, d$Moritorium.b)
  d$Pave.c <- knapsack(d$Delta.c, d$backlog.b, 1500000)
  d$Pave.c <- ifelse(d$PCI.b < 10, 1, d$Pave.c)
  d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$PCI.b, d$Functional, d$sq.yd),0)
  d$Moritorium.c <- Moritoriumf(d$Pave.c, d$Moritorium.b)
  d$Age.Tmp <- (1 + d$Age.b) 
  d$PCI.c <-  NewPCIf(d$Age.Tmp, d$Pave.c, d$PCI.b)
  d$Age.c <- Agef(d$PCI.c)
  d$backlog.c <- Costf(d$PCI.c, d$Functional, d$sq.yd)
  d$Delta.d <- Deltaf(d$PCI.c, d$sq.yd, d$Moritorium.c)
  d$Pave.d <- knapsack(d$Delta.d, d$backlog.c, 1500000)
  d$Pave.d <- ifelse(d$PCI.c < 10, 1, d$Pave.d)
  d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$PCI.c, d$Functional, d$sq.yd),0)
  d$Moritorium.d <- Moritoriumf(d$Pave.d, d$Moritorium.c)
  d$Age.Tmp <- (1 + d$Age.c) 
  d$PCI.d <-  NewPCIf(d$Age.Tmp, d$Pave.d, d$PCI.c)
  d$Age.d <- Agef(d$PCI.d)
  d$backlog.d <- Costf(d$PCI.d, d$Functional, d$sq.yd)
  d$Delta.e <- Deltaf(d$PCI.d, d$sq.yd, d$Moritorium.d)
  d$Pave.e <- knapsack(d$Delta.e, d$backlog.d, 1500000)
  d$Pave.e <- ifelse(d$PCI.d < 10, 1, d$Pave.e)
  d$cost.e <- ifelse(d$Pave.e == 1, Costf(d$PCI.d, d$Functional, d$sq.yd),0)
  d$Moritorium.e <- Moritoriumf(d$Pave.e, d$Moritorium.d)
  d$Age.Tmp <- (1 + d$Age.d) 
  d$PCI.e <-  NewPCIf(d$Age.Tmp, d$Pave.e, d$PCI.d)
  d$Age.e <- Agef(d$PCI.e)
  d$backlog.e <- Costf(d$PCI.e, d$Functional, d$sq.yd)
  #   Now create the outputs
  backlog <- sum(d$backlog.e)
  backlog.reduction <- (sum(d$backlog)) - (sum(d$backlog.e))
  total.cost <- sum(d$cost.a, d$cost.b, d$cost.c, d$cost.d, d$cost.e)
  benefit.to.cost <- backlog.reduction / total.cost
  average.annual.cost <- ((sum(d$cost.a)) + (sum(d$cost.b)) + (sum(d$cost.c)) + 
                            (sum(d$cost.d)) + (sum(d$cost.e))) / 5
  first.year <- sum(d$cost.a)
  output <- list(backlog, backlog.reduction, total.cost, benefit.to.cost, average.annual.cost, first.year)
  return(output)
}

Modelf(1)


# Here are the Streets that DPW did in 2013 (need moritorium) ####
Modelf <- function(n){
  d$Age <- Agef(d$OCI) # Estimated age in Nov 2012
  d$backlog <- Costf(d$OCI, d$Functional, d$sq.yd) # Backlog in Nov 2012
  ## The following list is missing the private ways and the part of Shore they did. Was it all Shore?
  d$Pave.a <- ifelse(d$STREETNAME %in% c('ALBION ST' , 'ALBION TERR', 'BELMONT SQ', 'HAMMOND ST',
                                         'MORGAN ST', 'WHEELER ST', 'WYATT ST', 'YORKTOWN ST'), 1, 0)
  d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.Tmp <- (1 + d$Age) #Temporary to do the new PCI calculation
  d$PCI.a <-  NewPCIf(d$Age.Tmp, d$Pave.a, d$OCI)
  d$Age.a <- Agef(d$PCI.a) # The new "age" in Nov 2013. E.g., crack and seal streets are not brand new.
  d$backlog.a <- Costf(d$PCI.a, d$Functional, d$sq.yd) #Backlog in Nov 2013
return(sum(d$backlog))
}


# Stochastic Model (random spending number) to see return on investment ####

Modelf <- function(n){
  random <- runif(1, min=10000, max=3000000)
  d$Age <- Agef(d$OCI) # Estimated age in Nov 2012
  d$backlog <- Costf(d$OCI, d$Functional, d$sq.yd) # Backlog in Nov 2012
  d$Moritorium <- 0 # Hold for three years between routine maintenance 
  d$Delta.a <- Deltaf(d$OCI, d$sq.yd, d$Moritorium) # Difference between old OCI and potential OCI
  d$Pave.a <- knapsack(d$Delta.a, d$backlog, random) # Pave or not in spring 2013
  d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Moritorium.a <- Moritoriumf(d$Pave.a, d$Moritorium) #if work has been done, hold for 3 years
  d$Age.Tmp <- (1 + d$Age) #Temporary to do the new PCI calculation
  d$PCI.a <-  NewPCIf(d$Age.Tmp, d$Pave.a, d$OCI)
  d$Age.a <- Agef(d$PCI.a) # The new "age" in Nov 2013. E.g., crack and seal streets are not brand new.
  d$backlog.a <- Costf(d$PCI.a, d$Functional, d$sq.yd) #Backlog in Nov 2013
  d$Delta.b <- Deltaf(d$PCI.a, d$sq.yd, d$Moritorium.a)
  d$Pave.b <- knapsack(d$Delta.b, d$backlog.a, random)
  d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$PCI.a, d$Functional, d$sq.yd),0)
  d$Moritorium.b <- Moritoriumf(d$Pave.b, d$Moritorium.a)
  d$Age.Tmp <- (1 + d$Age.a) 
  d$PCI.b <-  NewPCIf(d$Age.Tmp, d$Pave.b, d$PCI.a)
  d$Age.b <- Agef(d$PCI.b)
  d$backlog.b <- Costf(d$PCI.b, d$Functional, d$sq.yd)
  d$Delta.c <- Deltaf(d$PCI.b, d$sq.yd, d$Moritorium.b)
  d$Pave.c <- knapsack(d$Delta.c, d$backlog.b, random)
  d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$PCI.b, d$Functional, d$sq.yd),0)
  d$Moritorium.c <- Moritoriumf(d$Pave.c, d$Moritorium.b)
  d$Age.Tmp <- (1 + d$Age.b) 
  d$PCI.c <-  NewPCIf(d$Age.Tmp, d$Pave.c, d$PCI.b)
  d$Age.c <- Agef(d$PCI.c)
  d$backlog.c <- Costf(d$PCI.c, d$Functional, d$sq.yd)
  d$Delta.d <- Deltaf(d$PCI.c, d$sq.yd, d$Moritorium.c)
  d$Pave.d <- knapsack(d$Delta.d, d$backlog.c, random)
  d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$PCI.c, d$Functional, d$sq.yd),0)
  d$Moritorium.d <- Moritoriumf(d$Pave.d, d$Moritorium.c)
  d$Age.Tmp <- (1 + d$Age.c) 
  d$PCI.d <-  NewPCIf(d$Age.Tmp, d$Pave.d, d$PCI.c)
  d$Age.d <- Agef(d$PCI.d)
  d$backlog.d <- Costf(d$PCI.d, d$Functional, d$sq.yd)
  d$Delta.e <- Deltaf(d$PCI.d, d$sq.yd, d$Moritorium.d)
  d$Pave.e <- knapsack(d$Delta.e, d$backlog.d, random)
  d$cost.e <- ifelse(d$Pave.e == 1, Costf(d$PCI.d, d$Functional, d$sq.yd),0)
  d$Moritorium.e <- Moritoriumf(d$Pave.e, d$Moritorium.d)
  d$Age.Tmp <- (1 + d$Age.d) 
  d$PCI.e <-  NewPCIf(d$Age.Tmp, d$Pave.e, d$PCI.d)
  d$Age.e <- Agef(d$PCI.e)
  d$backlog.e <- Costf(d$PCI.e, d$Functional, d$sq.yd)
  #   Now create the outputs
  random <- random
  backlog <- sum(d$backlog.e)
  backlog.reduction <- (sum(d$backlog)) - (sum(d$backlog.e))
  total.cost <- sum(d$cost.a, d$cost.b, d$cost.c, d$cost.d, d$cost.e)
  benefit.to.cost <- backlog.reduction / total.cost
  average.annual.cost <- ((sum(d$cost.a)) + (sum(d$cost.b)) + (sum(d$cost.c)) + 
                            (sum(d$cost.d)) + (sum(d$cost.e))) / 5
  first.year <- sum(d$cost.a)
  output <- list(backlog, backlog.reduction, total.cost, benefit.to.cost, 
                 average.annual.cost, first.year, random)
  return(output)
}

Modelf(1)

# Now run the function X number of times
# http://stats.stackexchange.com/questions/7999/how-to-efficiently-repeat-a-function-on-a-data-set-in-r
library("plyr")
l <- alply(cbind(rep(1000,1000),rep(20,10)),1,Modelf)
backlog <- data.frame(matrix(unlist(l), nrow=1000, byrow=T))
colnames(backlog) <- c("backlog", "backlog.reduction", "total.cost", "benefit.to.cost", 
                       "average.annual.cost", "first.year", "PMSpending")

hist(backlog$benefit.to.cost)
hist(backlog$average.annual.cost)
hist(backlog$total.cost)
hist(backlog$first.year)


