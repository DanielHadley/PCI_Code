# Read Me ####
# This code analyzes data from FST, the City's pavement consultant
# It models the cost and PCI degredation in order to optimize pavement decisions
# OCI value = PCI.  Note these scores are from November 2012.
# Created By Daniel Hadley

# Considerations: 
# 1. The knapsack approximates an optimal outcome, but needs to be inspected in cases where the 
# limit is low,E.g., http://oucsace.cs.ohiou.edu/~razvan/courses/cs4040/lecture16.pdf
# 2. The model glosses over differences in the deterioration of arterial vs residential for now
# These differences seemed small enough to gloss over for the sake of comparison
# 3. The model also glosses over differences in PCI reset of various treatment types.
# A full repavement would generally reset to PCI = 100, whereas this model by default resets 
# everthing to 95. Details: http://www.mylongview.com/modules/showdocument.aspx?documentid=631
# These differences also seemed trivial when comparing strategies

library("plyr")


# Import data ####
setwd("K:/Somerstat/Common/Data/2014 StreetStat/PCI_Code")
# setwd ("~/Documents/Git/PCI_Code") #at home
d <- read.csv("PCI.csv")


# Create new variables #### 
d$sq.ft <- d$PavementWi * d$Length # Sq. Feet
d$sq.yd <- d$sq.ft * 0.111111 # Sq. Yards
d$total.pci <- d$sq.yd * d$OCI # Sum PCI
d$ideal.pci <- d$sq.yd * 100 # Ideal sum PCI
d$delta.pci <- d$ideal.pci - d$total.pci #Difference
d$cost.per.sq.yd <- d$ExtendedCo / d$sq.yd # Cost per sq yard


# Here I model the age as a function of PCI based on the references below:
# It is an average of residential and arterial, but we can divide those later
# http://onlinepubs.trb.org/onlinepubs/conferences/2012/assetmgmt/presentations/Data-A-Ramirez-Flores-Chang-Albitres.pdf
# https://repository.tamu.edu/bitstream/handle/1969.1/ETD-TAMU-2009-05-317/DESHMUKH-THESIS.pdf?sequence=2
# http://www.mylongview.com/modules/showdocument.aspx?documentid=631
# PCI = 100 - (106/((ln(79/AGE))^(1/.48)))

d$est.years <- 79*(2.71828^(-9.37879/(100-d$OCI)^0.48))


###  Model the Pavement decisions over 20 years 


# Functions needed for the model: f(Age) = PCI, f(PCI) = AGE, f(PCI) = Cost & fknapsack = pave ####
# f(Age) = PCI
# I can differentiate between residetnial and collector, but the difference is small
# See AnalyzePCI.R
PCIf <- function(AGE){ 
  PCI <- 100 - (106/((log(79/AGE))^(1/.48)))
  return(PCI)
}


# f(PCI) = Cost 
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
  df <- df[with(df, order(ID)), ] # Resort by ID
  rownames(df) <- NULL # Reset the row names for easier indexing
  return(df$add)
}


# Now run the function X number of times ####
# http://stats.stackexchange.com/questions/7999/how-to-efficiently-repeat-a-function-on-a-data-set-in-r
library("plyr")
l <- alply(cbind(rep(1,1),rep(20,10)),1,Modelf)
backlog <- data.frame(matrix(unlist(l), nrow=1, byrow=T))
colnames(backlog) <- c("backlog", "backlog.reduction", "total.cost", "benefit.to.cost", 
                       "average.annual.cost", "first.year")

hist(backlog$benefit.to.cost)
hist(backlog$average.annual.cost)
hist(backlog$total.cost)
hist(backlog$first.year) # The first year is always the highest cost


                            
                                   


# Stochastic attempt ####
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


# Here is where I attempt to create a stochastic model 
# The problem WAS that it's difficult to set the limit on spending, particularly in years  1 &
# So I included a greedy knapsack algo in the model 
# f(PCI) = Whether or Not to Pave
# Here is where we set the rules and try different scenarios


Modelf <- function(n){
  d$OCI.Model <- PCIf(d$est.years) # Use the model instead of the empirical OCI
  d$backlog <- Costf(d$OCI.Model, d$Functional, d$sq.yd) # when summed, this gives you your backlog
  d$Pave.a <- knapsack((d$sq.yd * 100 - d$sq.yd * d$OCI.Model), d$backlog, 3000000) # Decision to pave
  d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.a <- ifelse(d$Pave.a == 1, 1, 1 + d$est.years) #Age in year n
  d$OCI.a <- PCIf(d$Age.a) # OCI year n  
  d$backlog.a <- ifelse(d$Pave.a == 0, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.b <- knapsack((d$sq.yd * 100 - d$sq.yd * d$OCI.a), d$backlog.a, 3000000) # Decision to pave
  d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.b <- ifelse(d$Pave.b == 1, 1, 1 + d$Age.a) #Age in year n
  d$OCI.b <- PCIf(d$Age.b) # OCI year n  
  d$backlog.b <- ifelse(d$Pave.b == 0, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.c <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.b),d$backlog.b, 3000000) # Decision to pave
  d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.c <- ifelse(d$Pave.c == 1, 1, 1 + d$Age.b) #Age in year n
  d$OCI.c <- PCIf(d$Age.c) # OCI year n  
  d$backlog.c <- ifelse(d$Pave.c == 0, Costf(d$OCI.b, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.d <- knapsack(((d$sq.yd * 100) - (d$sq.yd * d$OCI.c)), d$backlog.c, 3000000) # Decision to pave
  d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$OCI.b, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.d <- ifelse(d$Pave.d == 1, 1, 1 + d$Age.c) #Age in year n
  d$OCI.d <- PCIf(d$Age.d) # OCI year n  
  d$backlog.d <- ifelse(d$Pave.d == 0, Costf(d$OCI.c, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.e <- knapsack(((d$sq.yd * 100) - (d$sq.yd * d$OCI.d)), d$backlog.d, 3000000) # Decision to pave
  d$cost.e <- ifelse(d$Pave.e == 1, Costf(d$OCI.c, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.e <- ifelse(d$Pave.e == 1, 1, 1 + d$Age.c) #Age in year n
  d$OCI.e <- PCIf(d$Age.e) # OCI year n  
  d$backlog.e <- ifelse(d$Pave.e == 0, Costf(d$OCI.c,d$Functional, d$sq.yd),0) #Backlog after year n
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


# Attempt to put the model into a function that can be automated in a for loop ####
for (i in 1:10){
  d$Pave.[i] <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI),d$backlog, 3000000) # Decision to pave
  d$Age.[i] <- ifelse(d$Pave.[i] == 1, 1, 1 + d$Age.a) #Age in year n
  d$OCI.[i] <- PCIf(d$Age.[i]) # OCI year n  
  d$cost.[i] <- ifelse(d$Pave.[i] == 1, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$backlog.[i] <- ifelse(d$Pave.[i] == 0, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #Backlog after year n
}


# Final model: change in PCI as the "value"; cost = "weight", cap = $4.5m ####


# f(n) = output
Modelf <- function(n){
  d$OCI.Model <- PCIf(d$est.years) # Use the model instead of the empirical OCI
  d$backlog <- Costf(d$OCI.Model, d$Functional, d$sq.yd) # when summed, this gives you your backlog
  d$Pave.a <- knapsack((d$sq.yd * 100 - d$sq.yd * d$OCI.Model), d$backlog, 4500000) # Decision to pave
  d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.a <- ifelse(d$Pave.a == 1, 1, 1 + d$est.years) #Age in year n
  d$OCI.a <- PCIf(d$Age.a) # OCI year n  
  d$backlog.a <- ifelse(d$Pave.a == 0, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.b <- knapsack((d$sq.yd * 100 - d$sq.yd * d$OCI.a), d$backlog.a, 4500000) # Decision to pave
  d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.b <- ifelse(d$Pave.b == 1, 1, 1 + d$Age.a) #Age in year n
  d$OCI.b <- PCIf(d$Age.b) # OCI year n  
  d$backlog.b <- ifelse(d$Pave.b == 0, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.c <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.b),d$backlog.b, 4500000) # Decision to pave
  d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.c <- ifelse(d$Pave.c == 1, 1, 1 + d$Age.b) #Age in year n
  d$OCI.c <- PCIf(d$Age.c) # OCI year n  
  d$backlog.c <- ifelse(d$Pave.c == 0, Costf(d$OCI.b, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.d <- knapsack(((d$sq.yd * 100) - (d$sq.yd * d$OCI.c)), d$backlog.c, 4500000) # Decision to pave
  d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$OCI.b, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.d <- ifelse(d$Pave.d == 1, 1, 1 + d$Age.c) #Age in year n
  d$OCI.d <- PCIf(d$Age.d) # OCI year n  
  d$backlog.d <- ifelse(d$Pave.d == 0, Costf(d$OCI.c, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.e <- knapsack(((d$sq.yd * 100) - (d$sq.yd * d$OCI.d)), d$backlog.d, 4500000) # Decision to pave
  d$cost.e <- ifelse(d$Pave.e == 1, Costf(d$OCI.c, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.e <- ifelse(d$Pave.e == 1, 1, 1 + d$Age.c) #Age in year n
  d$OCI.e <- PCIf(d$Age.e) # OCI year n  
  d$backlog.e <- ifelse(d$Pave.e == 0, Costf(d$OCI.c,d$Functional, d$sq.yd),0) #Backlog after year n
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


# Final model to compare against the consultant: cap = $3 + the worst streets ####
# Backlog after 5 years for consultant spending $6 m is $73 m.

# f(n) = output
Modelf <- function(n){
  d$OCI.Model <- PCIf(d$est.years) # Use the model instead of the empirical OCI
  d$backlog <- Costf(d$OCI.Model, d$Functional, d$sq.yd) # when summed, this gives you your backlog
  d$Pave.a <- knapsack((d$sq.yd * 100 - d$sq.yd * d$OCI.Model), d$backlog, 3000000) # Decision to pave
  d$Pave.a <- ifelse(d$OCI.Model < 10, 1, d$Pave.a) # Pave the worst street
  d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.a <- ifelse(d$Pave.a == 1, 1, 1 + d$est.years) #Age in year n
  d$OCI.a <- PCIf(d$Age.a) # OCI year n  
  d$backlog.a <- ifelse(d$Pave.a == 0, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.b <- knapsack((d$sq.yd * 100 - d$sq.yd * d$OCI.a), d$backlog.a, 3000000) # Decision to pave
  d$Pave.b <- ifelse(d$OCI.a < 10, 1, d$Pave.b) # Pave the worst street
  d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.b <- ifelse(d$Pave.b == 1, 1, 1 + d$Age.a) #Age in year n
  d$OCI.b <- PCIf(d$Age.b) # OCI year n  
  d$backlog.b <- ifelse(d$Pave.b == 0, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.c <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.b),d$backlog.b, 3000000) # Decision to pave
  d$Pave.c <- ifelse(d$OCI.b < 10, 1, d$Pave.c) # Pave the worst street
  d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.c <- ifelse(d$Pave.c == 1, 1, 1 + d$Age.b) #Age in year n
  d$OCI.c <- PCIf(d$Age.c) # OCI year n  
  d$backlog.c <- ifelse(d$Pave.c == 0, Costf(d$OCI.b, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.d <- knapsack(((d$sq.yd * 100) - (d$sq.yd * d$OCI.c)), d$backlog.c, 3000000) # Decision to pave
  d$Pave.d <- ifelse(d$OCI.c < 10, 1, d$Pave.d) # Pave the worst street
  d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$OCI.b, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.d <- ifelse(d$Pave.d == 1, 1, 1 + d$Age.c) #Age in year n
  d$OCI.d <- PCIf(d$Age.d) # OCI year n  
  d$backlog.d <- ifelse(d$Pave.d == 0, Costf(d$OCI.c, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.e <- knapsack(((d$sq.yd * 100) - (d$sq.yd * d$OCI.d)), d$backlog.d, 3000000) # Decision to pave
  d$Pave.e <- ifelse(d$OCI.d < 10, 1, d$Pave.e) # Pave the worst street
  d$cost.e <- ifelse(d$Pave.e == 1, Costf(d$OCI.c, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.e <- ifelse(d$Pave.e == 1, 1, 1 + d$Age.d) #Age in year n
  d$OCI.e <- PCIf(d$Age.e) # OCI year n  
  d$backlog.e <- ifelse(d$Pave.e == 0, Costf(d$OCI.c,d$Functional, d$sq.yd),0) #Backlog after year n
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

difference <- 73103186 - 37577635


# Final model to compare against the consultant: cap = $1.5 + some of the worst streets ####
# Backlog after 5 years for consultant spending $4.5 m is $81,115,018 m.

# f(n) = output
Modelf <- function(n){
  d$OCI.Model <- PCIf(d$est.years) # Use the model instead of the empirical OCI
  d$backlog <- Costf(d$OCI.Model, d$Functional, d$sq.yd) # when summed, this gives you your backlog
  d$Pave.a <- knapsack((d$sq.yd * 100 - d$sq.yd * d$OCI.Model), d$backlog, 1500000) # Decision to pave
  d$Pave.a <- ifelse(d$OCI.Model < 7, 1, d$Pave.a) # Pave the worst street
  d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.a <- ifelse(d$Pave.a == 1, 1, 1 + d$est.years) #Age in year n
  d$OCI.a <- PCIf(d$Age.a) # OCI year n  
  d$backlog.a <- ifelse(d$Pave.a == 0, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.b <- knapsack((d$sq.yd * 100 - d$sq.yd * d$OCI.a), d$backlog.a, 1500000) # Decision to pave
  d$Pave.b <- ifelse(d$OCI.a < 7, 1, d$Pave.b) # Pave the worst street
  d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.b <- ifelse(d$Pave.b == 1, 1, 1 + d$Age.a) #Age in year n
  d$OCI.b <- PCIf(d$Age.b) # OCI year n  
  d$backlog.b <- ifelse(d$Pave.b == 0, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.c <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.b),d$backlog.b, 1500000) # Decision to pave
  d$Pave.c <- ifelse(d$OCI.b < 7, 1, d$Pave.c) # Pave the worst street
  d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.c <- ifelse(d$Pave.c == 1, 1, 1 + d$Age.b) #Age in year n
  d$OCI.c <- PCIf(d$Age.c) # OCI year n  
  d$backlog.c <- ifelse(d$Pave.c == 0, Costf(d$OCI.b, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.d <- knapsack(((d$sq.yd * 100) - (d$sq.yd * d$OCI.c)), d$backlog.c, 1500000) # Decision to pave
  d$Pave.d <- ifelse(d$OCI.c < 7, 1, d$Pave.d) # Pave the worst street
  d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$OCI.b, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.d <- ifelse(d$Pave.d == 1, 1, 1 + d$Age.c) #Age in year n
  d$OCI.d <- PCIf(d$Age.d) # OCI year n  
  d$backlog.d <- ifelse(d$Pave.d == 0, Costf(d$OCI.c, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.e <- knapsack(((d$sq.yd * 100) - (d$sq.yd * d$OCI.d)), d$backlog.d, 1500000) # Decision to pave
  d$Pave.e <- ifelse(d$OCI.d < 7, 1, d$Pave.e) # Pave the worst street
  d$cost.e <- ifelse(d$Pave.e == 1, Costf(d$OCI.c, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.e <- ifelse(d$Pave.e == 1, 1, 1 + d$Age.d) #Age in year n
  d$OCI.e <- PCIf(d$Age.e) # OCI year n  
  d$backlog.e <- ifelse(d$Pave.e == 0, Costf(d$OCI.c,d$Functional, d$sq.yd),0) #Backlog after year n
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

difference <- 45314093 - 81115000


# Conservative model to compare against the consultant: cap = $1.5 + some of the worst streets ####
# Backlog after 5 years for consultant spending $4.5 m is $81,115,018 m.
# Age = age + 3 years because most treatments do not reset the PCI to 100 (or even 95)
# It's a much more conservative approach 

# f(n) = output
Modelf <- function(n){
  d$OCI.Model <- PCIf(d$est.years) # Use the model instead of the empirical OCI
  d$backlog <- Costf(d$OCI.Model, d$Functional, d$sq.yd) # when summed, this gives you your backlog
  d$Pave.a <- knapsack((d$sq.yd * 100 - d$sq.yd * d$OCI.Model), d$backlog, 1500000) # Decision to pave
  d$Pave.a <- ifelse(d$OCI.Model < 7, 1, d$Pave.a) # Pave the worst street
  d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.a <- ifelse(d$Pave.a == 1, 3, 1 + d$est.years) #Age in year n
  d$OCI.a <- PCIf(d$Age.a) # OCI year n  
  d$backlog.a <- ifelse(d$Pave.a == 0, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.b <- knapsack((d$sq.yd * 100 - d$sq.yd * d$OCI.a), d$backlog.a, 1500000) # Decision to pave
  d$Pave.b <- ifelse(d$OCI.a < 7, 1, d$Pave.b) # Pave the worst street
  d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.b <- ifelse(d$Pave.b == 1, 3, 1 + d$Age.a) #Age in year n
  d$OCI.b <- PCIf(d$Age.b) # OCI year n  
  d$backlog.b <- ifelse(d$Pave.b == 0, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.c <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.b),d$backlog.b, 1500000) # Decision to pave
  d$Pave.c <- ifelse(d$OCI.b < 7, 1, d$Pave.c) # Pave the worst street
  d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.c <- ifelse(d$Pave.c == 1, 3, 1 + d$Age.b) #Age in year n
  d$OCI.c <- PCIf(d$Age.c) # OCI year n  
  d$backlog.c <- ifelse(d$Pave.c == 0, Costf(d$OCI.b, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.d <- knapsack(((d$sq.yd * 100) - (d$sq.yd * d$OCI.c)), d$backlog.c, 1500000) # Decision to pave
  d$Pave.d <- ifelse(d$OCI.c < 7, 1, d$Pave.d) # Pave the worst street
  d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$OCI.b, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.d <- ifelse(d$Pave.d == 1, 3, 1 + d$Age.c) #Age in year n
  d$OCI.d <- PCIf(d$Age.d) # OCI year n  
  d$backlog.d <- ifelse(d$Pave.d == 0, Costf(d$OCI.c, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.e <- knapsack(((d$sq.yd * 100) - (d$sq.yd * d$OCI.d)), d$backlog.d, 1500000) # Decision to pave
  d$Pave.e <- ifelse(d$OCI.d < 7, 1, d$Pave.e) # Pave the worst street
  d$cost.e <- ifelse(d$Pave.e == 1, Costf(d$OCI.c, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.e <- ifelse(d$Pave.e == 1, 3, 1 + d$Age.d) #Age in year n
  d$OCI.e <- PCIf(d$Age.e) # OCI year n  
  d$backlog.e <- ifelse(d$Pave.e == 0, Costf(d$OCI.c,d$Functional, d$sq.yd),0) #Backlog after year n
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

difference <- 81115000 -46069393 


# Conservative model to compare against the consultant: cap = $.2 + some of the very worst streets ####
# Backlog after 5 years for consultant spending $1.1 m is $114,530,924
# But it's strange because they seem to suggest that they will not pave the worst roads
# This is a difficult comparison
# Age = age + 3 years because most treatments do not reset the PCI to 100 (or even 95)
# It's a much more conservative approach 

# f(n) = output
Modelf <- function(n){
  d$OCI.Model <- PCIf(d$est.years) # Use the model instead of the empirical OCI
  d$backlog <- Costf(d$OCI.Model, d$Functional, d$sq.yd) # when summed, this gives you your backlog
  d$Pave.a <- knapsack((d$sq.yd * 100 - d$sq.yd * d$OCI.Model), d$backlog, 250000) # Decision to pave
  d$Pave.a <- ifelse(d$OCI.Model < 5, 1, d$Pave.a) # Pave the worst street
  d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.a <- ifelse(d$Pave.a == 1, 3, 1 + d$est.years) #Age in year n
  d$OCI.a <- PCIf(d$Age.a) # OCI year n  
  d$backlog.a <- ifelse(d$Pave.a == 0, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.b <- knapsack((d$sq.yd * 100 - d$sq.yd * d$OCI.a), d$backlog.a, 250000) # Decision to pave
  d$Pave.b <- ifelse(d$OCI.a < 4, 1, d$Pave.b) # Pave the worst street
  d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.b <- ifelse(d$Pave.b == 1, 3, 1 + d$Age.a) #Age in year n
  d$OCI.b <- PCIf(d$Age.b) # OCI year n  
  d$backlog.b <- ifelse(d$Pave.b == 0, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.c <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.b),d$backlog.b, 250000) # Decision to pave
  d$Pave.c <- ifelse(d$OCI.b < 3, 1, d$Pave.c) # Pave the worst street
  d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.c <- ifelse(d$Pave.c == 1, 3, 1 + d$Age.b) #Age in year n
  d$OCI.c <- PCIf(d$Age.c) # OCI year n  
  d$backlog.c <- ifelse(d$Pave.c == 0, Costf(d$OCI.b, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.d <- knapsack(((d$sq.yd * 100) - (d$sq.yd * d$OCI.c)), d$backlog.c, 250000) # Decision to pave
  d$Pave.d <- ifelse(d$OCI.c < 2, 1, d$Pave.d) # Pave the worst street
  d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$OCI.b, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.d <- ifelse(d$Pave.d == 1, 3, 1 + d$Age.c) #Age in year n
  d$OCI.d <- PCIf(d$Age.d) # OCI year n  
  d$backlog.d <- ifelse(d$Pave.d == 0, Costf(d$OCI.c, d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.e <- knapsack(((d$sq.yd * 100) - (d$sq.yd * d$OCI.d)), d$backlog.d, 250000) # Decision to pave
  d$Pave.e <- ifelse(d$OCI.d < 1, 1, d$Pave.e) # Pave the worst street
  d$cost.e <- ifelse(d$Pave.e == 1, Costf(d$OCI.c, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$Age.e <- ifelse(d$Pave.e == 1, 3, 1 + d$Age.d) #Age in year n
  d$OCI.e <- PCIf(d$Age.e) # OCI year n  
  d$backlog.e <- ifelse(d$Pave.e == 0, Costf(d$OCI.c,d$Functional, d$sq.yd),0) #Backlog after year n
  #   Now create the outputs
  backlog <- sum(d$backlog.e)
  backlog.reduction <- (sum(d$backlog)) - (sum(d$backlog.e))
  total.cost <- sum(d$cost.a, d$cost.b, d$cost.c, d$cost.d, d$cost.e)
  benefit.to.cost <- backlog.reduction / total.cost
  average.annual.cost <- ((sum(d$cost.a)) + (sum(d$cost.b)) + (sum(d$cost.c)) + 
                            (sum(d$cost.d)) + (sum(d$cost.e))) / 5
  first.year <- sum(d$cost.a)
  averagePCI <- mean(d$OCI.e)
  output <- list(backlog, backlog.reduction, total.cost, benefit.to.cost, average.annual.cost, 
                 first.year, averagePCI)
  return(output)
}

Modelf(1)


# Here are the Streets that DPW did in 2013 ####
Modelf <- function(n){

d$OCI.Model <- PCIf(d$est.years) # Use the model instead of the empirical OCI
d$backlog <- Costf(d$OCI.Model, d$Functional, d$sq.yd) # when summed, this gives you your backlog
## The following list is missing the private ways and the part of Shore they did. Was it all Shore?
d$Pave.a <- ifelse(d$STREETNAME %in% c('ALBION ST' , 'ALBION TERR', 'BELMONT SQ', 'HAMMOND ST',
                                       'MORGAN ST', 'WHEELER ST', 'WYATT ST', 'YORKTOWN ST'), 1, 0)
d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
d$Age.a <- ifelse(d$Pave.a == 1, 3, 1 + d$est.years) #Age in year n
d$OCI.a <- PCIf(d$Age.a) # OCI year n  
d$backlog.a <- ifelse(d$Pave.a == 0, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #Backlog after year n
d$Pave.b <- knapsack((d$sq.yd * 100 - d$sq.yd * d$OCI.a), d$backlog.a, 250000) # Decision to pave
d$Pave.b <- ifelse(d$OCI.a < 4, 1, d$Pave.b) # Pave the worst street
d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$OCI.Model, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
d$Age.b <- ifelse(d$Pave.b == 1, 3, 1 + d$Age.a) #Age in year n
d$OCI.b <- PCIf(d$Age.b) # OCI year n  
d$backlog.b <- ifelse(d$Pave.b == 0, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #Backlog after year n
d$Pave.c <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.b),d$backlog.b, 250000) # Decision to pave
d$Pave.c <- ifelse(d$OCI.b < 3, 1, d$Pave.c) # Pave the worst street
d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
d$Age.c <- ifelse(d$Pave.c == 1, 3, 1 + d$Age.b) #Age in year n
d$OCI.c <- PCIf(d$Age.c) # OCI year n  
d$backlog.c <- ifelse(d$Pave.c == 0, Costf(d$OCI.b, d$Functional, d$sq.yd),0) #Backlog after year n
d$Pave.d <- knapsack(((d$sq.yd * 100) - (d$sq.yd * d$OCI.c)), d$backlog.c, 250000) # Decision to pave
d$Pave.d <- ifelse(d$OCI.c < 2, 1, d$Pave.d) # Pave the worst street
d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$OCI.b, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
d$Age.d <- ifelse(d$Pave.d == 1, 3, 1 + d$Age.c) #Age in year n
d$OCI.d <- PCIf(d$Age.d) # OCI year n  
d$backlog.d <- ifelse(d$Pave.d == 0, Costf(d$OCI.c, d$Functional, d$sq.yd),0) #Backlog after year n
d$Pave.e <- knapsack(((d$sq.yd * 100) - (d$sq.yd * d$OCI.d)), d$backlog.d, 250000) # Decision to pave
d$Pave.e <- ifelse(d$OCI.d < 1, 1, d$Pave.e) # Pave the worst street
d$cost.e <- ifelse(d$Pave.e == 1, Costf(d$OCI.c, d$Functional, d$sq.yd),0) #The cost to pave the selected streets
d$Age.e <- ifelse(d$Pave.e == 1, 3, 1 + d$Age.d) #Age in year n
d$OCI.e <- PCIf(d$Age.e) # OCI year n  
d$backlog.e <- ifelse(d$Pave.e == 0, Costf(d$OCI.c,d$Functional, d$sq.yd),0) #Backlog after year n
#   Now create the outputs
backlog <- sum(d$backlog.e)
backlog.reduction <- (sum(d$backlog)) - (sum(d$backlog.e))
total.cost <- sum(d$cost.a, d$cost.b, d$cost.c, d$cost.d, d$cost.e)
benefit.to.cost <- backlog.reduction / total.cost
average.annual.cost <- ((sum(d$cost.a)) + (sum(d$cost.b)) + (sum(d$cost.c)) + 
                          (sum(d$cost.d)) + (sum(d$cost.e))) / 5
first.year <- sum(d$cost.a)
averagePCI <- mean(d$OCI.e)
output <- list(backlog, backlog.reduction, total.cost, benefit.to.cost, average.annual.cost, 
               first.year, averagePCI)
return(output)
}

Modelf(1)


