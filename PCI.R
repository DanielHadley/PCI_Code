# This code cleans and analyzes data from FST, the pavement consultant
# OCI value = PCI.  Note these scores are from November 2012.
# It models the cost and PCI degredation in order to optimize pavement decisions
# Created By Daniel Hadley

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


# First I create functions that describe the relationships analyzed previously ####
# f(Age) = PCI 
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

# f(value, weight, limit ) = pave. 
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


# Here is where I attempt to create a stochastic model ####
# The problem WAS that it's difficult to set the limit on spending, particularly in years  1 &
# So I included a greedy knapsack algo in the model 
# f(PCI) = Whether or Not to Pave
# Here is where we set the rules and try different scenarios


# f(n) = output
Modelf <- function(n){
      d$OCI.Model <- PCIf(d$est.years) # Use the model instead of the empirical OCI
      d$backlog <- Costf(d$OCI.Model, d$Functional, d$sq.yd) # when summed, this gives you your backlog
      d$Pave.a <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.Model),d$backlog, 3000000) # Decision to pave
      # Decision here is made by running a knapsack algo on the value = delta in PCI
      d$Age.a <- ifelse(d$Pave.a == 1, 1, 1 + d$est.years) #Age in year n
      d$OCI.a <- PCIf(d$Age.a) # OCI year n  
      d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI.Model,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
    d$backlog.a <- ifelse(d$Pave.a == 0, Costf(d$OCI.Model,d$Functional, d$sq.yd),0) #Backlog after year n
      d$Pave.b <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.a),d$backlog.a, 3000000) # Decision to pave
      d$Age.b <- ifelse(d$Pave.b == 1, 1, 1 + d$Age.a) #Age in year n
      d$OCI.b <- PCIf(d$Age.b) # OCI year n  
      d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
    d$backlog.b <- ifelse(d$Pave.b == 0, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #Backlog after year n
      d$Pave.c <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.b),d$backlog.b, 3000000) # Decision to pave
      d$Age.c <- ifelse(d$Pave.c == 1, 1, 1 + d$Age.b) #Age in year n
      d$OCI.c <- PCIf(d$Age.c) # OCI year n  
      d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$OCI.b,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
    d$backlog.c <- ifelse(d$Pave.c == 0, Costf(d$OCI.b,d$Functional, d$sq.yd),0) #Backlog after year n
      d$Pave.d <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.c),d$backlog.c, 3000000) # Decision to pave
      d$Age.d <- ifelse(d$Pave.d == 1, 1, 1 + d$Age.c) #Age in year n
      d$OCI.d <- PCIf(d$Age.d) # OCI year n  
      d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$OCI.c,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
    d$backlog.d <- ifelse(d$Pave.d == 0, Costf(d$OCI.c,d$Functional, d$sq.yd),0) #Backlog after year n
      d$Pave.e <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.d),d$backlog.d, 3000000) # Decision to pave
      d$Age.e <- ifelse(d$Pave.e == 1, 1, 1 + d$Age.d) #Age in year n
      d$OCI.e <- PCIf(d$Age.e) # OCI year n  
      d$cost.e <- ifelse(d$Pave.e == 1, Costf(d$OCI.d,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
    d$backlog.e <- ifelse(d$Pave.e == 0, Costf(d$OCI.d,d$Functional, d$sq.yd),0) #Backlog after year n    
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


# f(n) = output
Modelf <- function(n){
  d$OCI.Model <- PCIf(d$est.years) # Use the model instead of the empirical OCI
  d$backlog <- Costf(d$OCI.Model, d$Functional, d$sq.yd) # when summed, this gives you your backlog
  d$Pave.a <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.Model),d$backlog, 3000000) # Decision to pave
  # Decision here is made by running a knapsack algo on the value = delta in PCI
  d$Age.a <- ifelse(d$Pave.a == 1, 1, 1 + d$est.years) #Age in year n
  d$OCI.a <- PCIf(d$Age.a) # OCI year n  
  d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI.Model,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$backlog.a <- ifelse(d$Pave.a == 0, Costf(d$OCI.Model,d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.b <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.a),d$backlog.a, 3000000) # Decision to pave
  d$Age.b <- ifelse(d$Pave.b == 1, 1, 1 + d$Age.a) #Age in year n
  d$OCI.b <- PCIf(d$Age.b) # OCI year n  
  d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$backlog.b <- ifelse(d$Pave.b == 0, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.c <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.b),d$backlog.b, 3000000) # Decision to pave
  d$Age.c <- ifelse(d$Pave.c == 1, 1, 1 + d$Age.b) #Age in year n
  d$OCI.c <- PCIf(d$Age.c) # OCI year n  
  d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$OCI.b,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$backlog.c <- ifelse(d$Pave.c == 0, Costf(d$OCI.b,d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.d <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.c),d$backlog.c, 3000000) # Decision to pave
  d$Age.d <- ifelse(d$Pave.d == 1, 1, 1 + d$Age.c) #Age in year n
  d$OCI.d <- PCIf(d$Age.d) # OCI year n  
  d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$OCI.c,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$backlog.d <- ifelse(d$Pave.d == 0, Costf(d$OCI.c,d$Functional, d$sq.yd),0) #Backlog after year n
  d$Pave.e <- knapsack((d$sq.yd*100 - d$sq.yd*d$OCI.d),d$backlog.d, 3000000) # Decision to pave
  d$Age.e <- ifelse(d$Pave.e == 1, 1, 1 + d$Age.d) #Age in year n
  d$OCI.e <- PCIf(d$Age.e) # OCI year n  
  d$cost.e <- ifelse(d$Pave.e == 1, Costf(d$OCI.d,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$backlog.e <- ifelse(d$Pave.e == 0, Costf(d$OCI.d,d$Functional, d$sq.yd),0) #Backlog after year n    
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
l <- alply(cbind(rep(1000,1000),rep(20,10)),1,Modelf)
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




