# This code cleans and analyzes data from FST, the pavement consultant
# OCI value = PCI.  Note these scores are from November 2012.
# It models the cost and PCI degredation in order to optimize pavement decisions
# Created By Daniel Hadley

setwd("K:/Somerstat/Common/Data/2014 StreetStat/PCI_Code")
# setwd ("~/Documents/Git/PCI_Code") #at home
d <- read.csv("PCI.csv")

# Create new variables 
d$sq.ft <- d$PavementWi * d$Length # Sq. Feet
d$sq.yd <- d$sq.ft * 0.111111 # Sq. Yards
d$total.pci <- d$sq.yd * d$OCI # Sum PCI
d$ideal.pci <- d$sq.yd * 95 # Ideal sum PCI
d$delta.pci <- d$ideal.pci - d$total.pci #Difference
d$delta.over.cost <- d$delta.pci / d$ExtendedCo #One cost measure
aggregate(delta.over.cost ~ PlanActivi, d, mean ) # Crack Seal is crazy cost efficient
aggregate(delta.over.cost ~ PlanActivi, d, max ) # Crack Seal is crazy cost efficient

d$cost.per.sq.yd <- d$ExtendedCo / d$sq.yd # Cost per sq yard


# Here I model the age as a function of PCI based on the references below:
# It is an average of residential and arterial, but we can divide those later
# http://onlinepubs.trb.org/onlinepubs/conferences/2012/assetmgmt/presentations/Data-A-Ramirez-Flores-Chang-Albitres.pdf
# https://repository.tamu.edu/bitstream/handle/1969.1/ETD-TAMU-2009-05-317/DESHMUKH-THESIS.pdf?sequence=2
# http://www.mylongview.com/modules/showdocument.aspx?documentid=631
# PCI = 100 - (106/((ln(79/AGE))^(1/.48)))

d$est.years <- 79*(2.71828^(-9.37879/(100-d$OCI)^0.48))
plot(d$est.years, d$OCI)

d$last.paved <- 2012 - d$est.years
hist(d$last.paved) # I'm skeptical there are streets we have not paved since the 80s


# test
plot(log(d$ExtendedCo), d$OCI)
plot(d$delta.over.cost, d$OCI) # checking cost-effectiveness
# This final plot is where we see that high OCI maintenance is the most cost-effective

# First model cost as a f(PCI) using logical tests: 
# this averages differences between collectors and arterials
# To see where the bands begin and end
aggregate(OCI ~ PlanActivi, d, mean )
aggregate(OCI ~ PlanActivi, d, min )
aggregate(OCI ~ PlanActivi, d, max )

d$cost.per.sq.yd.conditional <- ifelse((d$OCI >= 68) & (d$OCI < 88), 1.8,
                            ifelse((d$OCI >= 47) & (d$OCI < 68), 18.50,
                            ifelse((d$OCI >= 25) & (d$OCI < 47) & (d$Functional == "RT - Residential Local"), 76.80,
                            ifelse((d$OCI >= 25) & (d$OCI < 47) & (d$Functional == "RE - Residential Dead End"), 76.80,
                            ifelse((d$OCI >= 25) & (d$OCI < 47) & (d$Functional == "CO - Collector" ), 91.10,
                            ifelse((d$OCI >= 25) & (d$OCI < 47) & (d$Functional == "AR - Arterial"), 91.10,
                            ifelse((d$OCI >= 0) & (d$OCI < 25) & (d$Functional == "RT - Residential Local"), 139.80,
                            ifelse((d$OCI >= 0) & (d$OCI < 25) & (d$Functional == "RE - Residential Dead End"), 139.80,
                            ifelse((d$OCI >= 0) & (d$OCI < 25) & (d$Functional == "CO - Collector"), 147.70,
                            ifelse((d$OCI >= 0) & (d$OCI < 25) & (d$Functional == "AR - Arterial"), 162.10,
                            ifelse(d$OCI >= 88, 0, 360)))))))))))

# Test
plot(d$cost.per.sq.yd.conditional*d$sq.yd, d$cost.per.sq.yd*d$sq.yd)
plot(log(d$cost.per.sq.yd.conditional*d$sq.yd), log(d$cost.per.sq.yd*d$sq.yd))
# d$dif <- d$cost.per.sq.yd*d$sq.yd - d$cost.per.sq.yd.conditional*d$sq.yd  
# hugeDif <- d[ which(d$dif>650000 | d$dif < -115000), ]



  




###  Model the Pavement decisions over 20 years ###

# First I create functions that describe the relationships analyzed previously
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


# f(PCI) = Whether or Not to Pave
# Here is where we set the rules and try different scenarios
# This scenario is random with no bottom tiers
Pavef <- function(OCI){
  pave <- (ifelse((OCI >= 68) & (OCI < 88), 1,
                  ifelse((OCI >= 47) & (OCI < 68), 1,
                         ifelse(OCI == 47, 1, 0)))) # Top tiers
  return(pave)
}

# This scenario is totally random
Pavef <- function(OCI){
  pave <-  sample(c(0,1), 573, replace = TRUE) #random
  return(pave)
}

# Best - first. The problem is that you cannot control for the large 1st year cost
Pavef <- function(OCI){
  pave <- (ifelse((OCI >= 50),(sample(c(0,1), 573, replace = TRUE)),0))
  return(pave)
}



# f(n) = output
Modelf <- function(n){
    d$OCI.Model <- PCIf(d$est.years) # Use the model instead of the empirical OCI
    d$backlog <- Costf(d$OCI.Model, d$Functional, d$sq.yd) # when summed, this gives you your backlog
    d$Pave.a <- Pavef(d$OCI.Model) # Decision to pave based on Pavef function
    d$Age.a <- ifelse(d$Pave.a == 1, 1, 1 + d$est.years) #Age in year n
    d$OCI.a <- PCIf(d$Age.a) # OCI year n  
    d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI.Model,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
    d$backlog.a <- ifelse(d$Pave.a == 0, Costf(d$OCI.Model,d$Functional, d$sq.yd),0) #Backlog after year n
    d$Pave.b <- Pavef(d$OCI.a) # Decision to pave based on Pavef function
    d$Age.b <- ifelse(d$Pave.b == 1, 1, 1 + d$Age.a) #Age in year n
    d$OCI.b <- PCIf(d$Age.b) # OCI year n  
    d$cost.b <- ifelse(d$Pave.b == 1, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
    d$backlog.b <- ifelse(d$Pave.b == 0, Costf(d$OCI.a,d$Functional, d$sq.yd),0) #Backlog after year n
    d$Pave.c <- Pavef(d$OCI.b) # Decision to pave based on Pavef function
    d$Age.c <- ifelse(d$Pave.c == 1, 1, 1 + d$Age.b) #Age in year n
    d$OCI.c <- PCIf(d$Age.c) # OCI year n  
    d$cost.c <- ifelse(d$Pave.c == 1, Costf(d$OCI.b,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
    d$backlog.c <- ifelse(d$Pave.c == 0, Costf(d$OCI.b,d$Functional, d$sq.yd),0) #Backlog after year n
    d$Pave.d <- Pavef(d$OCI.c) # Decision to pave based on Pavef function
    d$Age.d <- ifelse(d$Pave.d == 1, 1, 1 + d$Age.c) #Age in year n
    d$OCI.d <- PCIf(d$Age.d) # OCI year n  
    d$cost.d <- ifelse(d$Pave.d == 1, Costf(d$OCI.c,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
    d$backlog.d <- ifelse(d$Pave.d == 0, Costf(d$OCI.c,d$Functional, d$sq.yd),0) #Backlog after year n
    d$Pave.e <- Pavef(d$OCI.d) # Decision to pave based on Pavef function
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
l <- alply(cbind(rep(100,100),rep(20,10)),1,Modelf)
backlog <- data.frame(matrix(unlist(l), nrow=100, byrow=T))
colnames(backlog) <- c("backlog", "backlog.reduction", "total.cost", "benefit.to.cost", 
                       "average.annual.cost", "first.year")

hist(backlog$benefit.to.cost)
hist(backlog$average.annual.cost)
hist(backlog$total.cost)
hist(backlog$first.year) # The first year is always the highest cost













# This scenario is totally random
Pavef <- function(OCI){ repeat {
  # do something
  pave <-  sample(c(0,1), 573, replace = TRUE) #random
  # exit if the condition is met
  if (sum(pave) < 200) break
}
return(pave)
}
  


# f(n) = output
Modelf <- function(n){
  d$OCI.Model <- PCIf(d$est.years) # Use the model instead of the empirical OCI
  d$backlog <- Costf(d$OCI.Model, d$Functional, d$sq.yd) # when summed, this gives you your backlog
  d$Pave.a <- Pavef(d$OCI.Model) # Decision to pave based on Pavef function
  d$Age.a <- ifelse(d$Pave.a == 1, 1, 1 + d$est.years) #Age in year n
  d$OCI.a <- PCIf(d$Age.a) # OCI year n  
  d$cost.a <- ifelse(d$Pave.a == 1, Costf(d$OCI.Model,d$Functional, d$sq.yd),0) #The cost to pave the selected streets
  d$backlog.a <- ifelse(d$Pave.a == 0, Costf(d$OCI.Model,d$Functional, d$sq.yd),0) #Backlog after year n
  #   Now create the outputs
  output <- sum(d$cost.a)
  return(output)
}

# Now run the function X number of times
# http://stats.stackexchange.com/questions/7999/how-to-efficiently-repeat-a-function-on-a-data-set-in-r
l <- alply(cbind(rep(100,100),rep(20,10)),1,Modelf)
backlog <- data.frame(matrix(unlist(l), nrow=100, byrow=T))
colnames(backlog) <- c("backlog", "backlog.reduction", "total.cost", "benefit.to.cost", 
                       "average.annual.cost", "first.year")

hist(backlog$benefit.to.cost)
hist(backlog$average.annual.cost)
hist(backlog$total.cost)
hist(backlog$first.year) # The first year is always the highest cost










###  Visualize ###
library(ggplot2)

my.theme <- 
  theme(plot.background = element_blank(), # Remove background
        panel.grid.major = element_blank(), # Remove gridlines
        panel.grid.minor = element_blank(), # Remove more gridlines
        panel.border = element_blank(), # Remove border
        panel.background = element_blank(), # Remove more background
        axis.ticks = element_blank(), # Remove axis ticks
        axis.text=element_text(size=24), # Enlarge axis text font
        axis.title=element_text(size=26), # Enlarge axis title font
        plot.title=element_text(size=42, hjust=0) # Enlarge, left-align title
        ,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )


# A simple method is to use the "weight" function with qplot. This will even work with aggregate
p <- qplot(PlanActivi, weight = Length, data = d, geom = "bar", alpha=I(.7), main="Activity by Length", ylab="Col2 Count")
p + my.theme

p <- qplot(Treatmentb, weight = Length, data = d, geom = "bar", alpha=I(.7), main="Activity by Length", ylab="Col2 Count")
p + my.theme

p <- qplot(OCI, weight = Length, data = d, colour = "white", fill=Treatmentb, geom = "bar", main="PCI by Length", ylab="Length of Roadway")
p + my.theme


ag <- aggregate(OCI ~ PlanActivi, d, mean )
p <- qplot(PlanActivi, weight = OCI, data = ag, geom = "bar", alpha=I(.7), main="Activity by Avg PCI", ylab="PCI")
p + my.theme

                            
                                   

