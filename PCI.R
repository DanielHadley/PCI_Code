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



# # Now model the cost degredation as a f(PCI) using a smooth curve
# # This smooths out all of the "cliffs" from the difference maintenance bands, 
# # but it approximates how quickly costs escalate as a f of PCI and allows for linear optimization
# 
# ####  Use the empirical distribution of PCI ####
# # http://davetang.org/muse/2013/05/09/on-curve-fitting/
# y = d$cost.per.sq.yd 
# x = d$OCI
# plot(x,y)
# 
# #fit first degree polynomial equation:
# fit  <- lm(y~x)
# #second degree
# fit2 <- lm(y~poly(x,2,raw=TRUE))
# #third degree
# fit3 <- lm(y~poly(x,3,raw=TRUE))
# #fourth degree
# fit4 <- lm(y~poly(x,4,raw=TRUE))
# #generate range of 50 numbers starting from 30 and ending at 160
# xx <- seq(0,160, length=50)
# plot(x,y,pch=19,ylim=c(0,150))
# lines(xx, predict(fit, data.frame(x=xx)), col="red")
# lines(xx, predict(fit2, data.frame(x=xx)), col="green")
# lines(xx, predict(fit3, data.frame(x=xx)), col="blue")
# lines(xx, predict(fit4, data.frame(x=xx)), col="purple") ## This looks like the best fit
# 
# # Now model it based on the coefficients from fit4
# #y=e + dx + cx^2 + bx^3 + ax^4 
# coef(fit4)
# 
# d$cost.model <- 1.351524e+02 + (1.845730e+00 * d$OCI) +  
#   (-1.630135e-01 * d$OCI^2) + 
#   (2.195987e-03 * d$OCI^3) + 
#   (-8.857414e-06 * d$OCI^4) 
# 
# 
# fit <- lm(d$cost.model,d$cost.per.sq.yd)
# summary(fit) # show results
# 
# # Visualize
# plot(d$cost.model*d$sq.yd, d$cost.per.sq.yd*d$sq.yd)
# plot(log(d$cost.model*d$sq.yd), log(d$cost.per.sq.yd*d$sq.yd))
# 
# # Damn, nothing seems to model the cliffs correctly




###  Model the Pavement decisions over 20 years ###
# dm <- d # dm = data model
# dm <- subset(dm, select = c(FID, Functional, STREETNAME, OCI, sq.yd)) #OR

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



# f(pave) = backlog, Total Cost to Pave, Benefit-To-Cost Ratio
myfunction <- function(pave){
  d$OCI <- PCIf(d$est.years)
  cost <- Costf(d$OCI, d$Functional, d$sq.yd)
  backlog <- sum(cost)
  d$Age.a <- ifelse(pave == 1, 1, 1 + d$est.years)
  d$OCI.a <- PCIf(d$Age.a)
  cost.to.pave.a <- ifelse(pave == 1, Costf(d$OCI,d$Functional, d$sq.yd),0)
  t.cost.to.pave.a <- sum(cost.to.pave.a)
  cost.a <- Costf(d$OCI.a, d$Functional, d$sq.yd)
  backlog.a <- sum(cost.a)
  benefit.to.cost <- (backlog - backlog.a)/t.cost.to.pave.a 
  output <- list(backlog.a, t.cost.to.pave.a, benefit.to.cost)
  return(output)
}


d$pave <- sample(c(0,1), 573, replace = TRUE) #random

d$pave <- ifelse(d$OCI < 25, 1,0) # all the worst

d$pave <- (ifelse((d$OCI >= 68) & (d$OCI < 88), 1,
                 ifelse((d$OCI >= 47) & (d$OCI < 68), 1,
                        ifelse(d$OCI == 47, 1, 0)))) # Top tiers

                 

myfunction(d$pave)





# f(pave) = backlog, Total Cost to Pave, Benefit-To-Cost Ratio
myfunction <- function(pave, pave.b){
  d$OCI <- PCIf(d$est.years)
  cost <- Costf(d$OCI, d$Functional, d$sq.yd)
  backlog <- sum(cost)
  d$Age.a <- ifelse(pave == 1, 1, 1 + d$est.years)
  d$OCI.a <- PCIf(d$Age.a)
  cost.to.pave.a <- ifelse(pave == 1, Costf(d$OCI,d$Functional, d$sq.yd),0)
  t.cost.to.pave.a <- sum(cost.to.pave.a)
  cost.a <- Costf(d$OCI.a, d$Functional, d$sq.yd)
  backlog.a <- sum(cost.a)
  d$Age.b <- ifelse(pave.b == 1, 1, 1 + d$Age.a)
  d$OCI.b <- PCIf(d$Age.b)
  cost.to.pave.b <- ifelse(pave.b == 1, Costf(d$OCI,d$Functional, d$sq.yd),0)
  t.cost.to.pave.b <- sum(cost.to.pave.b)
  cost.b <- Costf(d$OCI.b, d$Functional, d$sq.yd)
  backlog.b <- sum(cost.b)
  benefit.to.cost <- (backlog - backlog.b)/(t.cost.to.pave.a + t.cost.to.pave.b)
  output <- list(backlog.b, benefit.to.cost)
  return(output)
}


d$pave <- ifelse(d$OCI < 25, 1,0) # all the worst

d$pave.b <- (ifelse((d$OCI >= 68) & (d$OCI < 88), 1,
                  ifelse((d$OCI >= 47) & (d$OCI < 68), 1,
                         ifelse(d$OCI == 47, 1, 0)))) # Top tiers



myfunction(d$pave, d$pave.b)




# f(pave) = backlog, Total Cost to Pave, Benefit-To-Cost Ratio
myfunction <- function(pave, pave.b, pave.c){
  d$OCI <- PCIf(d$est.years)
  cost <- Costf(d$OCI, d$Functional, d$sq.yd)
  backlog <- sum(cost)
  d$Age.a <- ifelse(pave == 1, 1, 1 + d$est.years)
  d$OCI.a <- PCIf(d$Age.a)
  cost.to.pave.a <- ifelse(pave == 1, Costf(d$OCI,d$Functional, d$sq.yd),0)
  t.cost.to.pave.a <- sum(cost.to.pave.a)
  cost.a <- Costf(d$OCI.a, d$Functional, d$sq.yd)
  backlog.a <- sum(cost.a)
  d$Age.b <- ifelse(pave.b == 1, 1, 1 + d$Age.a)
  d$OCI.b <- PCIf(d$Age.b)
  cost.to.pave.b <- ifelse(pave.b == 1, Costf(d$OCI,d$Functional, d$sq.yd),0)
  t.cost.to.pave.b <- sum(cost.to.pave.b)
  cost.b <- Costf(d$OCI.b, d$Functional, d$sq.yd)
  backlog.b <- sum(cost.b)
  d$Age.c <- ifelse(d$pave.c == 1, 1, 2 + d$Age.a)
  d$OCI.c <- PCIf(d$Age.c)
  cost.to.pave.c <- ifelse(pave.c == 1, Costf(d$OCI,d$Functional, d$sq.yd),0)
  t.cost.to.pave.c <- sum(cost.to.pave.c)
  cost.c <- Costf(d$OCI.c, d$Functional, d$sq.yd)
  backlog.c <- sum(cost.c)
  benefit.to.cost <- (backlog - backlog.c)/(t.cost.to.pave.a + t.cost.to.pave.b + t.cost.to.pave.c)
  output <- list(backlog.b, benefit.to.cost)
  return(output)
}


d$pave <- ifelse(d$OCI < 25, 1,0) # all the worst

d$pave.b <- (ifelse((d$OCI >= 68) & (d$OCI < 88), 1,
                    ifelse((d$OCI >= 47) & (d$OCI < 68), 1,
                           ifelse(d$OCI == 47, 1, 0)))) # Top tiers

d$pave.c <- (ifelse((d$OCI >= 68) & (d$OCI < 88), 1,
                    ifelse((d$OCI >= 47) & (d$OCI < 68), 1,
                           ifelse(d$OCI == 47, 1, 0)))) # Top tiers



myfunction(d$pave, d$pave.b, d$pave.c.)










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

                            
                                   

