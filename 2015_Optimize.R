#### Read Me ####

# This code analyzes data from FST, the City's pavement consultant
# It models the cost and PCI degredation in order to optimize pavement decisions
# OCI value = PCI.  Note these scores are from November 2012.
# Created By Daniel Hadley


library(tidyr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(Rglpk)
# http://stackoverflow.com/questions/15147398/optimize-value-with-linear-or-non-linear-constraints-in-r


# Import data ####
setwd("C:/Users/dhadley/Documents/GitHub/PCI_Code")
setwd ("~/Documents/Git/PCI_Code") #at home
d <- read.csv("PCI.csv")

# Create new variables #### 
d$sq.ft <- d$PavementWi * d$Length # Sq. Feet
d$sq.yd <- d$sq.ft * 0.111111 # Sq. Yards


#### Functions needed for the model: ####

# f(OldPCI) = NewPCI
# This takes the current PCI and calculates the difference between that and the PCI post treatment
# This is essentially the "value" added by paving the street
# According to Bill from FST Pretty standard.  
# 25% for cracksealing/routine maintenance and 50% for patch/paving/microsurfacing/preventive maintenance.                                                                                                                                                                                                                             

NewOCIf <- function(OldOCI){ 
  NewOCI <- ifelse((OldOCI >= 68) & (OldOCI < 88), 
                  (OldOCI + (.25 * OldOCI)),
                  ifelse((OldOCI >= 47) & (OldOCI < 68), 
                         (OldOCI + (.50 * OldOCI)),
                         ifelse((OldOCI >= 25) & (OldOCI < 47),
                                (OldOCI + (.50 * OldOCI)),
                                ifelse((OldOCI < 25),
                                       100,
                                       OldOCI))))
  NewOCI <- ifelse(NewOCI > 100, 100, NewOCI)
  return(NewOCI)
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



#### Optimize ####

d <- d %>%
  mutate(PCI = OCI,
         NewPCI = NewOCIf(OCI),
         value = (NewPCI - OCI) * sq.yd, # the value to the city 
         cost = Costf(OCI, Functional, sq.yd)) %>% # cost to pave the street
  filter(value > 0) # drop the streets with no plan activity


# subset to the most important variables
d <- d %>%
  select(Functional, STREETNAME, PlanActivi, PCI, NewPCI, value, cost)


# Set up the problem in Rglpk
# number of variables
num.streets <- length(d$STREETNAME) 

# objective:
f <- d$value
# the variables are booleans:
# Either yes we pave the street, or no we don't
var.types <- rep("B", num.streets)
# the constraints
# Leaving out flex for now
A <- rbind(as.numeric(d$Functional == "CO - Collector"), # num collectors
           as.numeric(d$PlanActivi == "(BR) Recon/Reclaim Local w/ramps"), # num full-depth
           d$cost)                    # total budget

dir <- c(">=",
         ">=",
         "<=")

b <- c(32,
       3,
       3000000)


sol <- Rglpk_solve_LP(obj = f, mat = A, dir = dir, rhs = b,
                      types = var.types, max = TRUE)
sol

d$STREETNAME[sol$solution == 1]

toPave <- d[which(sol$solution == 1),]


