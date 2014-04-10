# This code cleans and analyzes data from FST, the pavement consultant
# OCI value = PCI.  Note these scores are from November 2012.
# This is where I am storing some of the analysis on the PCI data to keep my model more streamlined
# Created By Daniel Hadley


setwd("K:/Somerstat/Common/Data/2014 StreetStat/PCI_Code")
# setwd ("~/Documents/Git/PCI_Code") #at home
d <- read.csv("PCI.csv")

# Create new variables 
d$sq.ft <- d$PavementWi * d$Length # Sq. Feet
d$sq.yd <- d$sq.ft * 0.111111 # Sq. Yards
d$total.pci <- d$sq.yd * d$OCI # Sum PCI
d$ideal.pci <- d$sq.yd * 100 # Ideal sum PCI
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
# PCI = 100 - (106/((ln(79/AGE))^(1/.48))) # collector
# PCI = 100 - (109/((ln(88/AGE))^(1/.58))) # arterial
# PCI = 100 - (110/((ln(97/AGE))^(1/.61))) # residential

d$est.years <- 79*(2.71828^(-9.37879/(100-d$OCI)^0.48))
plot(d$est.years, d$OCI)

d$last.paved <- 2012 - d$est.years
hist(d$last.paved) # I'm skeptical there are streets we have not paved since the 80s

d$PCI2 = 100 - (110/((log(97/d$est.years))^(1/.61)))
d$PCI3 = 100 - (109/((log(88/d$est.years))^(1/.58)))
plot(d$est.years, d$PCI3)

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



# Visualize ####
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



