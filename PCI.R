# Dan,
# 
# See attached.  OCI value = PCI.  Note these scores are from November 2012.  You will recognize some streets have already been done - should have score of 100.  We typically freshen this data annually and do have an existing contract to update your pavement management system but are focusing our contract hours/budget towards finalizing the City's ADA Transition Plan with respect to the public right of way. 
# 
# I will forward you sidewalk and ramp shapes under another email.  As you will hear from Melissa, Stan, Besty Allen, and David Shapiro - as part of the City's transition plan the City needs to commit a reasonable portion of public works construction budget towards ADA improvements that entail sidewalk, ramp and APS work annually and report these ongoing improvements to the FHWA.  
# 
# I'm working for formulate some suggestions based on Cambridge and Boston's commitments, in the meantime you should present a visual of sidewalk/ramp conditions to the Mayor so he may start to digest this information.
# 
# Thanks,
# Bill

setwd("K:/Somerstat/Common/Data/2014 StreetStat/PCI_Code")
my.df <- read.csv("PCI.csv")

# Create new variables 
my.df$sq.ft <- my.df$PavementWi * my.df$Length
my.df$sq.yd <- my.df$sq.ft * 0.111111
my.df$total.pci <- my.df$sq.yd * my.df$OCI
my.df$ideal.pci <- my.df$sq.yd * 95
my.df$delta.pci <- my.df$ideal.pci - my.df$total.pci
my.df$delta.over.cost <- my.df$delta.pci / my.df$ExtendedCo
aggregate(delta.over.cost ~ PlanActivi, my.df, mean ) # Crack Seal is crazy cost efficient


# subset of the ones you would do the 1st year
biggest.bang <- my.df[which(my.df$delta.over.cost > 2),]
sum(biggest.bang$ExtendedCo)

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
p <- qplot(PlanActivi, weight = Length, data = my.df, geom = "bar", alpha=I(.7), main="Activity by Length", ylab="Col2 Count")
p + my.theme

p <- qplot(Treatmentb, weight = Length, data = my.df, geom = "bar", alpha=I(.7), main="Activity by Length", ylab="Col2 Count")
p + my.theme

p <- qplot(OCI, weight = Length, data = my.df, colour = "white", fill=Treatmentb, geom = "bar", main="PCI by Length", ylab="Length of Roadway")
p + my.theme


ag <- aggregate(OCI ~ PlanActivi, my.df, mean )
p <- qplot(PlanActivi, weight = OCI, data = ag, geom = "bar", alpha=I(.7), main="Activity by Avg PCI", ylab="PCI")
p + my.theme


# Here I model the age as a function of PCI based on the references below:
# It is an average of residential and arterial, but we can divide those later
# http://onlinepubs.trb.org/onlinepubs/conferences/2012/assetmgmt/presentations/Data-A-Ramirez-Flores-Chang-Albitres.pdf
# https://repository.tamu.edu/bitstream/handle/1969.1/ETD-TAMU-2009-05-317/DESHMUKH-THESIS.pdf?sequence=2
# http://www.mylongview.com/modules/showdocument.aspx?documentid=631
# PCI = 100 - (106/((ln(79/AGE))^(1/.48)))

my.df$est.years <- 79*(2.71828^(-9.37879/(100-my.df$OCI)^0.48))
plot(my.df$est.years, my.df$OCI)

my.df$last.paved <- 2012 - my.df$est.years
hist(my.df$last.paved) # I'm skeptical there are streets we have not paved since the 80s

# Subsets of the modeled data
curtatone <- my.df[which(my.df$last.paved > 2003),]
really.old <- my.df[which(my.df$last.paved < 1989),]


# test
plot(log(my.df$ExtendedCo), my.df$OCI)
plot(my.df$delta.over.cost, my.df$OCI) # checking cost-effectiveness
fit <- lm(my.df$delta.over.cost, my.df$OCI)

# To see where the bands begin and end
aggregate(OCI ~ PlanActivi, my.df, mean )
aggregate(OCI ~ PlanActivi, my.df, min )
aggregate(OCI ~ PlanActivi, my.df, max )

# Now model cost as a f(PCI): this averages differences between collectors and arterials
my.df$cost.per.sy <- ifelse((my.df$OCI > 67) & (my.df$OCI < 89), 1.8,
                            ifelse((my.df$OCI > 44) & (my.df$OCI < 67), 18.50,
                            ifelse((my.df$OCI > 23) & (my.df$OCI < 44), 83.50,
                                   ifelse((my.df$OCI > 0) & (my.df$OCI < 23), 150,
                                          ifelse(my.df$OCI > 89, 0, 360)))))
                            
                                   

