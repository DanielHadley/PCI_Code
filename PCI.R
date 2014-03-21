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
my.df$sq.ft <- my.df$PavementWi * my.df$Length # Sq. Feet
my.df$sq.yd <- my.df$sq.ft * 0.111111 # Sq. Yards
my.df$total.pci <- my.df$sq.yd * my.df$OCI # Sum PCI
my.df$ideal.pci <- my.df$sq.yd * 95 # Ideal sum PCI
my.df$delta.pci <- my.df$ideal.pci - my.df$total.pci #Difference
my.df$delta.over.cost <- my.df$delta.pci / my.df$ExtendedCo #One cost measure
aggregate(delta.over.cost ~ PlanActivi, my.df, mean ) # Crack Seal is crazy cost efficient

my.df$cost.per.sq.yd <- my.df$ExtendedCo / my.df$sq.yd


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


# test
plot(log(my.df$ExtendedCo), my.df$OCI)
plot(my.df$delta.over.cost, my.df$OCI) # checking cost-effectiveness
fit <- lm(my.df$delta.over.cost, my.df$OCI)

# To see where the bands begin and end
aggregate(OCI ~ PlanActivi, my.df, mean )
aggregate(OCI ~ PlanActivi, my.df, min )
aggregate(OCI ~ PlanActivi, my.df, max )

# Now model cost as a f(PCI) using logical tests: 
# this averages differences between collectors and arterials
my.df$cost.per.sq.yd.boolean <- ifelse((my.df$OCI > 67) & (my.df$OCI < 89), 1.8,
                            ifelse((my.df$OCI > 44) & (my.df$OCI < 67), 18.50,
                            ifelse((my.df$OCI > 23) & (my.df$OCI < 44), 83.50,
                                   ifelse((my.df$OCI > 0) & (my.df$OCI < 23), 150,
                                          ifelse(my.df$OCI > 89, 0, 360)))))



# Now model the cost degredation as a f(PCI) using a smooth curve
# This smooths out all of the "cliffs" from the difference maintenance bands, 
# but it approximates how quickly costs escalate as a f of PCI
# fit{{130, 0}, {100, 0}, {89, 0}, {95, 0}, {78, 1.8}, {57, 18.5}, {36, 83.5}, {18, 150}, {0, 150}
# I think this could be done much better in R
# x <- c(100, 89, 95, 78, 57, 36, 18, 0) # PCI
# y <- c(0.01, 0.01, 0.01, 1.8, 18.5, 83.5, 150, 150) #Cost 
# d <- data.frame(y, x) 
# I think one possible specification would be a cubic linear model
# y.hat <- predict(lm(y~x+I(x^2)+I(x^3), data=d)) 
my.df$cost <- 0.0141196 * my.df$OCI^2 -3.165 * my.df$OCI + 170.95
plot(my.df$cost, my.df$OCI)

# Test
plot(my.df$cost , my.df$ExtendedCo)
fit <- lm(my.df$ExtendedCo ~ my.df$cost , data=my.df)
summary(fit) # show results


####  Better yet, use the empirical distribution ####
# http://davetang.org/muse/2013/05/09/on-curve-fitting/
y = my.df$cost.per.sq.yd 
x = my.df$OCI
plot(x,y)

#fit first degree polynomial equation:
fit  <- lm(y~x)
#second degree
fit2 <- lm(y~poly(x,2,raw=TRUE))
#third degree
fit3 <- lm(y~poly(x,3,raw=TRUE))
#fourth degree
fit4 <- lm(y~poly(x,4,raw=TRUE))
#generate range of 50 numbers starting from 30 and ending at 160
xx <- seq(0,160, length=50)
plot(x,y,pch=19,ylim=c(0,150))
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")
lines(xx, predict(fit3, data.frame(x=xx)), col="blue")
lines(xx, predict(fit4, data.frame(x=xx)), col="purple") ## This looks like the best fit

# Now model it based on the coefficients from fit4
#y=e + dx + cx^2 + bx^3 + ax^4 
coef(fit4)

my.df$cost.model <- 1.351524e+02 + (1.845730e+00 * my.df$OCI) +  
  (-1.630135e-01 * my.df$OCI^2) + 
  (2.195987e-03 * my.df$OCI^3) + 
  (-8.857414e-06 * my.df$OCI^4) 


plot(my.df$cost.model,my.df$cost.per.sq.yd)

fit <- lm(my.df$cost.model,my.df$cost.per.sq.yd)
summary(fit) # show results

hist(my.df$cost.model)

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

                            
                                   

