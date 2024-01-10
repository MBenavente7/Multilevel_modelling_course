#-------------------------------------------------------------------------------
# MULTILEVEL MODELLING - PRACTICALS
#-------------------------------------------------------------------------------
# REVIEW OF LINEAR REGRESSION - HEDONISM DATA FOR UK, FRANCE, AND GERMANY
#-------------------------------------------------------------------------------
# George Leckie
# Centre for Multilevel Modelling, University of Bristol
#
# Adapted for R by Tamas Novak
# Centre for Multilevel Modelling, University of Bristol
#-------------------------------------------------------------------------------

# Load required packages
library(foreign)  # Importing Stata datasets
# You will need to install these packages if not already installed.

# Set the working directory
# From RStudio Session menu: Set Working Directory > To Source File Location
# Or, edit and uncomment the following command:
# setwd("TYPE THE DIRECTORY PATH WHERE THE R SCRIPT AND DATA ARE SAVED HERE")
setwd("C:/Users/mb02582/Documents/PhD/3. Courses/Multilevel modelling/Multilevel_modelling_course/Data")

# Load the hedonism3.dta dataset
mydata <- read.dta("hedonism3.dta")

# Return the first part of the data frame
head(mydata)


#------------------------------------------------------------------------------
# MODEL 1
#------------------------------------------------------------------------------

# Fit Model 1
m1 <- lm(hedonism ~ 1 + age, data = mydata)
summary(m1)

# Compute the residual variance
sigma(m1)^2

# Compute 95% confidence intervals for regression coefficients
confint(m1)



#------------------------------------------------------------------------------
# MODEL 2
#------------------------------------------------------------------------------

# Fit Model 2
m2 <- lm(hedonism ~ 1 + female, data = mydata)
summary(m2)
sigma(m2)^2
confint(m2)



#-------------------------------------------------------------------------------
# MODEL 3
#-------------------------------------------------------------------------------

# Generate a series of dummy variables for the countries
mydata$germany <- as.integer(mydata$country == "Germany")
mydata$france  <- as.integer(mydata$country == "France")
mydata$uk      <- as.integer(mydata$country == "UK")
head(mydata)

# Fit Model 3
m3 <- lm(hedonism ~ 1 + germany + france, data = mydata)
summary(m3)
sigma(m3)^2
confint(m3)

# Refit the model using standard R notation
mydata$country <- relevel(mydata$country, ref="UK")
lm(hedonism ~ 1 + country, data = mydata)
summary(m3)
sigma(m3)^2
confint(m3)



#------------------------------------------------------------------------------
# MODEL 4
#------------------------------------------------------------------------------

# Fit Model 4
m4 <- lm(hedonism ~ 1 + age + female, data = mydata)
summary(m4)
sigma(m4)^2
confint(m4)



#-------------------------------------------------------------------------------
# MODEL 5
#-------------------------------------------------------------------------------

# Generate the interaction between age and gender
mydata$ageXfemale = mydata$age * mydata$female
head(mydata)

# Fit Model 5
m5 <- lm(hedonism ~ 1 + age + female + ageXfemale, data = mydata)
summary(m5)
sigma(m5)^2
confint(m5)



#----------------------------------------------------------------------------
