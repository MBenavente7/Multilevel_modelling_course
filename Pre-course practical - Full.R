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

# In this practical we shall work with the hedonism data for UK, France, and 
# Germany data and we shall replicate all the analyses presented in the lecture.

# Load required packages
library(foreign)  # Importing Stata datasets
library(ggplot2)  # Graphs
# You will need to install these packages if not already installed.

# Set the working directory
# From RStudio Session menu: Set Working Directory > To Source File Location
# Or, edit and uncomment the following command:
# setwd("TYPE THE DIRECTORY PATH WHERE THE R SCRIPT AND DATA ARE SAVED HERE")

# Load the hedonism3.dta dataset and assign it to the object mydata
mydata <- read.dta("hedonism3.dta")

# Return the first part of the data frame
head(mydata)
# The head() function returns the first six rows of the data frame.

# Make a new variable 'gender' with value labels to be used in plots
mydata$gender <- mydata$female + 1
# We want 'gender' to be coded 1 = male and 2 = female. So we have added 1 to
# 'female' to do this.
mydata$gender <- factor(mydata$gender,
                        levels = c(1, 2),
                        labels = c("Male", "Female"))
# Here we attach value labels "Male" and "Female" to the numeric values 1 and 2.
head(mydata)


#------------------------------------------------------------------------------
# DATA STRUCTURE AND DESCRIPTIVE STATISTICS
#------------------------------------------------------------------------------

# Describe the variables
str(mydata)
# There are 4,905 observations (respondents) in the sample on 6 variables.

# Summarize all variables in the data
summary(mydata)
# Note that the summary() function does not report the SDs of the variables.
sd(mydata$hedonism)
# Hedonism scores have a mean of -0.16 and a standard deviation of 0.97. The 
# scores range from -3.76 to 2.91.

# List the variables for the first 5 observations
head(mydata, n = 5)
# The first row of data corresponds to a 25-year-old male respondent from 
# Germany. The respondent has a relatively high hedonism score of 1.55 (recall 
# that hedonism is standardised and so this respondent has a score one-and-a
# -half standard deviations above average).

# Histogram of hedonism scores
ggplot(mydata, aes(x = hedonism)) +
  geom_histogram(aes(y = ..density..), colour = "grey32", fill = "grey") +
  labs(x = "Hedonism score")
# We will use the ggplot function to create our plots. In our aesthetic mapping
# -aes()- we set the the x-axis to hedonism scores and the y axis to the density
# of hedonism scores. The dots within the "..density.." expression indicate that
# ggplot() needs to calculate density from the hedonism scores rather than
# accessing a potential variable named 'density' from the 'mydata' data frame. The
# colour = "grey32" argument specifies the colour of the outline of the histogram
# bins. The fill = "grey" argument specifies the colour of the contents of the
# histogram bins. The labs(x = "Hedonism score") argument adds labels to the plot,
# here only for the x-axis.

# Add a normal curve and a kernel density smoothed histogram line to the plot
ggplot(mydata, aes(x = hedonism)) + 
  geom_histogram(aes(y = ..density..), colour = "grey32", fill = "grey") +
  geom_density(aes(linetype = "density")) +
  stat_function(aes(linetype = "normal"),
                fun = dnorm, 
                args = list(
                  mean = mean(mydata$hedonism), 
                  sd = sd(mydata$hedonism))
                ) +
  scale_linetype_manual(guide = "legend",
                        values = c( "normal" = "solid",
                                    "density" = "dashed"), 
                        labels = c("normal" = "Normal curve", 
                                   "density" = "Kernel density"),
                        name = NULL) + 
  labs(x = "Hedonism score")
# Here we overlay the histogram with kernel density and normal curve lines. The
# geom_density() function adds the kernel density, the stat_function() function
# adds the normal curve. Specifically, stat_function() draws a normal curve with
# parameters 'mean' and 'sd' set equal to the mean and SD of hedonism scores.
# Including the argument 'linetype' in the aes() function for the geom_density()
# and stat_function() functions allows us to format and label the lines in the
# scale_linetype_manual() function. We format the normal curve as a solid line 
# and label it "Normal curve". We format the density curve as a dashed line and
# label it "Kernel density"

# Histogram of age
ggplot(mydata, aes(x = age)) + 
  geom_histogram(aes(y = ..density..), colour = "grey32", fill = "grey") +
  geom_density(aes(linetype = "density")) +
  stat_function(aes(linetype = "normal"), 
                fun = dnorm, 
                args = list(mean = mean(mydata$age), 
                            sd = sd(mydata$age))) +
  scale_linetype_manual(guide = "legend",
                        values = c("normal" = "solid",
                                   "density" = "dashed"),
                        labels = c("normal" = "Normal curve",
                                   "density" = "Kernel density"),
                        name = NULL) + 
  labs(x = "Age in years")

# Univariate descriptive statistics
summary(mydata[, c("hedonism", "age", "female")])
# We request summary statistics for just 'hedonism', 'age', and 'female'. We do 
# this by specifying the columns of the 'mydata' data frame that we want.

# Bar chart of percent of respondents by gender
ggplot(mydata, aes(x = gender)) + 
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Percentage")
# We use the (..count..)/sum(..count..) expression to define the y axis as a
# fraction. Specifically, it will count the number of observations within each
# gender and divide it by the total number of observations. As before we use the
# ".." to indicate that ggplot() has to calculate the y-axis from the x-axis
# variable, here gender. We use the scale_y_continuous() function to format the
# y-axis labels to be presented as percentages by using the percent_format()
# function from the scales package. We use the accuracy = 1 option to display
# y-axis labels as whole numbers (e.g., 50% as opposed to 50.0%).

# Bar chart of percent of respondents by country
ggplot(mydata, aes(x = country)) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Percentage")

# One-way tabulation of gender
table(mydata$gender)
# Here we specify mydata$gender to restrict the table to tabulate only the
# 'gender' variable within the 'mydata' data frame.

# One-way tabulation of country
table(mydata$country)

# Scatterplot of hedonism against age
ggplot(mydata, aes(x = age, y = hedonism)) + 
  geom_point(alpha = 0.2)
# We set alpha (colour transparency) to 0.2 to better see the overlap of 
# hedonism scores. 

# Pearson correlation between hedonism and age
cor.test(mydata$hedonism, mydata$age)

# Bar chart of mean hedonism score by gender
ggplot(mydata, aes(x = gender, y = hedonism)) +
  geom_bar(stat = "summary", fun = "mean") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-0.4, 0.2), 
                     oob = scales::oob_keep) +
  labs(y = "Mean of hedonism")
# We use the geom_bar() function with the stat = "summary" and fun = "mean"
# arguments to plot the mean of the y-axis variable 'hedonism' by the categories 
# of the x-axis variable 'gender'. We use the geom_hline() function to plot a
# horizontal line at zero. We set the scale of the y-axis through the
# scale_y_continuous() function. Specifically, we set the lower and upper limits 
# to -0.4 and 0.2. We set the 'oob' argument to 'oob_keep' to prevent the
# underlying observations from potentially being discarded by altering these
# scale limits; this is especially relevant when setting limits to a continuous
# axis with geom_bar as is the case here. If we did not include this option then
# all respondents with hedonism scores outside (-0.4, 0.2) would be dropped
# prior to the construction of the plot and this would lead to very different and
# misleading results.

# Bar chart of mean hedonism score by country
ggplot(mydata, aes(x = country, y = hedonism)) +
  geom_bar(stat = "summary", fun = "mean") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-0.4, 0.2),
                     breaks = seq(from = -4, to = 2) / 10 ,
                     oob    = scales::oob_keep) +
  labs(y = "Mean of hedonism")
# Here we use the breaks() function to specify the ticks on the y-axis.
# Specifically, we request ticks to be show from -0.4 to 0.2 in increments of
# 0.1.

# Summary statistics for hedonism by gender
aggregate(hedonism ~ gender,
          data = mydata, 
          FUN = function(x) c(n = length(x),
                              mean = mean(x),
                              sd = sd(x), 
                              min = min(x),
                              max = max(x)))
# We use the aggregate() function to calculate group statistics. The 'hedonism ~
# gender' expression tells aggregate to group the hedonism data by gender. The
# 'FUN' argument takes a vector of functions that will take the grouped data as
# their input. Here we request the number of respondents, mean, SD, min and max
# value of hedonism for each gender.

# Summary statistics for hedonism by country
aggregate(hedonism ~ country,
          data = mydata, 
          FUN = function(x) c(n = length(x), 
                              mean = mean(x),
                              sd = sd(x), 
                              min  = min(x),
                              max  = max(x)))



#------------------------------------------------------------------------------
# MODEL 1
#------------------------------------------------------------------------------

# Fit Model 1
m1 <- lm(hedonism ~ age, data = mydata)
# The lm() function is used to fit linear regression models. Models are fit by
# ordinary least squares. The notation specifies a linear regression of the
# dependent variable 'hedonism' on the predictor variable 'age'. We assign the
# model results to a new object which we name 'm1'.

# Produce a summary of the 'm1' model results
summary(m1)
# The estimated mean hedonism score across respondents is 0.682 for individuals
# aged zero. Each one year increase in age is predicted to lower hedonism by
# 0.018 units. A 10-year increase in age therefore corresponds to a 0.18 drop in
# hedonism (or is this a birth cohort effect?).
#
# The standard errors are reported after each parameter estimate. For example,
# the standard error for age is 0.001. The standard error allows us to test
# whether the slope coefficient on age is statistically significant. Dividing
# the parameter estimate by its standard error gives a t-ratio of -23.76 which
# is substantially larger than the critical value of 1.96 for testing at the 5%
# level of significance for this large sample. We therefore reject the null
# hypothesis that age has no effect on hedonism. Note that when using the lm()
# function in R you will find the ratio under "t value" in the summary.
#
# Note how the summary only reports the residual SD "Residual standard error:
# 0.9231" rather than the residual variance.

# Compute the residual variance
sigma(m1)^2
# The sigma() function extracts the residual standard deviation from model 'm1'.
# We then raise this to the power 2 to square it to give the residual variance.

# Compute 95% confidence intervals for regression coefficients
confint(m1)
# An equivalent way to test the null hypothesis that age has no effect on
# hedonism is to calculate the 95% confidence interval for age and to examine
# whether a value of zero lies within this range. We see from the output that
# The value of zero lies well outside the range of the 95% confidence interval
# (-0.019,-0.016) and so once again we reject the null hypothesis that age has
# no effect on hedonism.

# To visualise the fitted model, first calculate the predicted values
mydata$predm1 <- predict(m1)
# We store the predicted hedonism scores for each respondent in a new variable
# which we call predm1 and save to the data frame 'mydata'. These predicted scores
# are based on the model results and respondents' ages.
head(mydata)

# Produce a scatter plot of hedonism against age, and add a line of best fit
ggplot(mydata, aes(x = age)) +
  geom_point(aes(y = hedonism, colour = "scatter"), alpha = 0.8) +
  geom_line(aes(y = predm1, colour = "fitted"), size = 1) +
  scale_colour_manual(guide  = "legend", 
                      values = c("scatter" = "grey",
                                 "fitted" = "black"), 
                      labels = c("scatter" = "hedonism score",
                                 "fitted" = "Fitted line"),
                      name   = NULL)
# Here we use the geom_line() function to add a line plot to the scatter.
# Specifically, the predicted hedonism scores stored in 'prem1'. We use
# scale_colour_manual() to add custom colours and labels. Specifically, we 
# specify the scaterplot points to be grey and the line to be black.
#
# The line of best fit clearly shows the negative association between hedonism
# and age.

# Predicted hedonism score for a 40-year old
summary(m1)
coef(m1)[["(Intercept)"]] + coef(m1)[["age"]] * 40
# We use the coef() function to extract coefficients from our model 'm1'. We
# reference the coefficients by their names as presented in the summary() output.

# Calculate the predicted residuals
mydata$m1resid <- resid(m1)

# Check linearity assumption
ggplot(mydata, aes(x = age)) +
  geom_point(aes(y = hedonism, colour = "scatter"), alpha = 0.8) +
  geom_line(aes(y = predm1, colour = "fitted"), size = 1) +
  geom_smooth(aes(y = hedonism, colour = "loess"),
              method = "loess",
              se = FALSE,
              size = 0.5) +
  scale_colour_manual(guide  = "legend", 
                      values = c("scatter" = "grey",
                                 "fitted" = "black", 
                                 "loess" = "blue"), 
                      labels = c("scatter"="hedonism score",
                                 "fitted" = "Fitted line",
                                 "loess" = "Loess"),
                      name   = NULL)
# We use the geom_smooth() function to plot a loess line through the scatterplot.
# The line is convex to the x-axis suggesting some non-linearity in the
# relationship between hedonism and age. One might enter age-squared into the
# model to explore this further, but we do not pursue this here.

# Plot a histogram of the predicted residuals
ggplot(mydata, aes(x = m1resid)) + 
  geom_histogram(aes(y = ..density..), colour = "grey32", fill = "grey") +
  geom_density(aes(linetype = "density")) +
  stat_function(aes(linetype = "normal"), 
                fun = dnorm, 
                args = list(
                  mean = mean(mydata$m1resid),
                  sd   = sd(mydata$m1resid))) +
  scale_linetype_manual(guide = "legend",
                        values = c("normal" = "solid",
                                   "density" = "dashed"), 
                        labels = c("normal" = "Normal curve",
                                   "density" = "Kernel density"),
                        name = NULL) +
  labs(x = "Residuals", y = "Density")

# Plot a quantile-quantile plot of the predicted residuals
ggplot(mydata, aes(sample = m1resid)) +
  stat_qq() + 
  stat_qq_line() +
  labs(x = "Inverse normal", y = "Standardized values of residuals")
# The stat_qq() function requests the quantile-quantile plot. The stat_qq_line()
# function adds the 45 degree line.
#
# Residuals which are normally distributed are expected to lie in a straight
# line and this is the case in the plot shown here.

# Plot the predicted standardised residuals against age
ggplot(mydata,aes(x = age)) +
  geom_point(aes(y = scale(m1resid), colour = "scatter"), alpha = 0.7) +
  geom_line(aes(y = 0, colour = "zero"), size = 1) +
  scale_colour_manual(guide  = "legend", 
                      values = c("scatter" = "grey",
                                 "zero" = "black"), 
                      labels = c("scatter" = "hedonism score",
                                 "zero" = "Fitted line"),
                      name   = NULL) +
  labs(y = "Predicted residuals")
# There is no suggestion that the variance of the residuals changes with the 
# predicted response scores and so the homoskedasticity assumption appears 
# reasonable.



#------------------------------------------------------------------------------
# MODEL 2
#------------------------------------------------------------------------------
  
# Fit Model 2
m2 <- lm(hedonism ~ female, data = mydata)
summary(m2)
# The predicted score for males is -0.074. The predicted score for females is
# -0.232 (= -0.074 + (-0.158)). The difference between women and men is -0.158. 
confint(m2)
# This gender difference has a t-ratio of -5.68 and a p-value smaller 
# than 0.001 while the 95% confidence interval is (-0.212,-0.103). We therefore 
# reject the null hypothesis of there being no gender difference in hedonism 
# scores.
sigma(m2)^2



#-------------------------------------------------------------------------------
# MODEL 3
#-------------------------------------------------------------------------------
  
# Generate a series of dummy variables for the countries
mydata$germany <- as.integer(mydata$country == "Germany")
mydata$france  <- as.integer(mydata$country == "France")
mydata$uk      <- as.integer(mydata$country == "UK")
# The expression within as.integer() returns a vector of FALSE AND TRUE values.
# as.integer() then converts this into a vector of zeros and ones where 0 = FALSE
# and 1 = TRUE.

# Fit Model 3
m3 <- lm(hedonism ~ germany + france, data = mydata)
summary(m3)
sigma(m3)^2

# Calculate the predicted scores for UK, German and French respondents
coef(m3)[["(Intercept)"]]
coef(m3)[["(Intercept)"]] + coef(m3)[[("germany")]]
coef(m3)[["(Intercept)"]] + coef(m3)[[("france")]]
# The predicted score for UK respondents is -0.397. The predicted score for
# German respondents is -0.140 (= -0.397 + 0.257). The predicted score for
# French respondents is 0.138 (= -0.397 + 0.535). French respondents, therefore,
# appear the most hedonistic, UK respondents the least hedonistic. The t-ratios
# for both the Germany and France coefficients are much larger than 1.96 and so 
# both contrasts with the UK are statistically significant.

# Calculate the predicted France - Germany difference
coef(m3)[[("france")]] - coef(m3)[[("germany")]]

# Refit the model using factors instead of indicator variables notation
mydata$country <- relevel(mydata$country, ref = "UK")
lm(hedonism ~ country, data = mydata)
# R can use factor class data as categorical variables in regression. The
# reference category can be set with the relevel() function. The relevel() 
# function reorders the levels of the factor so that the UK appears first and is
# therefore automatically taken as the reference category when we fit the model.
summary(m3)
sigma(m3)^2



#------------------------------------------------------------------------------
# MODEL 4
#------------------------------------------------------------------------------

# Fit Model 4
m4 <- lm(hedonism ~ age + female, data = mydata)
summary(m4)
sigma(m4)^2
# The model results show that a one-year increase in age, holding gender
# constant, decreases hedonism by 0.018 units and that the difference between
# women and men, holding age constant, is -0.161. These results are very similar
# to those obtained from the simpler models presented above. This suggests that
# the variables are actually rather independent of one another.

# Pearson correlation between age and female
cor.test(mydata$age, mydata$female)

# Display results for models 1, 2 and 4
summary(m1)$coefficients
summary(m2)$coefficients
summary(m4)$coefficients

# Predict hedonism scores
mydata$predm4 <- predict(m4)

# Plot the gender specific hedonism scores against age
ggplot(mydata, aes(x = age)) +
  geom_point(aes(y = hedonism), alpha = 0.2) +
  geom_line(aes(y = predm4, colour = gender), size = 1) +
  labs(x = "Age in years", y = "Hedonism")
# By setting colour to 'gender' ggplot automatically assigns different colours
# to different genders (levels in factor class data).
#
# The plot shows the male and female lines are constrained to have the same
# slopes.
 


#-------------------------------------------------------------------------------
# MODEL 5
#-------------------------------------------------------------------------------

# Generate the interaction between age and gender
mydata$ageXfemale = mydata$age * mydata$female
head(mydata)

# Fit Model 5
m5 <- lm(hedonism ~ age + female + ageXfemale, data = mydata)
summary(m5)
sigma(m5)^2
# We see that the t-ratio is 0.77 and so the interaction term is not 
# statistically significant at the 5% level (p = 0.443). Despite this we will 
# proceed to plot and interpret this term for pedagogic purposes.
#
# The interaction term is positive and so hedonism scores decrease more rapidly 
# for men with age than they do for women. Put differently, the gender gap in 
# hedonism narrows with age.

# Predict hedonism scores
mydata$predm5 <- predict(m5)

# Plot the gender specific hedonism scores against age
ggplot(mydata, aes(x = age)) +
  geom_point(aes(y = hedonism), alpha = 0.2) +
  geom_line(aes(y = predm5, colour = gender), size = 1) +
  labs(x = "Age in years", y = "Hedonism")
# The plot shows the male and female lines to have similar slopes and to lie
# very close to one another. The interaction term therefore appears
# substantively small and we can say that the relationship between hedonism and
# age for men appears to be broadly similar to the relationship between hedonism
# and age for women.

# Refit the model using a different notation
m5 <- lm(hedonism ~ age*female, data = mydata)
# The expression "age*female" tells the lm() function to calculate the
# coefficients for the interaction term and the individual terms as well.
summary(m5)
sigma(m5)^2



#----------------------------------------------------------------------------
