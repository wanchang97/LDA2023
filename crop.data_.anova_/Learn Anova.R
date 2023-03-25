## Learning the use of ANOVA

install.packages(c("ggplot2","ggpubr","tidyverse","broom","AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
# Step 1: Load the data into R
#We will use the same dataset for all of our examples in this walkthrough. The only difference between the different analyses is how many independent variables we include and in what combination we include them
crop.data <- read.csv("crop.data.csv",header=TRUE,colClasses = c("factor","factor","factor","numeric"))
# check if the data has been imported correctly
summary(crop.data)
# Step 2 Perform the ANOVA
# ANOVA tests whether any of the group means are different from the overall mean of the data by checking the variance of each individual group against the overall variance of the data. 
# If one or more groups falls outside the range of variation predicted by the null hypothesis (all group means are equal), then the test is statistically significant
# One-way ANOVA
# In the one-way ANOVA example
one.way <- aov(yield ~fertilizer,data = crop.data)
summary(one.way)
# The model summary first lists the  independent variables being tested in the model ( in this case we have "fertilizer") and the model residuals.
# All of the variation that is not explained by the independent variables is called residual variance.

# The columns are describing the independent variables and residuals
# Df column : degrees of freedom for independent variable (the number of levels is the variable minus 1)
#             degrees of freedom for residuals (the total number of observations 96 minus one and minus the number of levels in the independent variables)
# Sum Sq : sum of squares, a.k.a. the total variation between the group means and the overall mean
# Mean Sq:  mean of the sum of squares, Sum Sq/Df
# F value; test statistic from the F test. This is the mean square pf each independent variable divided by the mean square of the residuals.
# The larger the F value, the more likely it is the variation caused by the independent variable is true and not due to chance


# Pr(>F) : p value of the F-statistics. It shows how likely it is that the F value calculated from the test would have occurred if the null hypothesis of no differenc among group means were true
# The p value of the fertilizer variable is low (p<0.05)
# so it appears that the type of fertilizer used has a real impact on the final crop yield

# TWp-way ANOVA
two.way <- aov(yield~fertilizer+density,data=crop.data)
summary(two.way)

# Adding planting density to the model seems to have made the model better: 
# it reduced the residual variance the residual Sum Sq from 35.89 to 30.765 
# and both planting density  and fertilizer are statistically significant (p-value < 0.05)

# Adding interactions https://www.scribbr.com/statistics/anova-in-r/ 
