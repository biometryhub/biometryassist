# Author:
# Organisation:
# Date:

######################################################################
# Analysis of <variable or project>
######################################################################

# Design:
# Response variable:
# Structural Component:
# Explanatory component:
# Experimental Unit:
# Observational Unit:
# Residual:

# Load required packages
library(tidyverse)
library(biometryassist)

# Read in data
dat <- read.csv("your_data_here.csv")

# Check data structure
str(dat)

# Change the required columns to factors for analysis
# Structural columns (row, column, block etc) and treatment columns should be factors
dat <- dat %>% mutate(across(c(1:5, 7:8), factor))
# dat <- dat %>% mutate(across(c(Row, Column, Blocks, Wplots, Subplots, Nitrogen, Variety), factor)) # Equivalently

# Explore the data as necessary
summary_graph(dat, response, exp_var = c(var1, var2), resp_units = "Y axis units")

# fitting the model
dat.aov <- aov(response ~ structural + treatments, data = dat) # fitting the model

# Check residual plots for appropriate model before moving on
resplot(dat.aov)

#summary(dat.aov)
anova(dat.aov)

# Prediction
pred.out <- multiple_comparisons(model.obj = dat.aov, classify = "treatments")
pred.out

# Graph the predicted values
autoplot(pred.out) +
    labs(x = "Treatments", y = "Predicted Response")
