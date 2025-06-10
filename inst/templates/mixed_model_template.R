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
library(asreml)

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
dat <- dat %>% arrange(Column, Row)
dat.asr <- asreml(yield ~ Variety + Nitrogen + Variety:Nitrogen,
                  random = ~ Blocks + Blocks:Wplots,
                  residual = ~ id(Column):ar1(Row), data = dat)

# Check residual plots for appropriate model before moving on
resplot(dat.asr)

# Check Wald table - is there a difference between the treatment levels?
dat.ww <- wald(dat.asr, denDF = "default")$Wald
round(dat.ww,3)

# Check the variogram
variogram(dat.asr)

# Check the variance components
summary(dat.asr)$varcomp

# Likelihood Ratio test
logl.tab <- logl_test(model.obj = dat.asr,
                      rand.terms = NULL,
                      resid.terms = c("ar1(Row)"))
logl.tab

# Prediction
pred.out <- multiple_comparisons(model.obj = dat.asr, classify = "Variety")
pred.out

# Graph the predicted values
autoplot(pred.out) +
    labs(x = "Treatments", y = "Predicted Response")
