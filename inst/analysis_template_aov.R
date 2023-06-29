# Author:
# Organisation:
# Date:

library(biometryassist)
library(tidyverse)

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

dat <- read.csv("<data file>", stringsAsFactors = TRUE)

str(dat)

# Change necessary columns to factors
dat <- dat %>% mutate(across(c(3), factor))  # Use column numbers
#dat <- dat %>% mutate(across(c(trt), factor)) # Or use column names

ggplot(data = dat, mapping = aes(x = trt, y = RL)) +
    geom_boxplot() +
    theme_bw()

dat.aov <- aov(RL ~ trt, data = dat)            # fitting the model

resplot(dat.aov)

#summary(dat.aov)
anova(dat.aov)

# Predict the means from the model
# Apply Tukey's to the means

pred.out <- multiple_comparisons(model.obj = dat.aov, classify = "trt")
pred.out


autoplot(pred.out) +
    labs(y = "Predicted Root Length (cm)",
         x = "Calcium Concentration")

# If you would like the graph in Calcium Concentration order

str(pred.out)
pred.out$trt <- factor(pred.out$trt,
                       levels = sort(as.numeric(as.character(pred.out$trt))))

autoplot(pred.out) +
    labs(y = "Predicted Root Length (cm)",
         x = "Calcium Concentration")
