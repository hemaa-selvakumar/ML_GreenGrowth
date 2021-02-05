library(dplyr)
library(Ecdat)
library(ggplot2)
library(ISLR)
library(GGally)
library(tidyverse)
library(car)
library(zoo)

setwd("/Users/hselvakumar/Desktop/MGT Project/Data")

h<-read.csv("table_1.csv")
str(h)
# replace missing data with column mean
view(h)

# Remove the first column, it only has the name of the country and year
h$RowLabels <- NULL 
h <- lapply(h, na.aggregate)

# Regression with all variables 
a.lm <- lm(formula = y ~ ., data = h)

# Anova
anova(a.lm)

# 3 star vars: COL.GPAT_CO_PR.ENV_PAT, DEV.GPAT_DE_CAP.ENV_PAT, DEV.GPAT_DE_GBAORD.ENV_PAT 
# 3 star vars: DEV.GPAT_DE_PERC.ICT, DIFF.GPAT_DI_PERC.BUILD, DIFF.GPAT_DI_PERC.GHG 
# 3 star vars: DIFF.GPAT_DI_PERC.GOODS
# 2 star vars: DEV.GPAT_DE_PERC.GHG, DIFF.GPAT_DI_AT.ENV_PAT
# 1 star vars: DEV.GPAT_DE_PERC.BUILD, DEV.GPAT_DE_PERC.GOODS, DEV.GPAT_DE_PERC.WAT, DIFF.GPAT_DI_AI.ENV_PAT
# 1 star vars: DIFF.GPAT_DI_PERC.ENE


summary(a.lm)


# 3 star vars: COL.GPAT_CO_PR.TOT, DEV.GPAT_DE_CAP.ENV_PAT, DIFF.GPAT_DI_AI.ENV_PAT, DIFF.GPAT_DI_AT.ENV_PAT
# 3 star vars: DIFF.GPAT_DI_PERC.BUILD, DIFF.GPAT_DI_PERC.GHG, DIFF.GPAT_DI_PERC.GOODS
# 2 star vars: DEV.GPAT_DE_GBAORD.ENV_PAT
# 1 star vars: COL.GPAT_CO_AT.ENV_PAT, COL.GPAT_CO_PR.ENV_PAT

# regression with all the 3 stars vars
b.lm <- lm(y~ COL.GPAT_CO_PR.TOT+DEV.GPAT_DE_CAP.ENV_PAT+DIFF.GPAT_DI_AI.ENV_PAT+DIFF.GPAT_DI_AT.ENV_PAT+DIFF.GPAT_DI_PERC.BUILD+DIFF.GPAT_DI_PERC.GHG+DIFF.GPAT_DI_PERC.GOODS, data = h)
summary(b.lm)

# We get an R-squared of 0.4668. Let's see if this model works for another country, we'll chose a country with a GDP in the 20000-40000 range

# new data point is cyprus 2010
newdata <- data.frame(COL.GPAT_CO_PR.TOT=11.05,DEV.GPAT_DE_CAP.ENV_PAT = 0.01, DIFF.GPAT_DI_AI.ENV_PAT= 0.09, DIFF.GPAT_DI_AT.ENV_PAT = 6.5,DIFF.GPAT_DI_PERC.GHG=0.5, DIFF.GPAT_DI_PERC.BUILD=12,DIFF.GPAT_DI_PERC.GOODS=26.67)

predict(b.lm, newdata, interval = "predict")

# result is: 
#   fit      lwr      upr
# 1 32993.03 13026.84 52959.21
# the actual GDP at that year is: 24773.21


# find the best subset of data 
subset = regsubsets(y~ COL.GPAT_CO_PR.TOT+DEV.GPAT_DE_CAP.ENV_PAT+DIFF.GPAT_DI_AI.ENV_PAT+DIFF.GPAT_DI_AT.ENV_PAT+DIFF.GPAT_DI_PERC.BUILD+DIFF.GPAT_DI_PERC.GHG+DIFF.GPAT_DI_PERC.GOODS, data = h)
summary(subset)
