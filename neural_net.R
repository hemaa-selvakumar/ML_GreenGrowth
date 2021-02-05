# neural network 
library(dplyr)
library(Ecdat)
library(ggplot2)
library(ISLR)
library(GGally)
library(tidyverse)
library(car)
library(zoo)
library(leaps)
library(neuralnet)

setwd("/Users/hselvakumar/Desktop/MGT Project/Data")
h<-read.csv("table_1.csv")

# Remove the first column, it only has the name of the country and year
h$RowLabels <- NULL 

# Remove the records with missing data
h <- na.omit(h)
sum(is.na(h$y))

scale1 <-function(x){
  (x-min(x))/(max(x) -min(x))
}
h_data <- h%>%
  mutate_all(scale1)

set.seed(1)

h_train <- sample_frac(tbl=h_data, replace=FALSE, size=0.8)
h_test <- anti_join(h_data, h_train)


h_nn <- neuralnet(y~., data=h_train,
                  hidden=c(4), 
                  act.fct= "logistic")

y_train <- h_train$y
y_hat_train <-unlist(h_nn$net.result)

# training error
nn_train_sse <- sum((y_hat_train-y_train)^2)

test_nn_output <-compute(h_nn, h_test)$net.result

y_test <- h_test$y
y_hat_test <-unlist(test_nn_output)

nn_test_sse <- sum((y_hat_test-y_test)^2)

