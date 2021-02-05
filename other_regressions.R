library(dplyr)
library(Ecdat)
library(ggplot2)
library(ISLR)
library(GGally)
library(tidyverse)
library(car)
library(zoo)
library(leaps)

setwd("/Users/hselvakumar/Desktop/MGT Project/Data")
h<-read.csv("table_1.csv")

# Remove the first column, it only has the name of the country and year
h$RowLabels <- NULL 

# Remove the records with missing data
h <- na.omit(h)
sum(is.na(h$y))


# Repeat regression with all the variables 
# Regression with all variables 
a.lm <- lm(formula = y ~ ., data = h)
summary(a.lm)

# Rsquared of 0.7786

# Best subset regression
regfit.full=regsubsets(y~., data=h, nvmax=28)

reg.summary = summary(regfit.full)

reg.summary$rsq

# output is 
# [1] 0.2376263 0.4808257 0.5962559 0.6303461 0.6507321 0.6853257
# [7] 0.7053313 0.7162556 0.7258624 0.7375057 0.7434148 0.7497806
# [13] 0.7573671 0.7621553 0.7649564 0.7673853 0.7692187 0.7706897
# [19] 0.7729927 0.7745641 0.7753003 0.7757054 0.7767580 0.7783214
# [25] 0.7784235 0.7784569 0.7785501 0.7785675

plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp")

# Ideally we should use 14 regressors
points(14, reg.summary$cp[14], pch=20, col="red")

# using the r-squared as metric 
reg.summary$adjr2
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted R-Squared")
which.max(reg.summary$adjr2)
points(16,reg.summary$adjr2[16],pch=20,col="red")

# using the BIC as metric 
reg.summary$bic
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC")
which.min(reg.summary$bic)
points(10,reg.summary$bic[10],pch=20,col="red")


# plotting RSS, adjusted R2, CP and Bic all together
# max reg.summary$adjr2 = 16
# min reg.summary$cp = 14
#
# min reg.summary$bic = 10
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted R-Squared", type="l")
points(16,reg.summary$adjr2[16],pch=20,col="red", cex=2)
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
points(14, reg.summary$cp[14], pch=20, col="red", cex=2)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC", type="l")
points(10,reg.summary$bic[10],pch=20,col="red", cex=2)


plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

set.seed(1)
# Make a training and validation set to chose a good subset model 
train=sample(c(TRUE,FALSE), nrow(h), rep=TRUE)
test = (!train)


# now apply regsubsets() to the training set to perform best subset selection 
regfit.best = regsubsets(y~., data=h[train,], nvmax=28)
summary(regfit.best)

# test restuls
test.mat = model.matrix(y~., data=h[test,])


# Make predictions on the observations not used for training

val.errors=rep(NA,28) # there are 28 models

for(i in 1:28){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((h$y[test]-pred)^2)
}

which.min(val.errors)

coef(regfit.best, 24)

# select best model from all the data
regfit.best=regsubsets(y~., data=h, nvmax=28)
coef(regfit.best, 24)


# Ridge Regression
library(glmnet)
x = model.matrix(y~., h)[,2:29]
y = h$y

grid=10^seq(10, -2, length=100)

ridge.mod=glmnet(x,y,alpha=0, lambda=grid) # this function standardizes the variables

dim(coef(ridge.mod))

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)

y.test=y[test]
ridge.mod =glmnet(x[train ,],y[train],alpha =0, lambda =grid,thresh =1e-12)

# ridge.pred=predict (ridge.mod ,s=4, newx=x[test ,])
# 
# mean(( ridge.pred -y.test)^2) #mean = 52975371
# mean(( mean(y[train ])-y.test)^2) # result = 83642733
# 
# ridge.pred=predict (ridge.mod ,s=1e10 ,newx=x[test ,])
# mean(( ridge.pred -y.test)^2) # result = 83642500

# find the optimal lambda
set.seet(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=0) 
plot(cv.out)
bestlam = cv.out$lambda.min # 479.8761

ridge.pred=predict (ridge.mod ,s=bestlam ,newx=x[test ,])
mean(( ridge.pred -y.test)^2) # 36173023

# refit the ridge on the whole dataset
out=glmnet(x,y,alpha =0)
predict (out,type="coefficients",s=bestlam )[1:29 ,]


# Lasso Regression 
lasso.mod=glmnet(x[train, ], y[train], alpha=1, lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)

bestlam = cv.out$lambda.min #3.715506

lasso.pred=predict (lasso.mod ,s=bestlam ,newx=x[test ,])
mean((lasso.pred -y.test)^2) #52988209


out=glmnet (x,y,alpha =1, lambda=grid)
lasso.coef=predict (out ,type ="coefficients",s=bestlam )[1:29 ,]
lasso.coef
