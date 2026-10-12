source("Pilot_simulation_1_function.R")
library(withr,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")
library(farver,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")
library(RColorBrewer,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")
library(caret,lib.loc ="/home/bcanourrego/work/paper2/Rpackages")

#Read data ------------------------------------------------------------------------
data = read.csv("Alzheimer_data.csv")
# WE have only numeric covariates

#NA
#table(apply(data,2, function(x) mean(is.na(x))))
# 0 NA in the entire data set


# Normalization of the data
# Removing ID and Response variables
data_values = data[,-c(1,452)]


# Synthetic ---------------------------------------------------------------

Sigma = lapply(split(data_values,data$class), cov)
Mu = lapply(split(data_values,data$class),function(x) apply(x,2,mean)) 

library(MASS)
nh=50000
np=50000
set.seed(123)
data1 = mvrnorm(n=nh, mu=Mu[["H"]],Sigma = Sigma[["H"]])
set.seed(123)
data2 = mvrnorm(n=np, mu=Mu[["P"]],Sigma = Sigma[["P"]])
class_label = rep(c("H","P"),each=50000 )
data3 = rbind(data1,data2)

remove(data1,data2)


#library(caret)
set.seed(123)
inTrain <- createDataPartition(
  y = class_label,  ## the outcome data are needed
  p = .9,   ## The percentage of data in the
  ## training set
  list = FALSE
)


training <- data3[ inTrain,]
testing  <- data3[-inTrain,]

preProc = preProcess(training,method=c("center","scale") )
training <- predict(preProc, training)
testing     <- predict(preProc, testing)


rownames(training) = 1:nrow(training)
rownames(testing) = 1:nrow(testing)
training = as.data.frame(training)
testing = as.data.frame(testing)
#training = training[,1:400]
#testing = testing[,1:400]
#summary(training)
#summary(testing)
# Application of my method
#g=1


save.image("/home/bcanourrego/work/paper2/Alzheimer_augmented.RData")