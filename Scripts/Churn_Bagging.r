# example 9.2 of section 9.1.1 of Zumel-Mount
# Exploring advanced methods : Using bagging and random forests to reduce training variance 
# and improve prediction 
# Title: Bagging decision trees

# Start clean
rm (list=ls())

# Load libraries
library(rpart)
library(caret)
library(ggplot2)

source("functions_bagging.R")

# Get telecom churn Data Set
#data <- read.csv('http://www.dataminingconsultant.com/data/churn.txt')
data <- read.csv('../churn/churn.txt')
# remove the "Phone" column
data <- subset(data, select=-c(Phone))
targetvar <- 'Churn.'
targetval <- "True."

nrows <- NROW(data)

#seed <- 72391
seed <- 2097865
ntrials <- 100
#ntrials <- 50
numth <- 100
splitprob <- 2/3

mq <- matrix(nrow = numth+1, ncol = ntrials)
mp <- matrix(nrow = numth+1, ncol = ntrials)
mr <- matrix(nrow = numth+1, ncol = ntrials)
mf1 <- matrix(nrow = numth+1, ncol = ntrials)

for (trial in 1:ntrials){
  writeLines(paste("Trial number: ",trial,"..."))
  # Form a random partition
  train.index <- createDataPartition(data[[targetvar]], p = splitprob, list = FALSE)
  train <- data[ train.index,]
  test  <- data[-train.index,]
  
  # Set up Formula
  xpart <- " . "
  ypart <- paste(targetvar," ~ ")
  Formula <- as.formula(paste(ypart,xpart))
  # Set weights for examples for rpart calls
  wt <- sum(train[[targetvar]]==targetval)/NROW(train)
  if (wt>=0.5){wt=0.5}
  # Let's override the above and simply try equal weights for the two classes for now.
  # This is done by setting wt=0.5:
  wt<-0.5
  weights <- (train[targetvar]==targetval)*(1-2*wt) + wt
  # Do training using bagging of rpart models
  ntree <- 100
  # bagtrain is a function I have formed - it is in functions_bagging.r file.
  # Homework: study both the logic as well as the R code for it carefully.
  bagmodels <- bagtrain(train,Formula,weights,ntree)
  prob <- bagpredict(bagmodels, test)
  #The following lines can be used to run single rpart or randomForest
  #rpartModel <- rpart(Formula, data = train, weights=weights)
  #Model <- rpartModel
  #rm(rpartModel)
  #rfModel <- randomForest(Formula, data = train, classwt=c(1-wt,wt), mtry = 2, importance = TRUE, do.trace = 100)
  #Model <- rfModel
  #rm(rfModel)
  #prob <- predict(Model,test,"prob")
  #prob <- prob[,2]
  #rm(Model)
  truth <- (test[[targetvar]] == targetval)
  threshold <- vector(mode="numeric", length=numth+1)
  queue_rate <- vector(mode="numeric", length=numth+1)
  precision <- vector(mode="numeric", length=numth+1)
  recall <- vector(mode="numeric", length=numth+1)
  f1 <- vector(mode="numeric", length=numth+1)
  for (i in 0:numth){
    th <- i/numth
    measures <- precrec(truth, prob, th)
    threshold[i+1] <- th
    queue_rate[i+1] <- measures[[1]]
    precision[i+1] <- measures[[2]]
    recall[i+1] <- measures[[3]]
  }
  mq[,trial] <- queue_rate
  mp[,trial] <- precision
  mr[,trial] <- recall
  mf1[,trial] <- 2*precision*recall/(precision+recall)
  rm(train.index,train,test,prob,truth,threshold,queue_rate,precision,recall)
}

mq1 <- apply(mq, 1, quantile, probs = c(0.1, 0.5, 0.9)) 
mqt <- t(mq1)
qmin <- mqt[,1]
q <- mqt[,2]
qmax <- mqt[,3]
rm(mq1,mqt)

mp1 <- apply(mp, 1, quantile, probs = c(0.1, 0.5, 0.9)) 
mpt <- t(mp1)
pmin <- mpt[,1]
p <- mpt[,2]
pmax <- mpt[,3]
rm(mp1,mpt)

mr1 <- apply(mr, 1, quantile, probs = c(0.1, 0.5, 0.9)) 
mrt <- t(mr1)
rmin <- mrt[,1]
r <- mrt[,2]
rmax <- mrt[,3]
rm(mr1,mrt)

mf11 <- apply(mf1, 1, quantile, probs = c(0.1, 0.5, 0.9), na.rm=TRUE) 
mf1t <- t(mf11)
f1min <- mf1t[,1]
f1 <- mf1t[,2]
f1max <- mf1t[,3]
rm(mf11,mf1t)

t <- (0:numth)/numth

draw(t,q,qmin,qmax,p,pmin,pmax,r,rmin,rmax,f1,f1min,f1max)

