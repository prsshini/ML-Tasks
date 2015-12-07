
rm (list=ls())


library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# The EvalFit function Measures  Best accuracy and GetBest function measures Best CP for the rpart.
source("Evalfit_GetBest_functions.R")

# The Spam Measures.R Function measures the truth,threshold and score values.
source("spam_measures.R")


spamD <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data',sep=',',header=F)


spamCols <- c(
  'word.freq.make', 'word.freq.address', 'word.freq.all',
  'word.freq.3d', 'word.freq.our', 'word.freq.over', 'word.freq.remove',
  'word.freq.internet', 'word.freq.order', 'word.freq.mail',
  'word.freq.receive', 'word.freq.will', 'word.freq.people',
  'word.freq.report', 'word.freq.addresses', 'word.freq.free',
  'word.freq.business', 'word.freq.email', 'word.freq.you',
  'word.freq.credit', 'word.freq.your', 'word.freq.font',
  'word.freq.000', 'word.freq.money', 'word.freq.hp', 'word.freq.hpl',
  'word.freq.george', 'word.freq.650', 'word.freq.lab',
  'word.freq.labs', 'word.freq.telnet', 'word.freq.857',
  'word.freq.data', 'word.freq.415', 'word.freq.85',
  'word.freq.technology', 'word.freq.1999', 'word.freq.parts',
  'word.freq.pm', 'word.freq.direct', 'word.freq.cs',
  'word.freq.meeting', 'word.freq.original', 'word.freq.project',
  'word.freq.re', 'word.freq.edu', 'word.freq.table',
  'word.freq.conference', 'char.freq.semi', 'char.freq.lparen',
  'char.freq.lbrack', 'char.freq.bang', 'char.freq.dollar',
  'char.freq.hash', 'capital.run.length.average',
  'capital.run.length.longest', 'capital.run.length.total',
  'targetvar'
)
colnames(spamD) <- spamCols

# Divide the Data Set using stratified partitioning method as train and validation set.
spam0 <- subset(spamD,spamD$targetvar==0)
spam1 <- subset(spamD,spamD$targetvar==1)


nrows <- NROW(spam0)
spam0$rgroup <- runif(nrows)
train0 <- subset(spam0,spam0$rgroup < 0.5)
val0 <- subset(spam0,spam0$rgroup >= 0.5 & spam0$rgroup < 0.75)
test0 <- subset(spam0,spam0$rgroup >= 0.75)

nrows <- NROW(spam1)
spam1$rgroup <- runif(nrows)
train1 <- subset(spam1,spam1$rgroup < 0.5)
val1 <- subset(spam1,spam1$rgroup >= 0.5 & spam1$rgroup < 0.75)
test1 <- subset(spam1,spam1$rgroup >= 0.75)

train=rbind(train0,train1)
val=rbind(val0,val1)
test=rbind(test0,test1)


rm(val0,val1,test0,test1,train0,train1,spamCols,spam0,spam1,spamD)

fit <- rpart(targetvar ~ .,
             data=train, method = "class")
#,             control=rpart.control(minsplit=2,maxdepth = 10, cp=0.01))
fancyRpartPlot(fit)
Evalfit(fit,val,targetvar)
GetBest(fit,val,targetvar)

probs <- predict(fit, val, type = "prob")
score <- probs[,2]
truth <- val$targetvar

threshold <- 0.85

Measures(truth,score,threshold)


