
rm(list=ls())
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


rm (list=ls())
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


nrows <- NROW(spamD)
spamD$rgroup <- runif(nrows)
train <- subset(spamD,spamD$rgroup < 0.5)
val <- subset(spamD,spamD$rgroup >= 0.5 & spamD$rgroup < 0.75)
test <- subset(spamD,spamD$rgroup >= 0.75)

fit <- rpart(targetvar ~ word.freq.receive+
               word.freq.free+
               char.freq.lparen+
               char.freq.bang+
               word.freq.credit+
               word.freq.money+
               word.freq.make+
               capital.run.length.total+
               capital.run.length.longest+
               char.freq.hash+
               char.freq.dollar+
               word.freq.font+
               word.freq.receive,
             data=train, method = "class",
             control=rpart.control(minsplit=2,maxdepth = 10, cp=0.01))
fancyRpartPlot(fit)
Evalfit(fit,val,targetvar)
GetBest(fit,val,targetvar)


fit <- rpart(targetvar ~ .,
             data=train, method = "class")
#,             control=rpart.control(minsplit=2,maxdepth = 10, cp=0.01))
fancyRpartPlot(fit)
Evalfit(fit,val,targetvar)
GetBest(fit,val,targetvar)


fit <-rpart(targetvar ~ word.freq.receive+
              word.freq.free+
              char.freq.lparen+
              char.freq.bang+
              word.freq.credit+
              word.freq.money+
              word.freq.make+
              capital.run.length.total+
              capital.run.length.longest+
              char.freq.hash+
              char.freq.dollar+
              word.freq.font+
              word.freq.receive,
            data=test, method = "class",
            control=rpart.control(minsplit=2,maxdepth = 10, cp=0.01))
fancyRpartPlot(fit)
Evalfit(fit,test,targetvar)
GetBest(fit,test,targetvar)


fit <-rpart(targetvar ~ .,
            data=test, method = "class") ##,
            
##control=rpart.control(minsplit=2,maxdepth = 10, cp=0.01))
fancyRpartPlot(fit)
Evalfit(fit,test,targetvar)
GetBest(fit,test,targetvar)
