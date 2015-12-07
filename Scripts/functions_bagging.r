# functions

bagtrain <- function(train,Formula,weights,ntree){
  #   Use bootstrap samples the same size as the training 
  #   set, with ntree trees.
  ntrain <- dim(train)[1]
  n <- ntrain
  #   Build the bootstrap samples by sampling the row indices of spamTrain with replacement. Each 
  #   column of the matrix samples represents the row indices into spamTrain 
  #   that comprise the bootstrap sample. 
  samples <- sapply(1:ntree, 
                    FUN = function(iter)
                    {sample(1:ntrain, size=n, replace=T)})
  #   Train the individual decision trees and return them 
  #   in a list. Note: this step can take a few minutes.
  treelist <-lapply(1:ntree,
                    FUN=function(iter)
                    {samp <- samples[,iter];
                    rpart(Formula, data=train[samp,], weights=weights)})
  treelist
}

bagpredict <- function(treelist, newdata){
  # assume that the underlying classifier returns decision probabilities, not decisions. 
  probs <- sapply(1:length(treelist),
                    FUN=function(iter) {
                      predict_onetree(treelist[[iter]], newdata)
                    })
  probsums <- rowSums(probs)
  probsums/length(treelist)
}

predict_onetree <- function(tree, newdata){
  prob <- predict(tree,newdata,"prob")
  prob <- prob[,2]
  prob
}

# Plotting commands taken from http://www.sr.bham.ac.uk/~ajrs/R/gallery/plot_CC-vs_nCC_kT_prof.txt
draw <- function(t,q,qmin,qmax,p,pmin,pmax,r,rmin,rmax,f1,f1min,f1max){
  # create data frame
  nr <- length(t)
  
  qu <- data.frame(matrix(ncol = 5, nrow = nr))
  colnames(qu) <- c('x','y','ymin','ymax','type')
  qu$x <- t
  qu$y <- q
  qu$ymin <- qmin
  qu$ymax <- qmax
  qu$type <- "Queue_Rate"
  
  pr <- data.frame(matrix(ncol = 5, nrow = nr))
  colnames(pr) <- c('x','y','ymin','ymax','type')
  pr$x <- t
  pr$y <- p
  pr$ymin <- pmin
  pr$ymax <- pmax
  pr$type <- "Precision"
  
  re <- data.frame(matrix(ncol = 5, nrow = nr))
  colnames(re) <- c('x','y','ymin','ymax','type')
  re$x <- t
  re$y <- r
  re$ymin <- rmin
  re$ymax <- rmax
  re$type <- "Recall"
  
  fm <- data.frame(matrix(ncol = 5, nrow = nr))
  colnames(fm) <- c('x','y','ymin','ymax','type')
  fm$x <- t
  fm$y <- f1
  fm$ymin <- f1min
  fm$ymax <- f1max
  fm$type <- "F1"
  
  #--Combine datasets into a single data frame:
  A <- rbind(qu,pr,re,fm)
  
  # install.packages("ggplot2")
  require(ggplot2)
  
  #--Define axis labels:
  xlabel <- "Threshold"
  ylabel <- "Queue_Rate, Precision, Recall, F1"
  
  ggplot(data=A, aes(x=x, y=y, ymin=ymin, ymax=ymax, fill=type, linetype=type)) + 
  geom_line() + 
  geom_ribbon(alpha=0.5) + 
  xlab(xlabel) + 
  ylab(ylabel)
  
  #ggsave(p, file="CC-vs_nCC_kT_prof.pdf", width=8, height=4.5)
  
}

precrec <- function(truth, probs, threshold){
  numpos <- sum(truth==1)
  decpos <- sum(probs >= threshold)
  corpos <- sum((truth==1) & (probs >= threshold))
  if (decpos >= 1){
    numex <- length(truth)
    queue_rate <- decpos/numex
    precision <- corpos/decpos
    recall <- corpos/numpos
  } else {
    queue_rate <- 0
    precision <- 1
    recall <- 0
  }
  list(queue_rate, precision, recall)
}

