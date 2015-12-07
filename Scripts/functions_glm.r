# functions

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

