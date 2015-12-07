Evalfit <- function(fit,val,targetvar)
{
  Prediction <- predict(fit,val,type="class")
  prediction <- as.numeric(paste(Prediction))
  accuracy <- 1 - (sum(abs(val$targetvar-prediction))/nrow(val))
  return(accuracy)
}

GetBest <- function(fit,val,targetvar){
  printcp(fit)
  cpvalues <- fit$cptable[,1]
  cprows <- NROW(cpvalues)
  bestcp <-0
  bestaccuracy <-0
  for(i in 1:cprows){
    pfit <- prune(fit,cp=cpvalues[i] + 1e-7) 
    accuracy <- Evalfit(pfit,val,targetvar)
    if (accuracy > bestaccuracy){
      bestaccuracy <- accuracy
      bestcp <- cpvalues[i]
    }
  }
  return (list(bestcp,bestaccuracy))
}

