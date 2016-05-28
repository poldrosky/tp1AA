statmod <- function(x) {
  z <- names(table(x))[table(x) == max(table(x))]
  return (z)
}

overfitting <- function(train, test) {
  confidenceFactor <- c()
  leaves <- c()
  test.performance <- c()
  train.performance <- c()
  
  for (i in seq(0.025, 0.5, 0.025)) {
    confidenceFactor <- c(confidenceFactor, i)
    modelo <- J48(mod_razona_cuantitativo_desem ~ .,
                  data = train,
                  control = Weka_control(C = i))
    
    txtLeave <- tail(capture.output(modelo), 10)
    leave <- as.numeric(sub("Number of Leaves  : \t" , "", txtLeave[7]))
    
    leaves <- c(leaves, leave)
    
    m.prd <- predict(modelo, test[,!colnames(test)=="mod_razona_cuantitativo_desem"])
    m.prop_table <- prop.table(table(m.prd, test$mod_razona_cuantitativo_desem,
      dnn = c("predicho", "observado")
    ))
    test.performance <- c(test.performance, sum(diag(m.prop_table)))
    train.performance <- c(train.performance, (summary(modelo)$details[1]) / 100)
  }
  
  testValues <- cbind(confidenceFactor, test.performance, 'test')
  trainValues <- cbind(confidenceFactor, as.numeric(train.performance), 'train')
  
  pruning <- as.data.frame(rbind(testValues, trainValues))
  pruning[, 1] <- as.numeric(levels(pruning[, 1])[pruning[, 1]])
  pruning[, 2] <- as.numeric(levels(pruning[, 2])[pruning[, 2]])
  
  names(pruning) <- c('confidenceFactor', 'performance', 'dataset')
  
  #Mejor Arbol
  confidenceBestTree <- pruning[pruning$dataset == 'test', ]
  confidenceBestTree <- confidenceBestTree[confidenceBestTree$performance == max(confidenceBestTree$performance), ]
  
  bestTree <- J48(mod_razona_cuantitativo_desem ~ ., data = train,
    control = Weka_control(C = confidenceBestTree[, 1][1])
  )
  
  p1 <- predict(bestTree, test, type = "prob")[, 2] #prob. clase=yes
  
  predict.rocr  <- prediction (p1, test$mod_razona_cuantitativo_desem)
  perf.rocr     <- performance(predict.rocr, "tpr", "fpr") #True y False postivie.rate
  
  # valor ROC
  auc <- as.numeric(performance(predict.rocr , "auc")@y.values)
  
  out <- list(confidenceFactor=confidenceFactor,
              leaves=leaves,
              pruning=pruning,
              perf.rocr=perf.rocr,
              auc=auc)
  return(out)
}
