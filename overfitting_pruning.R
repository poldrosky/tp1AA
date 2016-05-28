library('RWeka')
library('rJava')
library('ROCR')
library('ggplot2')
source('functp1.R')

set.seed(1234)

icfes <- read.csv("datasetOriginal.csv", sep = ";")

# create 80% for train y 20% for evaluation
train.regs <- sample(nrow(icfes), size = nrow(icfes) * 0.8)

icfes.train <- icfes[train.regs, ]
icfes.test <- icfes[-train.regs, ]

over <- overfitting(icfes.train, icfes.test)

pdf(file="3a.pdf")
qplot(over$confidenceFactor,over$leaves, xlab='Confidence Factor',
      main = 'Leaves vs Confidence Factor',
      ylab = 'Number Of Leaves') + 
  geom_line(colour="red") + 
  geom_point(colour="red")
dev.off()

pdf(file="3b.pdf")
ggplot(data = over$pruning, aes(x=confidenceFactor, y=performance)) + 
  geom_line(aes(colour=dataset)) + 
  ggtitle('Accuracy vs Confidence Factor') + 
  xlab("Confidence Factor") + 
  ylab("Accuracy") +
  labs(colour="Dataset")
dev.off()

pdf(file="3c.pdf")
qplot(over$perf.rocr@x.values[[1]], over$perf.rocr@y.values[[1]], 
      main = "ROC Curve", xlab="False Positive Rate",
      ylab = "True Positive Rate") + 
  geom_line(colour="red") + 
  geom_point(colour="red") +
  annotate("text", x = 0.5, y = 0.5, label = paste("AUC = ",round(over$auc, digits = 3)))
dev.off()

