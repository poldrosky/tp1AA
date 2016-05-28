library('RWeka')
library('rJava')
library('ROCR')
library('ggplot2')
library('arules')
source('functp1.R')

set.seed(1234)

icfes <- read.csv("datasetOriginal.csv", sep = ";")

#Supervised
icfes.supervised <- Discretize(mod_razona_cuantitativo_desem~., data = icfes,
                 control=Weka_control(R=3))

# create 80% for train y 20% for evaluation
train.regs <- sample(nrow(icfes.supervised), size = nrow(icfes.supervised) * 0.8)

icfes.train <- icfes.supervised[train.regs, ]
icfes.test <- icfes.supervised[-train.regs, ]

over <- overfitting(icfes.train, icfes.test)

pdf(file="6a.pdf")
qplot(over$confidenceFactor,over$leaves, xlab='Confidence Factor',
      main = 'Leaves vs Confidence Factor',
      ylab = 'Number Of Leaves') + 
  geom_line(colour="red") + 
  geom_point(colour="red")
dev.off()

pdf(file="6b.pdf")
ggplot(data = over$pruning, aes(x=confidenceFactor, y=performance)) + 
  geom_line(aes(colour=dataset)) + 
  ggtitle('Accuracy vs Confidence Factor') + 
  xlab("Confidence Factor") + 
  ylab("Accuracy") +
  labs(colour="Dataset")
dev.off()

pdf(file="6c.pdf")
qplot(over$perf.rocr@x.values[[1]], over$perf.rocr@y.values[[1]], 
      main = "ROC Curve", xlab="False Positive Rate",
      ylab = "True Positive Rate") + 
  geom_line(colour="red") + 
  geom_point(colour="red") +
  annotate("text", x = 0.5, y = 0.5, label = paste("AUC = ",round(over$auc, digits = 3)))
dev.off()

#Frecuencia y y densidad

density <- function(dataset, column, nbin) {
  dataset[,column] <- discretize(dataset[,column], "frequency", categories=nbin)
  return(dataset)
}

frequency <- function(dataset, column, nbin) {
  dataset[,column] <- discretize(dataset[,column], categories=nbin)
  return(dataset)
}

density.table <- c()
frequency.table <- c()

for (i in seq(1, 20, 1)){
  dataDensity <- density(icfes, 3 ,i)
  train.regs <- sample(nrow(dataDensity), size = nrow(dataDensity) * 0.8)
  icfes.train <- dataDensity[train.regs,]
  icfes.test <- dataDensity[-train.regs,]
  over <- overfitting(icfes.train, icfes.test)
  over$pruning <- cbind(i, over$pruning,over$leaves)
  density.table <- rbind(density.table, over$pruning)
}

for (i in seq(1, 20, 1)){
  dataFrequency <- frequency(icfes, 3 ,i)
  train.regs <- sample(nrow(dataFrequency), size = nrow(dataFrequency) * 0.8)
  icfes.train <- dataFrequency[train.regs,]
  icfes.test <- dataFrequency[-train.regs,]
  over <- overfitting(icfes.train, icfes.test)
  over$pruning <- cbind(i, over$pruning,over$leaves)
  frequency.table <- rbind(frequency.table, over$pruning)
}

frequency.table <- cbind(frequency.table, 'frecuency')
density.table <- cbind(density.table, 'density')

names(frequency.table) <- c('nbins','confidenceFactor',
                            'performance', 'dataset',
                            'leaves', 'discretize')

names(density.table) <- c('nbins','confidenceFactor',
                            'performance', 'dataset',
                            'leaves', 'discretize')

nbins.table <- rbind(frequency.table, density.table)

pdf(file="6d.pdf")
ggplot(data = nbins.table,
       aes(x=confidenceFactor, y=leaves, colour=discretize,
           group=interaction(nbins, discretize, dataset))) + 
  geom_point() +
  geom_line() +
  ggtitle('Number of Leaves vs Confidence Factor') +
  ylab("Number of Leaves") +
  xlab("Confidence Factor") +
  labs(colour="Discretize")
dev.off()

pdf(file="6e.pdf")
ggplot(data = nbins.table,
       aes(x = nbins, y = leaves)) + 
  geom_line(aes(colour = discretize)) +
  ggtitle('Number of Leaves vs Number of Bins') + 
  xlab("Number of Bins") + 
  ylab("Number Of Leaves") +
  labs(colour="Discretize")
dev.off()

pdf(file="6f.pdf")
ggplot(data = density.table,
       aes(x=nbins, y=performance, group=dataset)) + 
 geom_line(aes(colour=dataset)) +
  ggtitle('Accuracy vs Number of Bins (Density Discretize)') + 
  xlab("Number of Bins") +
  ylab("Accuracy") +
  labs(colour="Dataset", size="Confidence Factor")
dev.off()

pdf(file="6e.pdf")
ggplot(data = frequency.table,
       aes(x=nbins, y=performance, group=dataset)) + 
  geom_line(aes(colour=dataset)) +
  ggtitle('Accuracy vs Number of Bins (Frequency Discretize)') + 
  xlab("Number of Bins") +
  ylab("Accuracy") +
  labs(colour="Dataset", size="Confidence Factor")
dev.off()
