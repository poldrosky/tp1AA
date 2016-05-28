library('RWeka')
library('rJava')
library('ROCR')
library('ggplot2')
library('arules')
source('functp1.R')

set.seed(1234)

icfes <- read.csv("datasetOriginal.csv", sep = ";")

# create 80% for train y 20% for evaluation
train.regs <- sample(nrow(icfes), size = nrow(icfes) * 0.8)

icfes.train <- icfes[train.regs,]
icfes.test <- icfes[-train.regs,]

missing <- function(dataset, percentage) {
  # Borrar datos en el conjunto de entrenamiento
  index <- sample(nrow(dataset), size = nrow(dataset) * percentage)
  index1 <- sample(nrow(dataset), size = nrow(dataset) * percentage)
  dataset$inst_acreditada[index] <- NA
  dataset$estu_metodo_prgm[index1] <- NA
  
  dataset$inst_acreditada[is.na(dataset$inst_acreditada)] <- statmod(dataset$inst_acreditada)
  
  dataset$estu_metodo_prgm[is.na(dataset$estu_metodo_prgm) &
                             dataset$mod_razona_cuantitativo_desem == 'SOBRE LA MEDIA'] <-
    statmod(dataset$estu_metodo_prgm[dataset$mod_razona_cuantitativo_desem ==
                                       'SOBRE LA MEDIA'])
  dataset$estu_metodo_prgm[is.na(dataset$estu_metodo_prgm) &
                             dataset$mod_razona_cuantitativo_desem == 'BAJO LA MEDIA'] <-
    statmod(dataset$estu_metodo_prgm[dataset$mod_razona_cuantitativo_desem ==
                                       'BAJO LA MEDIA'])
  
  return(dataset)
}

missing.table <- c()
for (i in seq(0, 0.85, 0.025)){
  dataMissing <- missing(icfes.train,i)
  over <- overfitting(dataMissing, icfes.test)
  pruning <- cbind(i, over$pruning,over$leaves)
  missing.table <- rbind(missing.table, pruning)
}

pdf(file="4a.pdf")
ggplot(data = missing.table,
       aes(x=confidenceFactor, y=performance, colour=dataset, group=interaction(i, dataset))) + 
  geom_point() +
  geom_line(alpha=0.3) +
  ggtitle('Accuracy vs Confidence Factor') +
  ylab("Accuracy") +
  xlab("Confidence Factor") +
  labs(colour="Dataset", size="Missing")
dev.off()

pdf(file="4b.pdf")
ggplot(data = missing.table,
       aes(x = i, y = `over$leaves`)) + 
  geom_point(aes(colour = confidenceFactor)) +
  ggtitle('Leaves vs Percentage') + 
  xlab("Missing Percentage") + 
  ylab("Number Of Leaves") +
  scale_colour_gradientn(colours=rainbow(10)) +
  labs(colour="Confidence Factor")
dev.off()

pdf(file="4c.pdf")
ggplot(data = missing.table,
       aes(x=i, y=performance)) + 
  geom_line(aes(colour=dataset)) +
  ggtitle('Accuracy vs Missing Percentage') + 
  xlab("Missing Percentage") +
  ylab("Accuracy") +
  labs(colour="Dataset", size="Confidence Factor")
dev.off()

missing.table1<- cbind(missing.table[missing.table$dataset=='train',], 
               missing.table[missing.table$dataset=='test',]$performance)
names(missing.table1) <- c('percentage','confidenceFactor','performance.train','dataset',
                         'leaves','performance.test')

pdf(file="4d.pdf")
ggplot(data = missing.table1,
       aes(x=confidenceFactor, y=percentage)) + 
  geom_point(aes(colour=performance.test, size=performance.train), alpha=0.6) +
  scale_colour_gradientn(colours=rainbow(4)) +
  ggtitle('Missing Percentage vs Confidence Factor') + 
  xlab("Confidence Factor") +
  ylab("Missing Percentage") +
  labs(colour="Accuracy Test", size="Accuracy Train")
dev.off()




