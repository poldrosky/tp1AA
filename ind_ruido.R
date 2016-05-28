library('RWeka')
library('rJava')
library('ROCR')
library('ggplot2')
source('functp1.R')

set.seed(1234)

icfes <- read.csv("datasetOriginal.csv", sep = ";")

# create 80% for train y 20% for evaluation
train.regs <- sample(nrow(icfes), size = nrow(icfes) * 0.8)

icfes.train <- icfes[train.regs,]
icfes.test <- icfes[-train.regs,]

noise <- function(dataset, percentage) {
  index <- sample(nrow(dataset), size = nrow(dataset) * percentage)
  dataset$mod_razona_cuantitativo_desem[index]<- 
    ifelse(dataset$mod_razona_cuantitativo_desem[index]=="SOBRE LA MEDIA", 
           "BAJO LA MEDIA", "SOBRE LA MEDIA") 
  return(dataset)
}

noise.table <- c()
for (i in seq(0, 0.35, 0.05)){
  dataNoise <- noise(icfes.train,i)
  over <- overfitting(dataNoise, icfes.test)
  pruning <- cbind(i, over$pruning,over$leaves)
  noise.table <- rbind(noise.table, pruning)
}

pdf(file="5a.pdf")
ggplot(data = noise.table,
       aes(x=confidenceFactor, y=performance, colour=dataset, group=interaction(i, dataset))) + 
  geom_point() +
  geom_line(alpha=0.3) +
  ggtitle('Accuracy vs Confidence Factor') +
  ylab("Accuracy") +
  xlab("Confidence Factor") +
  labs(colour="Dataset", size="Noise")
dev.off()

pdf(file="5b.pdf")
ggplot(data = noise.table,
       aes(x = i, y = `over$leaves`)) + 
  geom_line() +
  ggtitle('Leaves vs Percentage') + 
  xlab("Noise Percentage") + 
  ylab("Number Of Leaves") +
  scale_colour_gradientn(colours=rainbow(10)) +
  labs(colour="Confidence Factor") 
dev.off()

pdf(file="5c.pdf")
ggplot(data = noise.table,
       aes(x=i, y=performance)) + 
  geom_line(aes(colour=dataset)) +
  ggtitle('Accuracy vs Noise Percentage') + 
  xlab("Noise Percentage") +
  ylab("Accuracy") +
  labs(colour="Dataset", size="Confidence Factor")
dev.off()

noise.table1<- cbind(noise.table[noise.table$dataset=='train',], 
                       noise.table[noise.table$dataset=='test',]$performance)
names(noise.table1) <- c('percentage','confidenceFactor','performance.train','dataset',
                           'leaves','performance.test')

pdf(file="5d.pdf")
ggplot(data = noise.table1,
       aes(x=confidenceFactor, y=percentage)) + 
  geom_point(aes(colour=performance.test, size=performance.train), alpha=0.6) +
  scale_colour_gradientn(colours=rainbow(4)) +
  ggtitle('Noise Percentage vs Confidence Factor') + 
  xlab("Confidence Factor") +
  ylab("Noise Percentage") +
  labs(colour="Accuracy Test", size="Accuracy Train")
dev.off()
  
  