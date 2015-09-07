setwd("C:/Users/hadorado/Desktop/Alex")

library(caret)

source('profilePlot.R')

data <- read.csv("original.csv",sep=";",na.strings="ND", dec=",",row.names=1)
data <- subset(data, DIBUJO_SIEMBRA_PLATANO!="EN_BARRERA")
data <- subset(data, d.interno!="LENTO A MUY LENTO")
data <- subset(data, CULT_ASOCIADO_PLATANO=="NO")
data$CULT_ASOCIADO_PLATANO <- NULL
data$ANALISIS_QUIMICO_PLATANO <- NULL
data$d.interno <- NULL
data$bio_3 <- NULL
data$bio_7 <- NULL
data <- droplevels(data)

management <- data[,c(1:3,26)]
climate <- data[,c(4:20,26)]
soil <- data[,c(21:25,26)]
mgtsoil <- data[,c(1:3,21:25,26)]
clisoil <- data[,c(4:25,26)]
mgtcli <- data[,c(1:20,26)]

data <- management

v <- integer()

profiles <- list()
length(profiles) <- length(names(data)[-ncol(data)])
names(profiles) <- names(data)[-ncol(data)]

perf <- 0

nb.it <- 100

for(i in 1:nb.it) {
  print(i)
  inTrain <- createDataPartition(y=data$RDT_HA, p=0.7, list=F)
  training <- data[inTrain,]
  testing <- data[-inTrain,]
  
  grid <- expand.grid(mtry=round(seq(from=1, to=ncol(training)-1, length=4)))
  model <- train(training[,-ncol(training)], training[,ncol(training)],
                 method="cforest", tuneGrid=grid)#rf
  
  performance <- R2(predict(model, testing), testing$RDT_HA) * 100
  
  perf <- perf + performance
  
  currentVarImp <- t(varImp(model, scale=F)$importance)
  
  scale <- performance / sum(currentVarImp)
  
  scaledVarImp <- scale * currentVarImp
  
  if(length(v) == 0) {
    v <- scaledVarImp
  } else {
    v <- rbind(v, scaledVarImp)
  }
  
  ordered <- sort(sapply(as.data.frame(scaledVarImp), median), decreasing=T)
  
  for(n in names(ordered)[1:3]) {
    profile <- profilePlot(model, n, data, F)
    if(length(profiles[[n]]) == 0) {
      profiles[[n]] <- data.frame(profile$y)
      #names(profiles)[n] <- n
      row.names(profiles[[n]]) <- profile$x
    } else {
      profiles[[n]] <- cbind(profiles[[n]], profile$y)
    }
  }
}

setwd("C:/Users/hadorado/Desktop/Alex/ResultUncorrelated/New")
source("multiProfile.R")

v <- as.data.frame(v)

ordered <- sort(sapply(v, median), decreasing=T)

perf <- signif(perf / nb.it, 5)

se <- apply(v, 2, function(x) 1.96*sd(x, na.rm=TRUE)/sqrt(nrow(v)))
mean <- as.data.frame(ordered)
mean <- cbind(mean, names(ordered))
names(mean) <- c("Mean", "Variable")
mean$Variable <- factor(names(ordered), levels= names(ordered))
limits <- aes(ymax = Mean + se, ymin=Mean - se)
m <- ggplot(mean, aes(x=Variable, y=Mean))
m + geom_bar(stat="identity", width=0.5, fill="blue") + ylab("Mean importance")+
  geom_errorbar(limits, width=0.25) + theme_bw() +
  ggtitle(paste("Importance of variables (with a mean R2 of", perf, "%)")) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0))