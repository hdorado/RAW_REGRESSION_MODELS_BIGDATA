setwd("C:/Users/hadorado/Desktop/Alex")
library(caret)
library(party)
data <- read.csv("original.csv",sep=";",dec=",",row.names=1)
data <- data[complete.cases(data),]
data <- subset(data, DIBUJO_SIEMBRA_PLATANO!="EN_BARRERA")
data <- subset(data, d.interno!="LENTO A MUY LENTO")
data$d.interno <- NULL
data <- droplevels(data)

inTrain <- createDataPartition(y=data$RDT_HA, p=0.7, list=F)
training <- data[inTrain,]
testing <- data[-inTrain,]

set.seed(22)
model <- cforest(RDT_HA ~ ., training, controls=
cforest_unbiased(mtry=max(floor(ncol(training)/3), 1), ntree=500))

vars <- varimp(model, conditional=T)

png("varimpSeed22.png", width=1920, height=1080)
dotplot(sort(vars),panel = function(x,y){panel.dotplot(x,y,col="darkblue",pch=16,cex=1.1)
							panel.abline(v=abs(min(vars)),col="red",lty="longdash",lwd=2)
						    })
dev.off()

set.seed(1024)
model <- cforest(RDT_HA ~ ., training, controls=
cforest_unbiased(mtry=max(floor(ncol(training)/3), 1), ntree=500))

vars <- varimp(model, conditional=T)

png("varimpSeed1024.png", width=1920, height=1080)
dotplot(sort(vars),panel = function(x,y){panel.dotplot(x,y,col="darkblue",pch=16,cex=1.1)
							panel.abline(v=abs(min(vars)),col="red",lty="longdash",lwd=2)
						    })
dev.off()

