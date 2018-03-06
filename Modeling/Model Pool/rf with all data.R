# RF Model Building With Caret
library(C50)
library(nnet)
library(caret)
library(ggplot2)
library(caret)

data = read.csv(gzfile("finalValue.csv.gz"))


data = data
which(data$AveragedExchange==0)
data$AveragedExchange = as.numeric(data$AveragedExchange) 
str(data$AveragedExchange)
hist(data$AveragedExchange)
data$AveragedExchange[data$AveragedExchange<= -.5] = -1
hist(data$AveragedExchange)
data$AveragedExchange[data$AveragedExchange> -.5 & data$AveragedExchange< .01] = -.5
hist(data$AveragedExchange)
data$AveragedExchange[data$AveragedExchange>= -.2 & data$AveragedExchange<= .2] = 0
hist(data$AveragedExchange)

data$AveragedExchange[data$AveragedExchange<= .5 & data$AveragedExchange> .01] = .5
hist(data$AveragedExchange)
data$AveragedExchange[data$AveragedExchange> .5 ] = 1
hist(data$AveragedExchange)

data$AveragedExchange[data$AveragedExchange == 1] = 5
hist(data$AveragedExchange)
data$AveragedExchange[data$AveragedExchange== -1] = 1
hist(data$AveragedExchange)
data$AveragedExchange[data$AveragedExchange == -.5] = 2
hist(data$AveragedExchange)
data$AveragedExchange[data$AveragedExchange==0] = 3
hist(data$AveragedExchange)
data$AveragedExchange[data$AveragedExchange== .5] = 4
hist(data$AveragedExchange)


data$AveragedExchange = as.factor(data$AveragedExchange)
data = data[ , apply(data, 2, function(x) !any(is.na(x)))]


# nzv <- nearZeroVar(data)
# data <- data[,-nzv]
################################################################
#-----> Removing Low Res Data
data = data[,which( colnames(data)=="article" ):ncol(data)]
################################################################
#--------------> Experimental Merging

# 
data2 = read.csv(gzfile("finalRate.csv.gz"))

#data2 = data2[,-3]
data2 = data2[,-ncol(data2)]

data2 = data2[,which( colnames(data2)=="article" ):ncol(data2)]
data2 = data2[ , apply(data2, 2, function(x) !any(is.na(x)))]


data3 = cbind(data, data2)
# data3 = data3[,-1]
# 
data = data3
#data = data[ , apply(data, 2, function(x) !any(is.na(x)))]

targetColumn = which( colnames(data)=="AveragedExchange" )
#nzv <- nearZeroVar(data)
#data <- data[,-nzv]


#data = na.omit(data)
#data = as.numeric(data)
#data$AveragedExchange =as.factor(data$AveragedExchange)

data = data[ , colSums(is.na(data)) == 0]

library(dplyr)

dataLagHour = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=2))
colnames(dataLagHour)[ncol(dataLagHour)] = c("AveragedExchange")

dataLagHalfDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=24))
colnames(dataLagHalfDay)[ncol(dataLagHalfDay)] = c("AveragedExchange")

dataLagDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=48))
colnames(dataLagDay)[ncol(dataLagDay)] = c("AveragedExchange")

dataLagDayHalf = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=72))
colnames(dataLagDayHalf)[ncol(dataLagDayHalf)] = c("AveragedExchange")

dataLagTwoDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=96))
colnames(dataLagTwoDay)[ncol(dataLagTwoDay)] = c("AveragedExchange")

train = data[1:1800,]
test = data[1801:nrow(data),]

data = train
summary(data)





myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 960,  #  <-----------  20 days
                              horizon = 96,         #  <-----------  2 days
                              fixedWindow = TRUE)


#numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
trainedModel <- train( AveragedExchange ~ .,
                       data = data,
                       method = "rf",
                       preProc = c("center", "scale"),
                       # trControl = numFolds,
                       trControl = myTimeControl)

#pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95

trainedModel
pred = as.data.frame(as.numeric(predict(trainedModel,test)))
true <- as.data.frame(as.numeric(test$AveragedExchange))

predict(trainedModel,data[1801,])

merged = cbind(pred,true)

postResample(pred,true)

conf1 <- confusionMatrix(pred, data$AveragedExchange)

conf1
probs <- predict(pred, newdata=data, type='prob')

(trainedModel)
trainedModel$results= true-pred
resid = as.data.frame(as.numeric(true)-as.numeric(as.character(pred)))


conf1 <- confusionMatrix(pred, data$AveragedExchange)

conf1
probs <- predict(pred, newdata=data, type='prob')

(trainedModel)
trainedModel$results= true-pred
resid = as.numeric(true)-as.numeric(as.character(pred))



jpeg('Modeling/plots/rfModel.jpg')
plot(as.numeric(true), col = "red", ylab = "true (red) , pred (blue)", ylim = range(c(as.numeric(as.character(pred)),true)))
lines(as.numeric(as.character(pred)), col = "blue") 
lines(resid)
dev.off()

saveRDS(trainedModel, "Modeling/trainedModels/rfModel.rds")

