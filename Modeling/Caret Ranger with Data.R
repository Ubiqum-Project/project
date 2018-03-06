# RF Model Building With Caret
library(C50)
library(nnet)
library(caret)
library(ggplot2)
library(caret)

try(my_model <- readRDS("Modeling/rangerModel.rds"))



data = read.csv(gzfile("finalRate.csv.gz"))
data=data[,3:ncol(data)] 
#nzv <- nearZeroVar(data)
#data <- data[,-nzv]


#data = na.omit(data)
#data = as.numeric(data)
#data$AveragedExchange =as.factor(data$AveragedExchange)

data = data[ , colSums(is.na(data)) == 0]
summary(data)

# timeSlices <- createTimeSlices(1:nrow(data), 
#                                initialWindow = 360, horizon = 120, fixedWindow = TRUE)
# 
# trainSlices <- timeSlices[[1]]
# testSlices <- timeSlices[[2]]


myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 960,  #  <-----------  20 days
                              horizon = 96,         #  <-----------  2 days
                              fixedWindow = TRUE)


#numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
trainedModel <- train( as.factor(AveragedExchange) ~ .,
                       data = data,
                       method = "ranger",
                       preProc = c("center", "scale"),
                       # trControl = numFolds,
                       trControl = myTimeControl)


#pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
pred = predict(trainedModel,data)
true <- data$AveragedExchange


conf1 <- confusionMatrix(pred, data$AveragedExchange)

conf1
probs <- predict(pred, newdata=data, type='prob')

(trainedModel)
trainedModel$results= true-pred
resid = as.numeric(true)-as.numeric(as.character(pred))



jpeg('Modeling/plots/rangerModel.jpg')
plot(as.numeric(true), col = "red", ylab = "true (red) , pred (blue)", ylim = range(c(as.numeric(as.character(pred)),true)))
lines(as.numeric(as.character(pred)), col = "blue") 
lines(resid)
dev.off()

saveRDS(trainedModel, "Modeling/trainedModels/rangerModel.rds")

