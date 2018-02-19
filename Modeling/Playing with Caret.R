# Model Building With Caret

library(caret)
library(ggplot2)
library(pls)

data = read.csv(gzfile("finalRate.csv.gz"))
data=data[,3:ncol(data)]
#data = na.omit(data)
data = as.numeric(data)
#data$AveragedExchange =as.factor(data$AveragedExchange)

data = data[ , colSums(is.na(data)) == 0]
summary(data)

# timeSlices <- createTimeSlices(1:nrow(data), 
#                                initialWindow = 360, horizon = 120, fixedWindow = TRUE)
# 
# trainSlices <- timeSlices[[1]]
# testSlices <- timeSlices[[2]]


myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 36,
                              horizon = 12,
                              fixedWindow = TRUE)


data$gtrendBitcoinPrice

as.factor(data$AveragedExchange)

plsFitTime <- train( AveragedExchange ~ .,
                    data = data,
                    method = "c50",
                    preProc = c("center", "scale"),
                    trControl = myTimeControl)

pred <- predict(plsFitTime,data)

true <- data$AveragedExchange


plot(true, col = "red", ylab = "true (red) , pred (blue)", ylim = range(c(pred,true)))
points(pred, col = "blue") 


 