# Model Building With Caret

library(caret)
library(ggplot2)
library(pls)

data = read.csv(gzfile("finalRate.csv.gz"))
data=data[,3:ncol(data)]
timeSlices <- createTimeSlices(1:nrow(data), 
                               initialWindow = 36, horizon = 12, fixedWindow = TRUE)

trainSlices <- timeSlices[[1]]
testSlices <- timeSlices[[2]]
data$AveragedExchange
plsFitTime <- train(AveragedExchange ~ .,
                    data = data[trainSlices[[1]],],
                    method = "pls",
                    preProc = c("center", "scale"))

pred <- predict(plsFitTime,data[testSlices[[1]],])

true <- data$unemploy[AveragedExchange[[1]]]

plot(true, col = "red", ylab = "true (red) , pred (blue)", ylim = range(c(pred,true)))
points(pred, col = "blue") 