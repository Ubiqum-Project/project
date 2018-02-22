library(keras)
library(OneR)
library(Hmisc)
library(caret)
library(binr)
bins = 3
splitLower = 1100
splitHigher = splitLower +1


#data = read.csv(gzfile("finalRate.csv.gz"))
data = read.csv(gzfile("finalValue.csv.gz"))
data = data+1
data = data[ , apply(data, 2, function(x) !any(is.na(x)))]
data=data[,3:ncol(data)]
 nzv <- nearZeroVar(data)
 data <- data[,-nzv]
data$AveragedExchange = as.numeric(as.character(data$AveragedExchange))

test = data
test$bin = as.numeric(cut2(data$AveragedExchange, g=bins, m=nrow(data)/bins))

# library(classInt)
# classIntervals(data$AveragedExchange, 4, style = 'quantile')
# x <- as.data.frame(classIntervals(data$AveragedExchange, 4, style = 'equal'))

data$AveragedExchange = as.numeric(cut2(data$AveragedExchange, g=bins))
plot(bin(data$AveragedExchange, bins))
summary(data$AveragedExchange)
data$AveragedExchange= as.numeric(data$AveragedExchange)-1

data$AveragedExchange = as.factor(data$AveragedExchange)
plot(data$AveragedExchange)
x_train = (data[1:splitLower,1:ncol(data)-1])
y_train = (data[1:splitLower,ncol(data)])
x_test = data[splitHigher:nrow(data),1:ncol(data)-1]
y_test = data[splitHigher:nrow(data),ncol(data)]


x_train <- as.data.frame(x_train)
y_train <- as.data.frame(y_train)
x_test <- as.data.frame(x_test)
y_test <- as.data.frame(y_test)

plot(y_test)
plot(y_train)
trainingSize = ncol(x_train)


x_train <-  as.matrix(x_train  )        #mnist$train$x
y_train <-    as.matrix(y_train )       #   mnist$train$y
x_test <-     as.matrix((x_test))       # mnist$test$x
y_test <-     as.matrix((y_test    ))      # mnist$test$y

# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), trainingSize))
x_test <- array_reshape(x_test, c(nrow(x_test), trainingSize))


model <- keras_model_sequential() 
model %>% 
  layer_dense(units = trainingSize, activation = 'relu', input_shape = c(trainingSize)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 45, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 25, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = bins, activation = 'softmax')

model %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)



history <- model %>% fit(
  x_train, y_train, 
  epochs = 100, batch_size = 150, 
  validation_split = 0.2
)

plot(history)

model %>% evaluate(x_test, y_test)

model %>% predict_classes(x_train)
predictions = round(as.data.frame(predict(model,x_test)), digits = 1)
colnames(predictions) =c("0","1","2")
predictions$predicted = colnames(predictions)[apply(predictions,1,which.max)]
predictions$actual = y_test
predictions$compare = (predictions$predicted == predictions$actual)

sum(predictions$compare, na.rm=TRUE)/nrow(predictions)

model %>% predict(x_train)

results = as.data.frame(y_train)
results$results = model %>% predict_classes(x_train)
