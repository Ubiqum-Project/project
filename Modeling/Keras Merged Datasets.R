
library(keras)
library(OneR)
library(Hmisc)
library(caret)

#--------------> Experimental Merging

data = read.csv(gzfile("finalRate.csv.gz"))

data = data[,-2]

#data=data[,3:ncol(data)-1]

data2 = read.csv(gzfile("finalValue.csv.gz"))
data2 = data2[,-2]
data2 = data2[,-ncol(data2)]
data2 = data2[-1,]
#data2 = data2[ , apply(data2, 2, function(x) !any(is.na(x)))]
#data2=data2[,3:ncol(data2)]

data3 = merge(data, data2, by.x = 1, by.y = 1)
data3 = data3[,-1]

data = data3

#------------------> experimental merging
data = data[ , apply(data, 2, function(x) !any(is.na(x)))]
data = (data)+1
str(data)
#nzv <- nearZeroVar(data)
#data <- data[,-nzv]

data$AveragedExchange = as.numeric(as.character(data$AveragedExchange))

test = data
test$bin = as.numeric(cut2(data$AveragedExchange, g=5))

data$AveragedExchange = as.numeric(cut2(data$AveragedExchange, g=5))

data$AveragedExchange= as.numeric(data$AveragedExchange)-1

data$AveragedExchange = as.factor(data$AveragedExchange)

AveragedExchange = "AveragedExchange"

x_train = (data[1:2000,1:ncol(data)])
x_train = x_train[,-which( colnames(data)==AveragedExchange )]
y_train = (data[1:2000,AveragedExchange])
x_test = data[2001:nrow(data),1:ncol(data)]
x_test = x_test[,-which( colnames(data)==AveragedExchange )]
y_test = data[2001:nrow(data),AveragedExchange]

x_train <- as.data.frame(x_train)
y_train <- as.data.frame(y_train)
x_test <- as.data.frame(x_test)
y_test <- as.data.frame(y_test)



x_train <-  as.matrix(x_train  )        #mnist$train$x
y_train <-    as.matrix(y_train )       #   mnist$train$y
x_test <-     as.matrix((x_test))       # mnist$test$x
y_test <-     as.matrix((y_test    ))      # mnist$test$y

# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 186))
x_test <- array_reshape(x_test, c(nrow(x_test), 186))


model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 186, activation = 'relu', input_shape = c(186)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 45, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 25, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 5, activation = 'softmax')

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
colnames(predictions) =c("0","1", "2", "3","4")
predictions$predicted = colnames(predictions)[apply(predictions,1,which.max)]
predictions$actual = y_test
predictions$compare = (predictions$predicted == predictions$actual)

sum(predictions$compare, na.rm=TRUE)/nrow(predictions)

model %>% predict(x_train)

results = as.data.frame(y_train)
results$results = model %>% predict_classes(x_train)
