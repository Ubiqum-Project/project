library(keras)
library(OneR)
#mnist <- dataset_mnist()

data = read.csv(gzfile("finalRate.csv.gz"))
data = data[ , apply(data, 2, function(x) !any(is.na(x)))]
data=data[,3:ncol(data)]
data$AveragedExchange = as.numeric(as.character(data$AveragedExchange))
data$AveragedExchange = bin(data$AveragedExchange, nbins = 5, labels = c(0,.25,.5,.75,1))


data$AveragedExchange = as.factor(data$AveragedExchange)

x_train = (data[1:1700,1:ncol(data)-1]+1)
y_train = (data[1:1700,ncol(data)])
x_test = data[1701:nrow(data),1:ncol(data)-1]+1
y_test = data[1701:nrow(data),ncol(data)]

x_train <- as.data.frame(x_train)
y_train <- as.data.frame(y_train)
x_test <- as.data.frame(x_test)
y_test <- as.data.frame(y_test)



x_train <-  as.matrix(x_train  )        #mnist$train$x
y_train <-    as.matrix(y_train )       #   mnist$train$y
x_test <-     as.matrix((x_test))       # mnist$test$x
y_test <-     as.matrix((y_test    ))      # mnist$test$y

# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 93))
x_test <- array_reshape(x_test, c(nrow(x_test), 93))


model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 93, activation = 'relu', input_shape = c(93)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 45, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 5, activation = 'relu')

model %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)
which(is.na(data))
plot(history)

model %>% evaluate(x_test, y_test)

model %>% predict_classes(x_test)

