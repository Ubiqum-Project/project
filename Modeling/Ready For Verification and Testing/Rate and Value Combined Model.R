library(caret)
library(keras)
library(dplyr)
################################################################

data = read.csv(gzfile("FullDBDataOutput.csv.gz")) #-----------> For Local Runs outside of APP
#data = data[ , apply(data, 2, function(x) !any(is.na(x)))]

targetColumn = which( colnames(data)=="AveragedExchange" )
data[,targetColumn]
#nzv <- nearZeroVar(data)
#data <- data[,-nzv]


#data = na.omit(data)
#data = as.numeric(data)
#data$AveragedExchange =as.factor(data$AveragedExchange)

data = data[ , colSums(is.na(data)) == 0]

dataLagDay = cbind(data[,-targetColumn],lead(data$AveragedExchange, n=48))
colnames(dataLagDay)[ncol(dataLagDay)] = c("AveragedExchange")

data = na.omit(dataLagDay)
# 
trainIndex = round(nrow(data) *.60,digits =0)
testIndex = nrow(data)-trainIndex


trainIndex
testIndex
# 
#  train = data[1:trainIndex,]
#  test = data[testIndex:nrow(dataLagDay),]
# train
# test




hist(as.numeric(data$AveragedExchange))
data <- data.matrix(data)
#data = normalize(data)




# The code for the data generator you’ll use is below. It yields a list (samples, targets), where samples is one batch of input data and targets is the corresponding array of target temperatures. It takes the following arguments:
#   
#   data — The original array of floating-point data, which you normalized in listing 6.32.
# lookback — How many timesteps back the input data should go.
# delay — How many timesteps in the future the target should be.
# min_index and max_index — Indices in the data array that delimit which timesteps to draw from. This is useful for keeping a segment of the data for validation and another for testing.
# shuffle — Whether to shuffle the samples or draw them in chronological order.
# batch_size — The number of samples per batch.
# step — The period, in timesteps, at which you sample data. You’ll set it 6 in order to draw one data point every hour.


generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows), 
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,which( colnames(data)=="AveragedExchange" )]
    }            
    
    list(samples, targets)
  }
}

data[,which( colnames(data)=="AveragedExchange" )]
targetColumn
data
# The i variable contains the state that tracks next window of data to return, so it is updated using superassignment
# (e.g. i <<- i + length(rows)).
# 
# Now, let’s use the abstract generator function to instantiate three generators: one for training, one for validation, and one for 
# testing. Each will look at different temporal segments of the original data: the training generator looks at the first 200,000 
# timesteps, the validation generator looks at the following 100,000, and the test generator looks at the remainder.

# lookback = 480 — Observations will go back 10 days.
# steps = 2 — Observations will be sampled at one data point per hour. (we pull 2x per hour...1 hour = 2 samples)
# delay = 48 — we sample every 30 minutes, so 24*2= 1 day in the future

lookback <- 500

step <- 200
delay <- 20
batch_size <- 150


trainIndex
testIndex
# 
#  train = data[1:trainIndex,]
#  test = data[testIndex:nrow(dataLagDay),]
# train
# test


train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = trainIndex,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)


val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = trainIndex+1,
  max_index = nrow(data),
  step = step,
  batch_size = batch_size
)

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = trainIndex+1,
  max_index = nrow(data),
  step = step,
  batch_size = batch_size
)

pred_gen <- generator(
  data,
  lookback = 1,
  delay = 1,
  min_index = nrow(data)-100,
  max_index = nrow(data),
  step = 1,
  batch_size = 1
)
pred_gen()[[2]]
pred_gen()[[1]][,,103][1]

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- (nrow(data) - 1 - lookback) / batch_size

# How many steps to draw from test_gen in order to see the entire test set
test_steps <- (nrow(data) - 1 - lookback) / batch_size



model <- keras_model_sequential() %>% 
  layer_gru(units = dim(data)[[2]]-1, 
            dropout = 0.1, 
            recurrent_dropout = 0.5,
            return_sequences = TRUE,
            input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_gru(units = 25, activation = "relu",
            dropout = 0.1,
            recurrent_dropout = 0.5) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_nadam(),
  loss = "mae",
  metrics = "accuracy"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs =10,
  validation_data = val_gen,
  validation_steps = val_steps
)

model %>% evaluate(test_gen()[[1]],test_gen()[[2]])
model %>% evaluate(val_gen()[[1]],val_gen()[[2]])

testing = test_gen()
x =as.data.frame(as.integer(round(model %>% predict(testing[[1]]), digits = 0)))
y =as.data.frame(as.integer(testing[[2]]))
testing[[1]]

data
resid = x-y

answ = data.frame(x,y,resid)

resid

model %>% predict(testing[[1]])

data[[1]]

targetColumn
features = data[,1:ncol(data)-1]

features
predout = pred_gen()

test = predout[[1]]
testing = as.data.frame(test)
as.list(test)
as.data.frame(predout[[1]])
predout[[2]]
testing[[2]]
training =train_gen()

training[[2]]
predict(model,samples)

val =val_gen()
val[[2]]
