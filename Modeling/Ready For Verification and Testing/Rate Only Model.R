library(caret)
library(keras)

################################################################
data = read.csv(gzfile("finalRate.csv.gz"))
#data = read.csv("rateRaw.csv")
#data = read.csv(gzfile("finalValue.csv.gz"))
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

# data = read.csv(gzfile("finalRate.csv.gz"))
# #---> data = read.csv("rateRaw.csv")
# 
# data = data[,-2]
# 
# #data=data[,3:ncol(data)-1]
# 
# data2 = read.csv(gzfile("finalValue.csv.gz"))
# #---> data2 = read.csv("valueRaw.csv")
# data2 = data2[,-2]
# data2 = data2[,-ncol(data2)]
# data2 = data2[-1,]
# #data2 = data2[ , apply(data2, 2, function(x) !any(is.na(x)))]
# #data2=data2[,3:ncol(data2)]
# 
# data3 = merge(data, data2, by.x = 1, by.y = 1)
# data3 = data3[,-1]
# 
# data = data3
#data = data[ , apply(data, 2, function(x) !any(is.na(x)))]

targetColumn = which( colnames(data)=="AveragedExchange" )
#------------------> experimental merging




#data[,targetColumn] = round(data[,targetColumn] , digits = 1)


#data[,targetColumn] = as.factor(as.numeric(as.character(data[,targetColumn])))



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
      targets[[j]] <- data[rows[[j]] + delay,targetColumn]
    }            
    
    list(samples, targets)
  }
}

# The i variable contains the state that tracks next window of data to return, so it is updated using superassignment
# (e.g. i <<- i + length(rows)).
# 
# Now, let’s use the abstract generator function to instantiate three generators: one for training, one for validation, and one for 
# testing. Each will look at different temporal segments of the original data: the training generator looks at the first 200,000 
# timesteps, the validation generator looks at the following 100,000, and the test generator looks at the remainder.

# lookback = 480 — Observations will go back 10 days.
# steps = 2 — Observations will be sampled at one data point per hour. (we pull 2x per hour...1 hour = 2 samples)
# delay = 48 — we sample every 30 minutes, so 24*2= 1 day in the future

lookback <- 100

step <- 20
delay <- 2
batch_size <- 15

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 2000,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

train_gen()[[2]]
val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 2001,
  max_index = nrow(data),
  step = step,
  batch_size = batch_size
)

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1000,
  max_index = nrow(data),
  step = step,
  batch_size = batch_size
)

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- (nrow(data) - 2001 - lookback) / batch_size

# How many steps to draw from test_gen in order to see the entire test set
test_steps <- (nrow(data) - 1000 - lookback) / batch_size

model <- keras_model_sequential() %>% 
  layer_gru(units = dim(data)[[2]]-4, 
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
  epochs = 10,
  validation_data = val_gen,
  validation_steps = val_steps
)

model %>% evaluate(test_gen()[[1]],test_gen()[[2]])
model %>% evaluate(val_gen()[[1]],val_gen()[[2]])

testing = test_gen()
x =as.data.frame(as.integer(round(model %>% predict(testing[[1]]), digits = 0)))
y =as.data.frame(as.integer(testing[[2]]))

resid = x-y

resid