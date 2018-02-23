library(caret)
################################################################
#data = read.csv(gzfile("finalRate.csv.gz"))
#data = read.csv("rateRaw.csv")
data = read.csv(gzfile("finalValue.csv.gz"))
data = data
data$AveragedExchange = as.numeric(data$AveragedExchange) 
str(data$AveragedExchange)
hist(data$AveragedExchange)
data$AveragedExchange[data$AveragedExchange<= -.5] = -1
hist(data$AveragedExchange)
data$AveragedExchange[data$AveragedExchange> -.5 & data$AveragedExchange< 0] = -.5
hist(data$AveragedExchange)
data$AveragedExchange[data$AveragedExchange==0] = 0
hist(data$AveragedExchange)
data$AveragedExchange[data$AveragedExchange>= .5] = .5
hist(data$AveragedExchange)
data$AveragedExchange[data$AveragedExchange< .5 & data$AveragedExchange> 0] = 1
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


# lookback = 480 — Observations will go back 10 days.
# steps = 2 — Observations will be sampled at one data point per hour. (we pull 2x per hour...1 hour = 2 samples)
# delay = 48 — we sample every 30 minutes, so 24*2= 1 day in the future


# First, you’ll convert the R data frame which we read earlier into a matrix of floating point values 
# 


#data[,targetColumn] = round(data[,targetColumn] , digits = 1)


#data[,targetColumn] = as.factor(as.numeric(as.character(data[,targetColumn])))



hist(as.numeric(data$AveragedExchange))
data <- data.matrix(data)
#data = normalize(data)


# You’ll then preprocess the data by subtracting the mean of each time series and dividing by the standard deviation. 
# You’re going to use the first 200,000 timesteps as training data, so compute the mean and standard deviation for 
# normalization only on this fraction of the data. (Already did this step)

# train_data <- data[1:1700,]
# mean <- apply(train_data, 2, mean)
# std <- apply(train_data, 2, sd)
# data <- scale(data, center = mean, scale = std)

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

lookback <- 48

step <- 20
delay <- 4
batch_size <- 10

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 1800,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

train_gen()[[2]]
val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1801,
  max_index = 2100,
  step = step,
  batch_size = batch_size
)

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 2101,
  max_index = nrow(data),
  step = step,
  batch_size = batch_size
)

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- (2000 - 1601 - lookback) / batch_size

# How many steps to draw from test_gen in order to see the entire test set
test_steps <- (nrow(data) - 2001 - lookback) / batch_size

train_gen()[[2]]

#--------------------------------Setting up NN --------------------------------------------------------------

# In the same way that it’s useful to establish a common-sense baseline before trying machine-learning approaches, 
# it’s useful to try simple, cheap machine-learning models (such as small, densely connected networks) before looking 
# into complicated and computationally expensive models such as RNNs. This is the best way to make sure any further complexity 
# you throw at the problem is legitimate and delivers real benefits.
# 
# The following listing shows a fully connected model that starts by flattening the data and then runs it through two dense layers. 
# Note the lack of activation function on the last dense layer, which is typical for a regression problem. You use MAE as the loss. 
# Because you evaluate on the exact same data and with the exact same metric you did with the common-sense approach, 
# the results will be directly comparable.

library(keras)

model <- keras_model_sequential() %>% 
  layer_batch_normalization(input_shape = c(lookback / step, dim(data)[-1]))%>%
  layer_flatten() %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae",
  metrics = "accuracy"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 50,
  epochs = 2,
  validation_data = val_gen,
  validation_steps = val_steps
)
plot(history)

# Gated Recurrent Network

# The first fully connected approach didn’t do well, but that doesn’t mean machine learning isn’t applicable to this problem. 
# The previous approach first flattened the time series, which removed the notion of time from the input data. Let’s instead look 
# at the data as what it is: a sequence, where causality and order matter. You’ll try a recurrent-sequence processing model – 
# it should be the perfect fit for such sequence data, precisely because it exploits the temporal ordering of data points, 
# unlike the first approach.
# 
# Instead of the LSTM layer introduced in the previous section, you’ll use the GRU layer, developed by Chung et al. in 2014. 
# Gated recurrent unit (GRU) layers work using the same principle as LSTM, but they’re somewhat streamlined and thus cheaper 
# to run (although they may not have as much representational power as LSTM). This trade-off between computational expensiveness 
# and representational power is seen everywhere in machine learning.

model <- keras_model_sequential() %>% 
  layer_gru(units = 32, input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 5,
  epochs = 2,
  validation_data = val_gen,
  validation_steps = val_steps
)
plot(history)

# It’s evident from the training and validation curves that the model is overfitting: the training and validation losses start 
# to diverge considerably after a few epochs. You’re already familiar with a classic technique for fighting this phenomenon: dropout,
# which randomly zeros out input units of a layer in order to break happenstance correlations in the training data that the layer is 
# exposed to. But how to correctly apply dropout in recurrent networks isn’t a trivial question. It has long been known that applying 
# dropout before a recurrent layer hinders learning rather than helping with regularization. In 2015, Yarin Gal, as part of his PhD 
# thesis on Bayesian deep learning, determined the proper way to use dropout with a recurrent network: the same dropout mask (the same 
# pattern of dropped units) should be applied at every timestep, instead of a dropout mask that varies randomly from timestep to
# timestep. What’s more, in order to regularize the representations formed by the recurrent gates of layers such as layer_gru and 
# layer_lstm, a temporally constant dropout mask should be applied to the inner recurrent activations of the layer (a recurrent dropout 
#  mask). Using the same dropout mask at every timestep allows the network to properly propagate its learning error through time;
# a temporally random dropout mask would disrupt this error signal and be harmful to the learning process.
# 
# Yarin Gal did his research using Keras and helped build this mechanism directly into Keras recurrent layers.
# Every recurrent layer in Keras has two dropout-related arguments: dropout, a float specifying the dropout rate for input
# units of the layer, and recurrent_dropout, specifying the dropout rate of the recurrent units. Let’s add dropout and recurrent
# dropout to the layer_gru and see how doing so impacts overfitting. Because networks being regularized with dropout always take
# longer to fully converge, you’ll train the network for twice as many epochs.

model <- keras_model_sequential() %>% 
  layer_gru(units = 32, dropout = 0.2, recurrent_dropout = 0.2,
            input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae",
  metrics = "accuracy"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 5,
  validation_data = val_gen,
  validation_steps = val_steps
)


# Because you’re no longer overfitting but seem to have hit a performance bottleneck, you should consider increasing the capacity of the network. Recall the description of the universal machine-learning workflow: 
#   it’s generally a good idea to increase the capacity of your network until overfitting becomes the primary obstacle (assuming you’re 
#    already taking basic steps to mitigate overfitting, such as using dropout). As long as you aren’t overfitting too badly,
# you’re likely under capacity.
# 
# Increasing network capacity is typically done by increasing the number of units in the layers or adding more layers.
# Recurrent layer stacking is a classic way to build more-powerful recurrent networks: for instance, what currently powers
# the Google Translate algorithm is a stack of seven large LSTM layers – that’s huge.
# 
# To stack recurrent layers on top of each other in Keras, all intermediate layers should return their full sequence of
# outputs (a 3D tensor) rather than their output at the last timestep. This is done by specifying return_sequences = TRUE.

model <- keras_model_sequential() %>% 
  layer_gru(units = 45, 
            dropout = 0.1, 
            recurrent_dropout = 0.5,
            return_sequences = TRUE,
            input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_gru(units = 64, activation = "relu",
            dropout = 0.1,
            recurrent_dropout = 0.5) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae",
  metrics = "accuracy"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)
model %>% evaluate(test_gen()[[1]],test_gen()[[2]])
model %>% evaluate(val_gen()[[1]],val_gen()[[2]])
model %>% predict(test_gen()[[1]])
test_gen()[[2]]
# 
# The last technique introduced in this section is called bidirectional RNNs. A bidirectional RNN is a common RNN variant 
# that can offer greater performance than a regular RNN on certain tasks. It’s frequently used in natural-language processing – 
# you could call it the Swiss Army knife of deep learning for natural-language processing.
# 
# RNNs are notably order dependent, or time dependent: they process the timesteps of their input sequences in order, and 
# shuffling or reversing the timesteps can completely change the representations the RNN extracts from the sequence. This 
# is precisely the reason they perform well on problems where order is meaningful, such as the temperature-forecasting problem. 
# A bidirectional RNN exploits the order sensitivity of RNNs: it consists of using two regular RNNs, such as the layer_gru 
# and layer_lstm you’re already familiar with, each of which processes the input sequence in one direction (chronologically 
#  and antichronologically), and then merging their representations. By processing a sequence both ways, a bidirectional RNN 
# can catch patterns that may be overlooked by a unidirectional RNN.
# 
# Remarkably, the fact that the RNN layers in this section have processed sequences in chronological order (older timesteps 
#    first) may have been an arbitrary decision. At least, it’s a decision we made no attempt to question so far. 
# Could the RNNs have performed well enough if they processed input sequences in antichronological order, for instance 
# (newer timesteps first)? Let’s try this in practice and see what happens. All you need to do is write a variant of the 
# data generator where the input sequences are reverted along the time dimension (replace the last line with 
#  list(samples[,ncol(samples):1,], targets)). Training the same one-GRU-layer network that you used in the first experiment
# in this section, you get the results shown below.
# 
# The reversed-order GRU underperforms even the common-sense baseline, indicating that in this case, chronological processing 
# is important to the success of your approach. This makes perfect sense: the underlying GRU layer will typically be better at
# remembering the recent past than the distant past, and naturally the more recent weather data points are more predictive than 
# older data points for the problem (that’s what makes the common-sense baseline fairly strong). Thus the chronological version 
# of the layer is bound to outperform the reversed-order version. Importantly, this isn’t true for many other problems, 
# including natural language: intuitively, the importance of a word in understanding a sentence isn’t usually dependent 
# on its position in the sentence. Let’s try the same trick on the LSTM IMDB example from section 6.2.

model <- keras_model_sequential() %>% 
  
  bidirectional(
    layer_gru(units = dim(data)[[-1]]), input_shape = list(NULL, dim(data)[[-1]])
  ) %>% 
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1) %>%
  layer_dropout(rate = 0.2)

model %>% compile(
  optimizer = optimizer_nadam(),
  loss = "mae",
  metrics = "accuracy"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 5,
  validation_data = val_gen,
  validation_steps = val_steps
)

model %>% evaluate(test_gen()[[1]],test_gen()[[2]])
model %>% evaluate(val_gen()[[1]],val_gen()[[2]])
model %>% predict(test_gen()[[1]])
train_gen()[[2]]
plot(history)
########################################################################################

