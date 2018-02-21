# Clear workspace
rm(list=ls())

# Load libraries

require(rnn)


data = read.csv(gzfile("finalRate.csv.gz"))
data = data[ , apply(data, 2, function(x) !any(is.na(x)))]
data=data[,3:ncol(data)]
data$AveragedExchange = as.numeric(as.character(data$AveragedExchange))

test = data
test$bin = as.numeric(cut2(data$AveragedExchange, g=20))

data$AveragedExchange = as.numeric(cut2(data$AveragedExchange, g=20))


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
#x_train <- array_reshape(x_train, c(nrow(x_train), 93))
#x_test <- array_reshape(x_test, c(nrow(x_test), 93))

# Train model. Keep out the last two sequences.
model <- trainr(Y = y_train,
                X = x_train,
                learningrate = 0.05,
                hidden_dim = 16,
                numepochs = 10)

# Predicted values
Yp <- predictr(model, X)

# Plot predicted vs actual. Training set + testing set
plot(as.vector(t(Y)), col = 'red', type = 'l', main = "Actual vs predicted", ylab = "Y,Yp")
lines(as.vector(t(Yp)), type = 'l', col = 'blue')
legend("topright", c("Predicted", "Real"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))

# Plot predicted vs actual. Testing set only.
plot(as.vector(t(Y[test,])), col = 'red', type='l', main = "Actual vs predicted: testing set", ylab = "Y,Yp")
lines(as.vector(t(Yp[test,])), type = 'l', col = 'blue')
legend("topright", c("Predicted", "Real"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))

