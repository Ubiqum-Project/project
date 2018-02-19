# Clear workspace
rm(list=ls())

# Load libraries

require(rnn)

# Set seed for reproducibility purposes
set.seed(10)

# Set frequency
f <- 5
w <- 2*pi*f

# Create sequences
t <- seq(0.005,2,by=0.005)
x <- sin(t*w) + rnorm(200, 0, 0.25)
y <- cos(t*w)

# Samples of 20 time series
X <- matrix(x, nrow = 40)
Y <- matrix(y, nrow = 40)

# Plot noisy waves
plot(as.vector(X), col='blue', type='l', ylab = "X,Y", main = "Noisy waves")
lines(as.vector(Y), col = "red")
legend("topright", c("X", "Y"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))

# Standardize in the interval 0 - 1
X <- (X - min(X)) / (max(X) - min(X))
Y <- (Y - min(Y)) / (max(Y) - min(Y))

# Transpose
X <- t(X)
Y <- t(Y)

# Training-testing sets
train <- 1:8
test <- 9:10

# Train model. Keep out the last two sequences.
model <- trainr(Y = Y[train,],
                X = X[train,],
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

