library(h2o)
library(dplyr)


data = read.csv(gzfile("finalValue.csv.gz"))
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

# 
data2 = read.csv(gzfile("finalRate.csv.gz"))

#data2 = data2[,-3]
data2 = data2[,-ncol(data2)]

data2 = data2[,which( colnames(data2)=="article" ):ncol(data2)]
data2 = data2[ , apply(data2, 2, function(x) !any(is.na(x)))]


data3 = cbind(data, data2)
# data3 = data3[,-1]
# 
data = data3
#data = data[ , apply(data, 2, function(x) !any(is.na(x)))]

targetColumn = which( colnames(data)=="AveragedExchange" )
#nzv <- nearZeroVar(data)
#data <- data[,-nzv]


#data = na.omit(data)
#data = as.numeric(data)
#data$AveragedExchange =as.factor(data$AveragedExchange)

data = data[ , colSums(is.na(data)) == 0]

#-------------------------->H2O One Day Model ---------------------------------
dataLagDay = cbind(data[,-targetColumn],lead(data$AveragedExchange, n=48))
colnames(dataLagDay)[ncol(dataLagDay)] = c("AveragedExchange")

dataLagDay = na.omit(dataLagDay)

trainIndex = nrow(dataLagDay) *.70
testIndex = nrow(dataLagDay)-trainIndex

train_tbl = dataLagDay[1:trainIndex,]
valid_tbl = dataLagDay[testIndex:nrow(dataLagDay),]
test_tbl = dataLagDay[testIndex:nrow(dataLagDay),]
# Split into training, validation and test sets


h2o.init()        # Fire up h2o

#h2o.no_progress() # Turn off progress bars

train_h2o <- as.h2o(train_tbl)
valid_h2o <- as.h2o(valid_tbl)
test_h2o  <- as.h2o(test_tbl)

# Set names for h2o
y <- "AveragedExchange"
x <- setdiff(names(train_h2o), y)

# linear regression model used, but can use any model
automl_models_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o, 
  leaderboard_frame = test_h2o, 
  max_runtime_secs = 180, 
  stopping_metric = "AUTO")

# Extract leader model
automl_leader <- automl_models_h2o@leader

pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

h2o.performance(automl_leader, newdata = test_h2o)

variableImportance = as.data.frame(h2o.varimp(automl_leader))

h2o.r2(automl_leader, train = FALSE, valid = T, xval = F)

h2o.saveModel(object=automl_leader, path="Modeling/trainedModels/h20ModelOneDay", force=TRUE)

#-------------------------->H2O Two Day Model ---------------------------------
dataLagDay = cbind(data[,-targetColumn],lead(data$AveragedExchange, n=96))
colnames(dataLagDay)[ncol(dataLagDay)] = c("AveragedExchange")

dataLagDay = na.omit(dataLagDay)

trainIndex = nrow(dataLagDay) *.70
testIndex = nrow(dataLagDay)-trainIndex

train_tbl = dataLagDay[1:trainIndex,]
valid_tbl = dataLagDay[testIndex:nrow(dataLagDay),]
test_tbl = dataLagDay[testIndex:nrow(dataLagDay),]
# Split into training, validation and test sets


h2o.init()        # Fire up h2o

#h2o.no_progress() # Turn off progress bars

train_h2o <- as.h2o(train_tbl)
valid_h2o <- as.h2o(valid_tbl)
test_h2o  <- as.h2o(test_tbl)

# Set names for h2o
y <- "AveragedExchange"
x <- setdiff(names(train_h2o), y)

# linear regression model used, but can use any model
automl_models_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o, 
  leaderboard_frame = test_h2o, 
  max_runtime_secs = 360, 
  stopping_metric = "AUTO")

# Extract leader model
automl_leader <- automl_models_h2o@leader

pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

h2o.performance(automl_leader, newdata = test_h2o)

variableImportance = as.data.frame(h2o.varimp(automl_leader))

h2o.r2(automl_leader, train = FALSE, valid = T, xval = F)

h2o.saveModel(object=automl_leader, path="Modeling/trainedModels/h20ModelTwoDay", force=TRUE)


#write.csv(variableImportance, "variableImportance.csv")
# 
# # Investigate test error
# error_tbl <- data %>% 
#   filter(lubridate::year(date) == 2017) %>%
#   add_column(pred = pred_h2o %>% as.tibble() %>% pull(predict)) %>%
#   rename(actual = price) %>%
#   mutate(
#     error     = actual - pred,
#     error_pct = error / actual
#   ) 
# error_tbl
# 
# error_tbl %>%
#   summarise(
#     me   = mean(error),
#     rmse = mean(error^2)^0.5,
#     mae  = mean(abs(error)),
#     mape = mean(abs(error_pct)),
#     mpe  = mean(error_pct)
#   ) %>%
#   glimpse()
# 
