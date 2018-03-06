# RF Model Building With Caret
library(C50)
library(nnet)
library(caret)
library(ggplot2)
library(caret)
library(e1071)
library(h2o)

library(dplyr)

library(ranger)


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



##########################################################################################################
#####                                          H2O    Model                                          #####
#####                                                                                                #####
##########################################################################################################

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

# #-------------------------->H2O Two Day Model ---------------------------------
# dataLagDay = cbind(data[,-targetColumn],lead(data$AveragedExchange, n=96))
# colnames(dataLagDay)[ncol(dataLagDay)] = c("AveragedExchange")
# 
# dataLagDay = na.omit(dataLagDay)
# 
# trainIndex = nrow(dataLagDay) *.70
# testIndex = nrow(dataLagDay)-trainIndex
# 
# train_tbl = dataLagDay[1:trainIndex,]
# valid_tbl = dataLagDay[testIndex:nrow(dataLagDay),]
# test_tbl = dataLagDay[testIndex:nrow(dataLagDay),]
# # Split into training, validation and test sets
# 
# 
# h2o.init()        # Fire up h2o
# 
# #h2o.no_progress() # Turn off progress bars
# 
# train_h2o <- as.h2o(train_tbl)
# valid_h2o <- as.h2o(valid_tbl)
# test_h2o  <- as.h2o(test_tbl)
# 
# # Set names for h2o
# y <- "AveragedExchange"
# x <- setdiff(names(train_h2o), y)
# 
# # linear regression model used, but can use any model
# automl_models_h2o <- h2o.automl(
#   x = x, 
#   y = y, 
#   training_frame = train_h2o, 
#   validation_frame = valid_h2o, 
#   leaderboard_frame = test_h2o, 
#   max_runtime_secs = 360, 
#   stopping_metric = "AUTO")
# 
# # Extract leader model
# automl_leader <- automl_models_h2o@leader
# 
# pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)
# 
# h2o.performance(automl_leader, newdata = test_h2o)
# 
# variableImportance = as.data.frame(h2o.varimp(automl_leader))
# 
# h2o.r2(automl_leader, train = FALSE, valid = T, xval = F)
# 
# h2o.saveModel(object=automl_leader, path="Modeling/trainedModels/h20ModelTwoDay", force=TRUE)






##########################################################################################################
#####                                          Ranger Model                                          #####
#####                                                                                                #####
##########################################################################################################

# 
# #---------------Lag data by an hour -------------------------------------------
# dataLagHour = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=2))
# colnames(dataLagHour)[ncol(dataLagHour)] = c("AveragedExchange")
# 
# dataLagHour = na.omit(dataLagHour)
# 
# trainIndex = nrow(dataLagHour) *.70
# testIndex = nrow(dataLagHour)-trainIndex
# 
# train = dataLagHour[1:trainIndex,]
# test = dataLagHour[testIndex:nrow(dataLagHour),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagHourModelRanger <- train( as.factor(AveragedExchange) ~ .,
#                        data = dataLagHour,
#                        method = "ranger",
#                        preProc = c("center", "scale"),
#                        # trControl = numFolds,
#                        trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagHourModelRanger.rds")
# 
# #-------------------------------End of Hour Data Lag ----------------------------------------------
# 
# 
# #---------------Lag data by a half day -------------------------------------------
# dataLagHalfDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=24))
# colnames(dataLagHalfDay)[ncol(dataLagHalfDay)] = c("AveragedExchange")
# 
# dataLagHalfDay = na.omit(dataLagHalfDay)
# 
# trainIndex = nrow(dataLagHalfDay) *.70
# testIndex = nrow(dataLagHalfDay)-trainIndex
# 
# train = dataLagHalfDay[1:trainIndex,]
# test = dataLagHalfDay[testIndex:nrow(dataLagHalfDay),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagHalfDayModelRanger <- train( as.factor(AveragedExchange) ~ .,
#                            data = dataLagHalfDay,
#                            method = "ranger",
#                            preProc = c("center", "scale"),
#                            # trControl = numFolds,
#                            trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagHalfDayModelRanger.rds")
# 
# #-------------------------------End of half day Data Lag ----------------------------------------------
# 
# 
# 


#---------------Lag data by a  day -------------------------------------------
dataLagDay = cbind(data[,-targetColumn],lead(data$AveragedExchange, n=48))
colnames(dataLagDay)[ncol(dataLagDay)] = c("AveragedExchange")

dataLagDay = na.omit(dataLagDay)

trainIndex = nrow(dataLagDay) *.70
testIndex = nrow(dataLagDay)-trainIndex

train = dataLagDay[1:trainIndex,]
test = dataLagDay[testIndex:nrow(dataLagDay),]

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 960,  #  <-----------  20 days
                              horizon = 96,         #  <-----------  2 days
                              fixedWindow = TRUE)


#numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
dataLagDayModelRanger <- train( as.factor(AveragedExchange) ~ .,
                              data = dataLagDay,
                              method = "ranger",
                              preProc = c("center", "scale"),
                              # trControl = numFolds,
                              trControl = myTimeControl)


#pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95


saveRDS(dataLagDayModelRanger, "Modeling/trainedModels/dataLagDayModelRanger2.rds")
saveRDS(dataLagDayModelRanger, "Ranger_Day_Shift")
#-------------------------------End of day Data Lag ----------------------------------------------


# 
# #---------------Lag data by a day and a half -------------------------------------------
# dataLagDayHalf = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=72))
# colnames(dataLagDayHalf)[ncol(dataLagDayHalf)] = c("AveragedExchange")
# 
# dataLagDayHalf = na.omit(dataLagDayHalf)
# 
# trainIndex = nrow(dataLagDayHalf) *.70
# testIndex = nrow(dataLagDayHalf)-trainIndex
# 
# train = dataLagDayHalf[1:trainIndex,]
# test = dataLagDayHalf[testIndex:nrow(dataLagDayHalf),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagDayHalfModelRanger <- train( as.factor(AveragedExchange) ~ .,
#                               data = dataLagDayHalf,
#                               method = "ranger",
#                               preProc = c("center", "scale"),
#                               # trControl = numFolds,
#                               trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagDayHalfModelRanger.rds")

#-------------------------------End of day and half Data Lag ----------------------------------------------




# #---------------Lag data by a 2 dayand a half -------------------------------------------
# dataLagTwoDay = cbind(data[,-targetColumn],lead(data$AveragedExchange, n=96))
# colnames(dataLagTwoDay)[ncol(dataLagTwoDay)] = c("AveragedExchange")
# 
# dataLagTwoDay = na.omit(dataLagTwoDay)
# 
# trainIndex = nrow(dataLagTwoDay) *.70
# testIndex = nrow(dataLagTwoDay)-trainIndex
# 
# train = dataLagTwoDay[1:trainIndex,]
# test = dataLagTwoDay[testIndex:nrow(dataLagTwoDay),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagTwoDayModelRanger <- train( as.factor(AveragedExchange) ~ .,
#                               data = dataLagTwoDay,
#                               method = "svm",
#                               preProc = c("center", "scale"),
#                               # trControl = numFolds,
#                               trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# 
# #-------------------------------End of 2 day Data Lag ----------------------------------------------
# dataLagHourModelRanger
# dataLagHalfDayModelRanger
# dataLagDayModelRanger
# dataLagDayHalfModelRanger
# dataLagTwoDayModelRanger
# 
# 
# saveRDS(dataLagTwoDayModelRanger, "Modeling/trainedModels/dataLagTwoDayModelRanger2.rds")



##########################################################################################################
#####                                          C50 Model                                             #####
#####                                                                                                #####
##########################################################################################################


# 
# #---------------Lag data by an hour -------------------------------------------
# dataLagHour = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=2))
# colnames(dataLagHour)[ncol(dataLagHour)] = c("AveragedExchange")
# 
# dataLagHour = na.omit(dataLagHour)
# 
# trainIndex = nrow(dataLagHour) *.70
# testIndex = nrow(dataLagHour)-trainIndex
# 
# train = dataLagHour[1:trainIndex,]
# test = dataLagHour[testIndex:nrow(dataLagHour),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagHourModelc50 <- train( as.factor(AveragedExchange) ~ .,
#                                  data = dataLagHour,
#                                  method = "C5.0",
#                                  preProc = c("center", "scale"),
#                                  # trControl = numFolds,
#                                  trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagHourModelc50.rds")
# 
# 
# #-------------------------------End of Hour Data Lag ----------------------------------------------
# 
# 
# 
# 
# #---------------Lag data by a  day -------------------------------------------
# dataLagHalfDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=24))
# colnames(dataLagHalfDay)[ncol(dataLagHalfDay)] = c("AveragedExchange")
# 
# dataLagHalfDay = na.omit(dataLagHalfDay)
# 
# trainIndex = nrow(dataLagHalfDay) *.70
# testIndex = nrow(dataLagHalfDay)-trainIndex
# 
# train = dataLagHalfDay[1:trainIndex,]
# test = dataLagHalfDay[testIndex:nrow(dataLagHalfDay),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagHalfDayModelc50 <- train( as.factor(AveragedExchange) ~ .,
#                                     data = dataLagHalfDay,
#                                     method = "C5.0",
#                                     preProc = c("center", "scale"),
#                                     # trControl = numFolds,
#                                     trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagHalfDayModelc50.rds")
# 
# 
# #-------------------------------End of half day Data Lag ----------------------------------------------
# 


#---------------Lag data by a  day -------------------------------------------
dataLagDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=48))
colnames(dataLagDay)[ncol(dataLagDay)] = c("AveragedExchange")

dataLagDay = na.omit(dataLagDay)

trainIndex = nrow(dataLagDay) *.70
testIndex = nrow(dataLagDay)-trainIndex

train = dataLagDay[1:trainIndex,]
test = dataLagDay[testIndex:nrow(dataLagDay),]

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 960,  #  <-----------  20 days
                              horizon = 96,         #  <-----------  2 days
                              fixedWindow = TRUE)


#numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
dataLagDayModelc50 <- train( as.factor(AveragedExchange) ~ .,
                                data = dataLagDay,
                                method = "C5.0",
                                preProc = c("center", "scale"),
                                # trControl = numFolds,
                                trControl = myTimeControl)


#pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95

saveRDS(dataLagDayModelc50, "Modeling/trainedModels/dataLagDayModelc502.rds")

dataLagDayModelc50
#-------------------------------End of day Data Lag ----------------------------------------------


# 
# #---------------Lag data by a day and a half -------------------------------------------
# dataLagDayHalf = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=72))
# colnames(dataLagDayHalf)[ncol(dataLagDayHalf)] = c("AveragedExchange")
# 
# dataLagDayHalf = na.omit(dataLagDayHalf)
# 
# trainIndex = nrow(dataLagDayHalf) *.70
# testIndex = nrow(dataLagDayHalf)-trainIndex
# 
# train = dataLagDayHalf[1:trainIndex,]
# test = dataLagDayHalf[testIndex:nrow(dataLagDayHalf),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagDayHalfModelc50 <- train( as.factor(AveragedExchange) ~ .,
#                                     data = dataLagDayHalf,
#                                     method = "C5.0",
#                                     preProc = c("center", "scale"),
#                                     # trControl = numFolds,
#                                     trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagDayHalfModelc50.rds")
# 

#-------------------------------End of day and half Data Lag ----------------------------------------------




# #---------------Lag data by a 2 dayand a half -------------------------------------------
# dataLagTwoDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=96))
# colnames(dataLagTwoDay)[ncol(dataLagTwoDay)] = c("AveragedExchange")
# 
# dataLagTwoDay = na.omit(dataLagTwoDay)
# 
# trainIndex = nrow(dataLagTwoDay) *.70
# testIndex = nrow(dataLagTwoDay)-trainIndex
# 
# train = dataLagTwoDay[1:trainIndex,]
# test = dataLagTwoDay[testIndex:nrow(dataLagTwoDay),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagTwoDayModelc50 <- train( as.factor(AveragedExchange) ~ .,
#                                    data = dataLagTwoDay,
#                                    method = "svm",
#                                    preProc = c("center", "scale"),
#                                    # trControl = numFolds,
#                                    trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# saveRDS(dataLagTwoDayModelc50, "Modeling/trainedModels/dataLagTwoDayModelc50.rds")
# 
# #-------------------------------End of 2 day Data Lag ----------------------------------------------
# 
# 
# 
# 




##########################################################################################################
#####                                          RF Model                                             #####
#####                                                                                                #####
##########################################################################################################


# 
# #---------------Lag data by an hour -------------------------------------------
# dataLagHour = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=2))
# colnames(dataLagHour)[ncol(dataLagHour)] = c("AveragedExchange")
# 
# dataLagHour = na.omit(dataLagHour)
# 
# trainIndex = nrow(dataLagHour) *.70
# testIndex = nrow(dataLagHour)-trainIndex
# 
# train = dataLagHour[1:trainIndex,]
# test = dataLagHour[testIndex:nrow(dataLagHour),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagHourModelRF <- train( as.factor(AveragedExchange) ~ .,
#                              data = dataLagHour,
#                              method = "rf",
#                              preProc = c("center", "scale"),
#                              # trControl = numFolds,
#                              trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagHourModelRF.rds")
# 
# #-------------------------------End of Hour Data Lag ----------------------------------------------
# 
# 
# 
# 
# #---------------Lag data by a  day -------------------------------------------
# dataLagHalfDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=24))
# colnames(dataLagHalfDay)[ncol(dataLagHalfDay)] = c("AveragedExchange")
# 
# dataLagHalfDay = na.omit(dataLagHalfDay)
# 
# trainIndex = nrow(dataLagHalfDay) *.70
# testIndex = nrow(dataLagHalfDay)-trainIndex
# 
# train = dataLagHalfDay[1:trainIndex,]
# test = dataLagHalfDay[testIndex:nrow(dataLagHalfDay),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagHalfDayModelRF <- train( as.factor(AveragedExchange) ~ .,
#                                 data = dataLagHalfDay,
#                                 method = "rf",
#                                 preProc = c("center", "scale"),
#                                 # trControl = numFolds,
#                                 trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagHalfDayModelRF.rds")
# #-------------------------------End of half day Data Lag ----------------------------------------------



#---------------Lag data by a  day -------------------------------------------
dataLagDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=48))
colnames(dataLagDay)[ncol(dataLagDay)] = c("AveragedExchange")

dataLagDay = na.omit(dataLagDay)

trainIndex = nrow(dataLagDay) *.70
testIndex = nrow(dataLagDay)-trainIndex

train = dataLagDay[1:trainIndex,]
test = dataLagDay[testIndex:nrow(dataLagDay),]

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 960,  #  <-----------  20 days
                              horizon = 96,         #  <-----------  2 days
                              fixedWindow = TRUE)


#numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
dataLagDayModelRF <- train( as.factor(AveragedExchange) ~ .,
                            data = dataLagDay,
                            method = "rf",
                            preProc = c("center", "scale"),
                            # trControl = numFolds,
                            trControl = myTimeControl)


#pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95


saveRDS(dataLagDayModelRF, "Modeling/trainedModels/dataLagDayModelRF2.rds")

dataLagDayModelRF
#-------------------------------End of day Data Lag ----------------------------------------------


# 
# #---------------Lag data by a day and a half -------------------------------------------
# dataLagDayHalf = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=72))
# colnames(dataLagDayHalf)[ncol(dataLagDayHalf)] = c("AveragedExchange")
# 
# dataLagDayHalf = na.omit(dataLagDayHalf)
# 
# trainIndex = nrow(dataLagDayHalf) *.70
# testIndex = nrow(dataLagDayHalf)-trainIndex
# 
# train = dataLagDayHalf[1:trainIndex,]
# test = dataLagDayHalf[testIndex:nrow(dataLagDayHalf),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagDayHalfModelRF <- train( as.factor(AveragedExchange) ~ .,
#                                 data = dataLagDayHalf,
#                                 method = "rf",
#                                 preProc = c("center", "scale"),
#                                 # trControl = numFolds,
#                                 trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagDayHalfModelRF.rds")
# #-------------------------------End of day and half Data Lag ----------------------------------------------
# 


# 
# #---------------Lag data by a 2 dayand a half -------------------------------------------
# dataLagTwoDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=96))
# colnames(dataLagTwoDay)[ncol(dataLagTwoDay)] = c("AveragedExchange")
# 
# dataLagTwoDay = na.omit(dataLagTwoDay)
# 
# trainIndex = nrow(dataLagTwoDay) *.70
# testIndex = nrow(dataLagTwoDay)-trainIndex
# 
# train = dataLagTwoDay[1:trainIndex,]
# test = dataLagTwoDay[testIndex:nrow(dataLagTwoDay),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagTwoDayModelRF <- train( as.factor(AveragedExchange) ~ .,
#                                data = dataLagTwoDay,
#                                method = "rf",
#                                preProc = c("center", "scale"),
#                                # trControl = numFolds,
#                                trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# 
# #-------------------------------End of 2 day Data Lag ----------------------------------------------
# dataLagHourModelRF
# dataLagHalfDayModelRF
# dataLagDayModelRF
# dataLagDayHalfModelRF
# dataLagTwoDayModelRF
# 
# 
# 
# saveRDS(dataLagTwoDayModelRF, "Modeling/trainedModels/dataLagTwoDayModelRF.rds")



##########################################################################################################
#####                                          KNN Model                                             #####
#####                                                                                                #####
##########################################################################################################



# #---------------Lag data by an hour -------------------------------------------
# dataLagHour = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=2))
# colnames(dataLagHour)[ncol(dataLagHour)] = c("AveragedExchange")
# 
# dataLagHour = na.omit(dataLagHour)
# 
# trainIndex = nrow(dataLagHour) *.70
# testIndex = nrow(dataLagHour)-trainIndex
# 
# train = dataLagHour[1:trainIndex,]
# test = dataLagHour[testIndex:nrow(dataLagHour),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagHourModelKNN <- train( as.factor(AveragedExchange) ~ .,
#                               data = dataLagHour,
#                               method = "knn",
#                               preProc = c("center", "scale"),
#                               # trControl = numFolds,
#                               trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagHourModelKNN.rds")
# 
# #-------------------------------End of Hour Data Lag ----------------------------------------------




# #---------------Lag data by a  day -------------------------------------------
# dataLagHalfDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=24))
# colnames(dataLagHalfDay)[ncol(dataLagHalfDay)] = c("AveragedExchange")
# 
# dataLagHalfDay = na.omit(dataLagHalfDay)
# 
# trainIndex = nrow(dataLagHalfDay) *.70
# testIndex = nrow(dataLagHalfDay)-trainIndex
# 
# train = dataLagHalfDay[1:trainIndex,]
# test = dataLagHalfDay[testIndex:nrow(dataLagHalfDay),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagHalfDayModelKNN <- train( as.factor(AveragedExchange) ~ .,
#                                  data = dataLagHalfDay,
#                                  method = "knn",
#                                  preProc = c("center", "scale"),
#                                  # trControl = numFolds,
#                                  trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagHalfDayModelKNN.rds")

#-------------------------------End of half day Data Lag ----------------------------------------------



#---------------Lag data by a  day -------------------------------------------
dataLagDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=48))
colnames(dataLagDay)[ncol(dataLagDay)] = c("AveragedExchange")

dataLagDay = na.omit(dataLagDay)

trainIndex = nrow(dataLagDay) *.70
testIndex = nrow(dataLagDay)-trainIndex

train = dataLagDay[1:trainIndex,]
test = dataLagDay[testIndex:nrow(dataLagDay),]

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 960,  #  <-----------  20 days
                              horizon = 96,         #  <-----------  2 days
                              fixedWindow = TRUE)


#numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
dataLagDayModelKNN <- train( as.factor(AveragedExchange) ~ .,
                             data = dataLagDay,
                             method = "knn",
                             preProc = c("center", "scale"),
                             # trControl = numFolds,
                             trControl = myTimeControl)


#pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95


saveRDS(dataLagDayModelKNN, "Modeling/trainedModels/dataLagDayModelKNN2.rds")

dataLagDayModelKNN

#-------------------------------End of day Data Lag ----------------------------------------------


# 
# #---------------Lag data by a day and a half -------------------------------------------
# dataLagDayHalf = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=72))
# colnames(dataLagDayHalf)[ncol(dataLagDayHalf)] = c("AveragedExchange")
# 
# dataLagDayHalf = na.omit(dataLagDayHalf)
# 
# trainIndex = nrow(dataLagDayHalf) *.70
# testIndex = nrow(dataLagDayHalf)-trainIndex
# 
# train = dataLagDayHalf[1:trainIndex,]
# test = dataLagDayHalf[testIndex:nrow(dataLagDayHalf),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagDayHalfModelKNN <- train( as.factor(AveragedExchange) ~ .,
#                                  data = dataLagDayHalf,
#                                  method = "knn",
#                                  preProc = c("center", "scale"),
#                                  # trControl = numFolds,
#                                  trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagDayHalfModelKNN.rds")
# 
# #-------------------------------End of day and half Data Lag ----------------------------------------------
# 


# 
# #---------------Lag data by a 2 dayand a half -------------------------------------------
# dataLagTwoDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=96))
# colnames(dataLagTwoDay)[ncol(dataLagTwoDay)] = c("AveragedExchange")
# 
# dataLagTwoDay = na.omit(dataLagTwoDay)
# 
# trainIndex = nrow(dataLagTwoDay) *.70
# testIndex = nrow(dataLagTwoDay)-trainIndex
# 
# train = dataLagTwoDay[1:trainIndex,]
# test = dataLagTwoDay[testIndex:nrow(dataLagTwoDay),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagTwoDayModelKNN <- train( as.factor(AveragedExchange) ~ .,
#                                 data = dataLagTwoDay,
#                                 method = "knn",
#                                 preProc = c("center", "scale"),
#                                 # trControl = numFolds,
#                                 trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# 
# saveRDS(dataLagTwoDayModelKNN, "Modeling/trainedModels/dataLagTwoDayModelKNN.rds")

#-------------------------------End of 2 day Data Lag ----------------------------------------------





##########################################################################################################
#####                                          GBM Model                                             #####
#####                                                                                                #####
##########################################################################################################

# 
# 
# #---------------Lag data by an hour -------------------------------------------
# dataLagHour = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=2))
# colnames(dataLagHour)[ncol(dataLagHour)] = c("AveragedExchange")
# 
# dataLagHour = na.omit(dataLagHour)
# 
# trainIndex = nrow(dataLagHour) *.70
# testIndex = nrow(dataLagHour)-trainIndex
# 
# train = dataLagHour[1:trainIndex,]
# test = dataLagHour[testIndex:nrow(dataLagHour),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagHourModelGBM <- train( as.factor(AveragedExchange) ~ .,
#                               data = dataLagHour,
#                               method = "gbm",
#                               preProc = c("center", "scale"),
#                               # trControl = numFolds,
#                               trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagHourModelGBM.rds")
# #-------------------------------End of Hour Data Lag ----------------------------------------------
# 
# 
# 
# 
# #---------------Lag data by a  day -------------------------------------------
# dataLagHalfDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=24))
# colnames(dataLagHalfDay)[ncol(dataLagHalfDay)] = c("AveragedExchange")
# 
# dataLagHalfDay = na.omit(dataLagHalfDay)
# 
# trainIndex = nrow(dataLagHalfDay) *.70
# testIndex = nrow(dataLagHalfDay)-trainIndex
# 
# train = dataLagHalfDay[1:trainIndex,]
# test = dataLagHalfDay[testIndex:nrow(dataLagHalfDay),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagHalfDayModelGBM <- train( as.factor(AveragedExchange) ~ .,
#                                  data = dataLagHalfDay,
#                                  method = "gbm",
#                                  preProc = c("center", "scale"),
#                                  # trControl = numFolds,
#                                  trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagHalfDayModelGBM.rds")
# 
# #-------------------------------End of half day Data Lag ----------------------------------------------
# 
# 

#---------------Lag data by a  day -------------------------------------------
dataLagDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=48))
colnames(dataLagDay)[ncol(dataLagDay)] = c("AveragedExchange")

dataLagDay = na.omit(dataLagDay)

trainIndex = nrow(dataLagDay) *.70
testIndex = nrow(dataLagDay)-trainIndex

train = dataLagDay[1:trainIndex,]
test = dataLagDay[testIndex:nrow(dataLagDay),]

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 960,  #  <-----------  20 days
                              horizon = 96,         #  <-----------  2 days
                              fixedWindow = TRUE)


#numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
dataLagDayModelGBM <- train( as.factor(AveragedExchange) ~ .,
                             data = dataLagDay,
                             method = "gbm",
                             preProc = c("center", "scale"),
                             # trControl = numFolds,
                             trControl = myTimeControl)


#pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95


saveRDS(dataLagDayModelGBM, "Modeling/trainedModels/dataLagDayModelGBM2.rds")

#-------------------------------End of day Data Lag ----------------------------------------------
dataLagDayModelGBM

# 
# #---------------Lag data by a day and a half -------------------------------------------
# dataLagDayHalf = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=72))
# colnames(dataLagDayHalf)[ncol(dataLagDayHalf)] = c("AveragedExchange")
# 
# dataLagDayHalf = na.omit(dataLagDayHalf)
# 
# trainIndex = nrow(dataLagDayHalf) *.70
# testIndex = nrow(dataLagDayHalf)-trainIndex
# 
# train = dataLagDayHalf[1:trainIndex,]
# test = dataLagDayHalf[testIndex:nrow(dataLagDayHalf),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagDayHalfModelGBM <- train( as.factor(AveragedExchange) ~ .,
#                                  data = dataLagDayHalf,
#                                  method = "gbm",
#                                  preProc = c("center", "scale"),
#                                  # trControl = numFolds,
#                                  trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# 
# saveRDS(trainedModel, "Modeling/trainedModels/dataLagDayHalfModelGBM.rds")
# #-------------------------------End of day and half Data Lag ----------------------------------------------
# 



# #---------------Lag data by a 2 dayand a half -------------------------------------------
# dataLagTwoDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=96))
# colnames(dataLagTwoDay)[ncol(dataLagTwoDay)] = c("AveragedExchange")
# 
# dataLagTwoDay = na.omit(dataLagTwoDay)
# 
# trainIndex = nrow(dataLagTwoDay) *.70
# testIndex = nrow(dataLagTwoDay)-trainIndex
# 
# train = dataLagTwoDay[1:trainIndex,]
# test = dataLagTwoDay[testIndex:nrow(dataLagTwoDay),]
# 
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 960,  #  <-----------  20 days
#                               horizon = 96,         #  <-----------  2 days
#                               fixedWindow = TRUE)
# 
# 
# #numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# dataLagTwoDayModelGBM <- train( as.factor(AveragedExchange) ~ .,
#                                 data = dataLagTwoDay,
#                                 method = "gbm",
#                                 preProc = c("center", "scale"),
#                                 # trControl = numFolds,
#                                 trControl = myTimeControl)
# 
# 
# #pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95
# 
# 
# #-------------------------------End of 2 day Data Lag ----------------------------------------------
# dataLagHourModelGBM
# dataLagHalfDayModelGBM
# dataLagDayModelGBM
# dataLagDayHalfModelGBM
# dataLagTwoDayModelGBM
# 
# 
# saveRDS(dataLagTwoDayModelGBM, "Modeling/trainedModels/dataLagTwoDayModelGBM.rds")







#                NNET

#---------------Lag data by a  day -------------------------------------------
dataLagDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=48))
colnames(dataLagDay)[ncol(dataLagDay)] = c("AveragedExchange")

dataLagDay = na.omit(dataLagDay)

trainIndex = nrow(dataLagDay) *.70
testIndex = nrow(dataLagDay)-trainIndex

train = dataLagDay[1:trainIndex,]
test = dataLagDay[testIndex:nrow(dataLagDay),]

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 960,  #  <-----------  20 days
                              horizon = 96,         #  <-----------  2 days
                              fixedWindow = TRUE)



dataLagDayModelNNET <- train( as.factor(AveragedExchange) ~ .,
                       data = data,
                       method = "nnet",
                       preProc = c("center", "scale"),
                       # trControl = numFolds,
                       trControl = myTimeControl,
                       tuneGrid=expand.grid(size=c(102,50,25,5), decay=c(0.1)))

#pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95


saveRDS(dataLagDayModelNNET, "Modeling/trainedModels/dataLagDayModelNNET2.rds")

#-------------------------------End of day Data Lag ----------------------------------------------
dataLagDayModelNNET



#---------------Lag data by a  day -------------------------------------------
dataLagDay = cbind(data[,-targetColumn],lag(data$AveragedExchange, n=48))
colnames(dataLagDay)[ncol(dataLagDay)] = c("AveragedExchange")

dataLagDay = na.omit(dataLagDay)
nzv <- nearZeroVar(dataLagDay)
dataLagDay <- dataLagDay[,-nzv]
trainIndex = nrow(dataLagDay) *.70
testIndex = nrow(dataLagDay)-trainIndex

train = dataLagDay[1:trainIndex,]
test = dataLagDay[testIndex:nrow(dataLagDay),]



myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 960,  #  <-----------  20 days
                              horizon = 96,         #  <-----------  2 days
                              fixedWindow = TRUE)


#numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
dataLagDayModelGBM <- train( as.factor(AveragedExchange) ~ .,
                             data = dataLagDay,
                             method = "gbm",
                             preProc = c("center", "scale"),
                             # trControl = numFolds,
                             trControl = myTimeControl)


dataLagDayModelSVM <- svm((as.factor(AveragedExchange))~.,
              data = dataLagDay,
              trControl = myTimeControl,
              preProc = c("center", "scale"))

#pred <- ((as.numeric(predict(trainedModel,data)))/5)-1.95


saveRDS(dataLagDayModelSVM, "Modeling/trainedModels/dataLagDayModelSVM2.rds")

#-------------------------------End of day Data Lag ----------------------------------------------
dataLagDayModelSVM


pred = as.data.frame(as.numeric(predict(trainedModel,data)))
true <- as.data.frame(as.numeric(test$AveragedExchange))

merged = cbind(pred,true)

conf1 <- confusionMatrix(pred, data$AveragedExchange)

conf1
probs <- predict(pred, newdata=data, type='prob')

(trainedModel)
trainedModel$results= true-pred
resid = as.data.frame(as.numeric(true)-as.numeric(as.character(pred)))




plot(as.numeric(true), col = "red", ylab = "true (red) , pred (black)", ylim = range(c(as.numeric(as.character(pred)),true)))
points(as.numeric(as.character(pred)), col = "black") 
lines(resid)




