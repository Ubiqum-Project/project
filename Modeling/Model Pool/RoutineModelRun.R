
library(plyr)
library(readr)
library(reshape2)
library(caret)
library(dplyr)

library(e1071)
library(C50)
library(nnet)
library(h2o)
library(ranger)
library(gbm)

pdf(NULL)

#jscode <- "shinyjs.refresh = function() { history.go(0); }"


print("library Loaded")

#----------------------------------------------------------
#   This is the Magical Part of the Modeling Process #####
#----------------------------------------------------------

#-------->  Let's Import In Our Data ---------------------------

#--------> Enable Below for Diagnostic Mode--------------------------------------
data = read.csv(gzfile("RoutineDataOutput.csv.gz")) #-----------> For Local Runs outside of APP

data$AveragedExchange=as.factor(data$AveragedExchange)

RangerDay <- readRDS("cointrader/trainedModels/dataLagDayModelGBM2.rds")#------------>Change
RangerTwoDay<- readRDS("cointrader/trainedModels/dataLagDayModelGBM2.rds")#------------>Change
GBMDay<- readRDS("cointrader/trainedModels/dataLagDayModelGBM2.rds")
GBMTwoDay<- readRDS("cointrader/trainedModels/dataLagDayModelGBM2.rds")
RFDay<- readRDS("cointrader/trainedModels/dataLagDayModelRF2.rds")
RFTwoDay<- readRDS("cointrader/trainedModels/dataLagDayModelRF2.rds")
SVMDay<- readRDS("cointrader/trainedModels/dataLagDayModelSVM2.rds")
SVMTwoDay<- readRDS("cointrader/trainedModels/dataLagDayModelSVM2.rds")


print("files Loaded")



#--------------------------------------------------------------------------


#########################################################################
##                RANGER                                              ###
#########################################################################
RangerDayALLPredict = predict(RangerDay,data)
#--------> Generate Predictions from Ranger One Day Out-------------------

RangerDayPredict = predict(RangerDay,data[(nrow(data)-48):nrow(data),])

RangerDayAccuracy = paste0(round(max(RangerDay$results[4])*100, digits = 0))
#--------------------------------------------------------------------------

#--------> Generate Predictions from Ranger Two Days Out-------------------

RangerTwoDayPredict = predict(RangerTwoDay,data[(nrow(data)-96):nrow(data),])

RangerTwoDayAccuracy = paste0(round(max(RangerTwoDay$results[4])*100, digits = 0))
#--------------------------------------------------------------------------

print("ranger loaded ")
#########################################################################
##                GBM                                                 ###
#########################################################################
GBMDayALLPredict = predict(GBMDay,data)
#--------> Generate Predictions from Ranger One Day Out-------------------

GBMDayPredict = predict(GBMDay,data[(nrow(data)-48):nrow(data),])
GBMDayAccuracy = paste0(round(max(GBMDay$results[5])*100, digits = 0))

#--------------------------------------------------------------------------

#--------> Generate Predictions from Ranger Two Days Out-------------------

GBMTwoDayPredict = predict(GBMTwoDay,data[(nrow(data)-96):nrow(data),])
GBMTwoDayAccuracy = paste0(round(max(GBMTwoDay$results[5])*100, digits = 0))
#--------------------------------------------------------------------------
print("gbm loaded ")
#########################################################################
##                RF                                                 ###
#########################################################################
RFDayALLPredict = predict(RFDay,data)
#--------> Generate Predictions from Ranger One Day Out-------------------

RFDayPredict = predict(RFDay,data[(nrow(data)-48):nrow(data),])
RFDayAccuracy = paste0(round(max(RFDay$results[2])*100, digits = 0))

#--------------------------------------------------------------------------

#--------> Generate Predictions from Ranger Two Days Out-------------------

RFTwoDayPredict = predict(RFTwoDay,data[(nrow(data)-96):nrow(data),])
RFTwoDayAccuracy = paste0(round(max(RFTwoDay$results[2])*100, digits = 0))
#--------------------------------------------------------------------------
print("rf loaded ")
#########################################################################
##                SVM                                               ###
#########################################################################
#nzv <- nearZeroVar(data)
dataSVM <- data#[,-nzv]

SVMDayALLPredict = predict(SVMDay,data)
#--------> Generate Predictions from Ranger One Day Out-------------------

SVMDayPredict = predict(SVMDay,dataSVM[(nrow(dataSVM)-48):nrow(dataSVM),])

svmAccuracy = confusionMatrix(dataSVM$AveragedExchange, predict(SVMDay,dataSVM))
svmOneAccuracy = round(svmAccuracy$overall[1]*100, digits = 0)

#--------------------------------------------------------------------------

#--------> Generate Predictions from Ranger Two Days Out-------------------


SVMTwoDayPredict = predict(SVMTwoDay,dataSVM[(nrow(dataSVM)-96):nrow(dataSVM),])

svmAccuracy = confusionMatrix(dataSVM$AveragedExchange, predict(SVMTwoDay,dataSVM))
svmTwoAccuracy = round(svmAccuracy$overall[1]*100, digits = 0)


svmTwoAccuracy = round(svmAccuracy$overall[1]*100, digits = 0)



#--------------------------------------------------------------------------
print("svm loaded ")
#########################################################################
##                H2O AML                                             ###
#########################################################################

#--------> Generate Predictions from AML One Day Out-------------------

h2o.init()
print("H2O Initialized ")
#H2oDay <- h2o.loadModel("cointrader/trainedModels/StackedEnsemble_AllModels_0_AutoML_20180305_184458")
print("H2o model loaded ")
H2oAMLDay <- h2o.loadModel(paste0("cointrader/trainedModels/h20AMLOneDay/",dir("cointrader/trainedModels/h20AMLOneDay")))
dataX <- as.h2o(data)
H2oAMLDayALLPredict = predict(H2oAMLDay,dataX)
h20AMLDayAllResults = as.data.frame(H2oAMLDayALLPredict)
H2oAMLDayAllPredict = as.data.frame(h20AMLDayAllResults$predict)

data1 <- as.h2o(data[(nrow(data)-48):nrow(data),])
H2oAMLDayPredictData = predict(H2oAMLDay,data1)
h20AMLDayResults = as.data.frame(H2oAMLDayPredictData)
H2oAMLDayPredict = as.data.frame(h20AMLDayResults$predict)
h2oAMLAccuracyDay =round((H2oAMLDay@model$validation_metrics@metrics$r2)*100,digits = 0)

#--------------------------------------------------------------------------

#--------> Generate Predictions from Ranger Two Days Out-------------------
#H2oTwoDay <- h2o.loadModel("cointrader/trainedModels/GBM_grid_0_AutoML_20180305_185644_model_3")
H2oAMLTwoDay <- h2o.loadModel(paste0("cointrader/trainedModels/h20AMLOneDay/",dir("cointrader/trainedModels/h20AMLOneDay")))

data2 <- as.h2o(data[(nrow(data)-96):nrow(data),])
H2oAMLTwoDayPredictData = predict(H2oAMLTwoDay,data2)
h20AMLTwoDayResults = as.data.frame(H2oAMLTwoDayPredictData)
H2oAMLTwoDayPredict = as.data.frame(h20AMLTwoDayResults$predict)
h2oAMLAccuracyTwoDay =round((H2oAMLTwoDay@model$validation_metrics@metrics$r2)*100,digits = 0)
print("h2o loaded ")


#########################################################################
##                H2O Deep                                             ###
#########################################################################

#--------> Generate Predictions from H2O Deep Learning One Day Out-------------------

h2o.init()
print("H2O Initialized ")
#H2oDay <- h2o.loadModel("cointrader/trainedModels/StackedEnsemble_AllModels_0_AutoML_20180305_184458")
print("H2o model loaded ")
H2oDeepDay <- h2o.loadModel(paste0("cointrader/trainedModels/h20DeepOneDay/",dir("cointrader/trainedModels/h20DeepOneDay")))
dataX <- as.h2o(data)
H2oDeepDayALLPredict = predict(H2oDeepDay,dataX)
h20DeepDayAllResults = as.data.frame(H2oDeepDayALLPredict)
H2oDeepDayAllPredict = as.data.frame(h20DeepDayAllResults$predict)

data1 <- as.h2o(data[(nrow(data)-48):nrow(data),])
H2oDeepDayPredictData = predict(H2oDeepDay,data1)
h20DeepDayResults = as.data.frame(H2oDeepDayPredictData)
H2oDeepDayPredict = as.data.frame(h20DeepDayResults$predict)
h2oDeepAccuracyDay =round((H2oDeepDay@model$validation_metrics@metrics$r2)*100,digits = 0)

#--------------------------------------------------------------------------

#--------> Generate Predictions from Ranger Two Days Out-------------------
#H2oTwoDay <- h2o.loadModel("cointrader/trainedModels/GBM_grid_0_AutoML_20180305_185644_model_3")
H2oDeepTwoDay <- h2o.loadModel(paste0("cointrader/trainedModels/h20DeepOneDay/",dir("cointrader/trainedModels/h20DeepOneDay")))

data2 <- as.h2o(data[(nrow(data)-96):nrow(data),])
H2oDeepTwoDayPredictData = predict(H2oDeepTwoDay,data2)
h20DeepTwoDayResults = as.data.frame(H2oDeepTwoDayPredictData)
H2oDeepTwoDayPredict = as.data.frame(h20DeepTwoDayResults$predict)
h2oDeepAccuracyTwoDay =round((H2oDeepTwoDay@model$validation_metrics@metrics$r2)*100,digits = 0)
print("h2o loaded ")


#########################################################################
##                LSTM                                               ###
#########################################################################


#########################################################################
##                Build the Predictions Frame                         ###
#########################################################################


oneDayAllCombined = cbind(as.data.frame(data$AveragedExchange), as.data.frame(RangerDayALLPredict),as.data.frame(RFDayALLPredict),as.data.frame(GBMDayALLPredict),as.data.frame(SVMDayALLPredict),as.data.frame(H2oDeepDayAllPredict),as.data.frame(H2oAMLDayAllPredict),as.data.frame(RangerDayALLPredict))
oneDayAllCombined = as.data.frame(oneDayAllCombined)
colnames(oneDayAllCombined) = c("Actual", "Ranger","RandomForest", "GBM", "SVM", "H2O Deep", "H2O AML","LSTM")

#-------> Building Answer Key ------------------------------------------

AveragedExchange = data$AveragedExchange

historicalFrame = data.frame(AveragedExchange,AveragedExchange,AveragedExchange,AveragedExchange,AveragedExchange,AveragedExchange,AveragedExchange)

colnames(historicalFrame) = c("Ranger", "RandomForest", "GBM", "SVM", "H2O Deep", "H2O AML","LSTM")
print("ghistorical frame built ")
#---------DataFrame of Day Predictions ---------------------------------

predictOneDayFrame = data.frame(as.data.frame(RangerDayPredict),as.data.frame(RFDayPredict),as.data.frame(GBMDayPredict),as.data.frame(SVMDayPredict),as.data.frame(H2oDeepDayPredict), as.data.frame(H2oAMLDayPredict),as.data.frame(RangerDayPredict))
colnames(predictOneDayFrame) = c("Ranger", "RandomForest", "GBM", "SVM", "H2O Deep", "H2O AML","LSTM")
oneDayPredictions= rbind(historicalFrame, predictOneDayFrame)
print("one day frame built ")

#---------DataFrame of Day Accuracy ---------------------------------

oneDayModelAccuracy = data.frame(RangerDayAccuracy,RFDayAccuracy,GBMDayAccuracy,svmOneAccuracy,round(h2oDeepAccuracyDay[3]*100,digits=0), round(h2oAMLAccuracyDay*100,digits=0),RangerDayAccuracy)
colnames(oneDayModelAccuracy) = c("Ranger", "RandomForest", "GBM", "SVM", "H2O Deep", "H2O AML","LSTM")

#---------DataFrame of 2 Day Predictions -------------------------------

predictTwoDayFrame = data.frame(RangerTwoDayPredict,RFTwoDayPredict,GBMTwoDayPredict,SVMTwoDayPredict,H2oDeepTwoDayPredict, H2oAMLTwoDayPredict,RangerTwoDayPredict)
colnames(predictTwoDayFrame) = c("Ranger", "RandomForest", "GBM", "SVM", "H2O Deep", "H2O AML","LSTM")
twoDayPredictions = rbind(historicalFrame, predictTwoDayFrame)
print("two day frame built ")

#---------DataFrame of Two Day Accuracy ---------------------------------

twoDayModelAccuracy = data.frame(RangerTwoDayAccuracy,RFTwoDayAccuracy,GBMTwoDayAccuracy,svmTwoAccuracy,round(h2oDeepAccuracyDay[3]*100,digits=0), round(h2oAMLAccuracyDay*100,digits=0),RangerTwoDayAccuracy)
colnames(twoDayModelAccuracy) = c("Ranger", "RandomForest", "GBM", "SVM", "H2O Deep", "H2O AML","LSTM")

z <- gzfile("cointrader/trainedModels/RoutineOneDay.csv.gz")
write.csv(oneDayPredictions, z)

z <- gzfile("cointrader/trainedModels/RoutineOneDayAccuracy.csv.gz")
write.csv(oneDayModelAccuracy, z)

z <- gzfile("cointrader/trainedModels/RoutineTwoDay.csv.gz")
write.csv(twoDayPredictions, z)

z <- gzfile("cointrader/trainedModels/RoutineTwoDayAccuracy.csv.gz")
write.csv(twoDayModelAccuracy, z)
