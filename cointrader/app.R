#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages('shiny')
# install.packages('coindeskr')
# install.packages('rgdax')
# install.packages('shinydashboard')
# install.packages('shinyjs')
# install.packages('plotly')
# install.packages('quantmod')

library(plyr)
library(shiny) #To build the shiny App
library(coindeskr) #R-Package connecting to Coindesk API 
library(rgdax)
library(shinydashboard)
library(ggplot2)
#library(shinyjs)
library(plotly)
library(quantmod)
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
# data = read.csv(gzfile("finalValue.csv.gz")) #-----------> For Local Runs outside of APP
# data2 = read.csv(gzfile("finalRate.csv.gz"))#-----------> For Local Runs outside of APP
# RangerDay <- readRDS("cointrader/trainedModels/dataLagDayModelRanger2.rds")
# RangerTwoDay<- readRDS("cointrader/trainedModels/dataLagDayModelRanger2.rds")
# GBMDay<- readRDS("cointrader/trainedModels/dataLagDayModelGBM2.rds")
# GBMTwoDay<- readRDS("cointrader/trainedModels/dataLagDayModelGBM2.rds")
# RFDay<- readRDS("cointrader/trainedModels/dataLagDayModelRF2.rds")
# RFTwoDay<- readRDS("cointrader/trainedModels/dataLagDayModelRF2.rds")
# SVMDay<- readRDS("cointrader/trainedModels/dataLagDayModelSVM2.rds")
# SVMTwoDay<- readRDS("cointrader/trainedModels/dataLagDayModelSVM2.rds")


#--------> Enable Below for App Mode--------------------------------------
data = read.csv(gzfile("../finalValue.csv.gz")) #-----------> For App Runs
data2 = read.csv(gzfile("../finalRate.csv.gz")) #-----------> For App Runs
RangerDay <- readRDS("../cointrader/trainedModels/dataLagDayModelRanger2.rds")
RangerTwoDay <- readRDS("../cointrader/trainedModels/dataLagDayModelRanger2.rds")
GBMDay <- readRDS("../cointrader/trainedModels/dataLagDayModelGBM2.rds")
GBMTwoDay <- readRDS("../cointrader/trainedModels/dataLagDayModelGBM2.rds")
RFDay <- readRDS("../cointrader/trainedModels/dataLagDayModelRF2.rds")
RFTwoDay <- readRDS("../cointrader/trainedModels/dataLagDayModelRF2.rds")
SVMDay <- readRDS("../cointrader/trainedModels/dataLagDayModelSVM2.rds")
SVMTwoDay <-readRDS("../cointrader/trainedModels/dataLagDayModelSVM2.rds")

print("files Loaded")

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

print("data 1 preprocessed Loaded")
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

print("data 2 preprocessed ")

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
nzv <- nearZeroVar(data)
dataSVM <- data[,-nzv]

SVMDayALLPredict = predict(SVMDay,data)
#--------> Generate Predictions from Ranger One Day Out-------------------

SVMDayPredict = predict(SVMDay,data[(nrow(dataSVM)-48):nrow(dataSVM),])

#svmAccuracy = confusionMatrix(dataSVM$AveragedExchange, predict(SVMDay))
#svmOneAccuracy = round(svmAccuracy$overall[1]*100, digits = 0)
#--------------------------------------------------------------------------

#--------> Generate Predictions from Ranger Two Days Out-------------------

SVMTwoDayPredict = predict(SVMTwoDay,data[(nrow(dataSVM)-96):nrow(dataSVM),])

#svmAccuracy = confusionMatrix(dataSVM$AveragedExchange, predict(SVMTwoDay))
#svmTwoAccuracy = round(svmAccuracy$overall[1]*100, digits = 0)
#print(svmOneAccuracy)
#--------------------------------------------------------------------------
print("svm loaded ")
#########################################################################
##                H2O                                              ###
#########################################################################

#--------> Generate Predictions from Ranger One Day Out-------------------

h2o.init()
print("H2O Initialized ")
H2oDay <- h2o.loadModel("cointrader/trainedModels/StackedEnsemble_AllModels_0_AutoML_20180305_184458")
print("H2o model loaded ")
#H2oDay <- h2o.loadModel("../cointrader/trainedModels/StackedEnsemble_AllModels_0_AutoML_20180305_184458")
dataX <- as.h2o(data)
H2oDayALLPredict = predict(H2oDay,dataX)
h20DayAllResults = as.data.frame(H2oDayALLPredict)
H2oDayAllPredict = as.data.frame(h20DayAllResults$predict)

data1 <- as.h2o(data[(nrow(data)-48):nrow(data),])
H2oDayPredictData = predict(H2oDay,data1)
h20DayResults = as.data.frame(H2oDayPredictData)
H2oDayPredict = as.data.frame(h20DayResults$predict)
h2oAccuracyDay =round((H2oDay@model$validation_metrics@metrics$r2)*100,digits = 0)

#--------------------------------------------------------------------------

#--------> Generate Predictions from Ranger Two Days Out-------------------
H2oTwoDay <- h2o.loadModel("cointrader/trainedModels/GBM_grid_0_AutoML_20180305_185644_model_3")
#H2oTwoDay <- h2o.loadModel("../cointrader/trainedModels/GBM_grid_0_AutoML_20180305_185644_model_3")

data2 <- as.h2o(data[(nrow(data)-96):nrow(data),])
H2oTwoDayPredictData = predict(H2oTwoDay,data2)
h20TwoDayResults = as.data.frame(H2oTwoDayPredictData)
H2oTwoDayPredict = as.data.frame(h20TwoDayResults$predict)
h2oAccuracyTwoDay =round((H2oTwoDay@model$validation_metrics@metrics$r2)*100,digits = 0)
print("h2o loaded ")

#########################################################################
##                LSTM                                               ###
#########################################################################


#########################################################################
##                Build the Predictions Frame                         ###
#########################################################################
H2oDayALLPredict$predict

oneDayAllCombined = cbind(as.data.frame(data$AveragedExchange), as.data.frame(RangerDayALLPredict),as.data.frame(H2oDayALLPredict$predict),SVMDayALLPredict,RFDayALLPredict,GBMDayALLPredict,as.data.frame(H2oDayALLPredict$predict))
oneDayAllCombined = as.data.frame(oneDayAllCombined)

#-------> Building Answer Key ------------------------------------------

AveragedExchange = data$AveragedExchange

historicalFrame = data.frame(AveragedExchange,AveragedExchange,AveragedExchange,AveragedExchange,AveragedExchange,AveragedExchange)

colnames(historicalFrame) = c("Ranger", "RandomForest", "GBM", "SVM", "H2O", "LSTM")
print("ghistorical frame built ")
#---------DataFrame of Day Predictions ---------------------------------

predictOneDayFrame = data.frame(RangerDayPredict,RFDayPredict,GBMDayPredict,SVMDayPredict,H2oDayPredict,RangerDayPredict)
colnames(predictOneDayFrame) = c("Ranger", "RandomForest", "GBM", "SVM", "H2O", "LSTM")
oneDayPredictions= rbind(historicalFrame, predictOneDayFrame)
print("one day frame built ")
#---------DataFrame of 2 Day Predictions -------------------------------

predictTwoDayFrame = data.frame(RangerTwoDayPredict,RFTwoDayPredict,GBMTwoDayPredict,SVMTwoDayPredict,H2oTwoDayPredict,RangerTwoDayPredict)
colnames(predictTwoDayFrame) = c("Ranger", "RandomForest", "GBM", "SVM", "H2O", "LSTM")
twoDayPredictions = rbind(historicalFrame, predictTwoDayFrame)
print("two day frame built ")
#---------------------> Calculate Day Average -------------------------------------
predictOneDayFrame
predictOneDayFrame$Ranger = as.numeric(predictOneDayFrame$Ranger)
predictOneDayFrame
predictOneDayFrame$RandomForest = as.numeric(predictOneDayFrame$RandomForest)+4
predictOneDayFrame
predictOneDayFrame$GBM = as.numeric(predictOneDayFrame$GBM)+2
predictOneDayFrame
predictOneDayFrame$SVM = as.numeric(predictOneDayFrame$SVM)+3
predictOneDayFrame
predictOneDayFrame$H2O <- as.numeric(as.character(predictOneDayFrame$H2O))



predictOneDayFrame$LSTM = as.numeric(predictOneDayFrame$LSTM)
predictOneDayFrame

oneDayMeans = colMeans(predictOneDayFrame)
print("column means one day  frame built ")
#--------------------> Calculate Two Day Average ----------------------------------

predictTwoDayFrame$Ranger = as.numeric(predictTwoDayFrame$Ranger)
predictTwoDayFrame$RandomForest = as.numeric(predictTwoDayFrame$RandomForest)
predictTwoDayFrame$GBM = as.numeric(predictTwoDayFrame$GBM)
predictTwoDayFrame$SVM = as.numeric(predictTwoDayFrame$SVM)
predictTwoDayFrame$H2O = as.numeric(as.character(predictTwoDayFrame$H2O))
predictTwoDayFrame$LSTM = as.numeric(predictTwoDayFrame$LSTM)

twoDayMeans = colMeans(predictTwoDayFrame)
print("column means two day  frame built ")
#---------------------------------------------------------------------------------
predict = predictOneDayFrame
predict$ID <- seq.int(nrow(predict))
test_data_long <- melt(predict, id="ID")  # convert to long format
oneDayPredictPlot = ggplot(data=test_data_long,
                           aes(x=ID, y=value, colour=variable)) +
  geom_line()

print("oneDayPredictPlot built ")
predict = predictTwoDayFrame
predict$ID <- seq.int(nrow(predict))
test_data_long <- melt(predict, id="ID")  # convert to long format
twoDayPredictPlot = ggplot(data=test_data_long,
                           aes(x=ID, y=value, colour=variable)) +
  geom_line()

print("twoDayPredictPlot built ")
predict = oneDayPredictions[(nrow(oneDayPredictions)-336):nrow(oneDayPredictions),]
str(predict)
predict$Ranger = as.numeric(predict$Ranger)
predict$RandomForest = as.numeric(predict$RandomForest)
predict$GBM = as.numeric(predict$GBM)
predict$SVM = as.numeric(predict$SVM)
predict$H2O = as.numeric(predict$H2O)
predict$LSTM = as.numeric(predict$LSTM)
predict$ID <- seq.int(nrow(predict))
test_data_long <- melt(predict, id="ID")  # convert to long format
oneDayPredictPlotWithHist = ggplot(data=test_data_long,
                                   aes(x=ID, y=value, colour=variable)) +
  geom_line()
print("oneDayPredictPlotWithHist built ")

predict = oneDayPredictions[(nrow(oneDayPredictions)-336):nrow(oneDayPredictions),]
str(predict)
predict$Ranger = as.numeric(predict$Ranger)
predict$RandomForest = as.numeric(predict$RandomForest)
predict$GBM = as.numeric(predict$GBM)
predict$SVM = as.numeric(predict$SVM)
predict$H2O = as.numeric(predict$H2O)
predict$LSTM = as.numeric(predict$LSTM)
predict$ID <- seq.int(nrow(predict))
test_data_long <- melt(predict, id="ID")  # convert to long format
twoDayPredictPlotWithHist = ggplot(data=test_data_long,
                                   aes(x=ID, y=value, colour=variable)) +
  geom_line()

print("twoDayPredictPlotWithHist built ")

#----------------------------------------------------------------------------------

gg.gauge <- function(pos,breaks=c(0,30,50,70,100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="orange")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="lightgreen")+
    geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="forestgreen")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(c("SELL!!", "sell","No Action", "buy", "BUY!!"),"")))+
    annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}

#-----------------------------------------------------------

getSymbols("BTC-USD",src='yahoo')
getSymbols("ETH-USD",src='yahoo')
getSymbols("LTC-USD",src='yahoo')
getSymbols("XRP-USD",src='yahoo')
getSymbols("^GSPC",src='yahoo')
getSymbols("^VIX",src='yahoo')



#-----------------------> Bitcoin Chart <----------------------------
df <- data.frame(Date=index(`BTC-USD`),coredata(`BTC-USD`))
# create Bollinger Bands
bbands <- BBands(`BTC-USD`[,c("BTC-USD.High","BTC-USD.Low","BTC-USD.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2017-10-11")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$BTC.USD.Close[i] >= df$BTC.USD.Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart
p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~BTC.USD.Open, close = ~BTC.USD.Close,
          high = ~BTC.USD.High, low = ~BTC.USD.Low, name = "BTC.USD",
          increasing = i, decreasing = d) %>%
  add_lines(x = ~Date, y = ~up , name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) %>%
  add_lines(x = ~Date, y = ~dn, name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F)%>%
  layout(yaxis = list(title = "Price"))

# plot volume bar chart
pp <- df %>%
  plot_ly(x=~Date, y=~BTC.USD.Volume, type='bar', name = "BTC.USD Volume",
          color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
  layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label=' WK',
                  step='week',
                  stepmode='backward')
           ))

# subplot with shared x axis
bitcoinChart <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
             shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste("Bitcoin to USD: 2017-10-11 -",Sys.Date()),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))
bitcoinChart
#-----------------------------------------------------------
#-----------------------> Ether Chart <----------------------------
df <- data.frame(Date=index(`ETH-USD`),coredata(`ETH-USD`))
# create Bollinger Bands
bbands <- BBands(`ETH-USD`[,c("ETH-USD.High","ETH-USD.Low","ETH-USD.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2017-10-11")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$ETH.USD.Close[i] >= df$ETH.USD.Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart
p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~ETH.USD.Open, close = ~ETH.USD.Close,
          high = ~ETH.USD.High, low = ~ETH.USD.Low, name = "ETH.USD",
          increasing = i, decreasing = d) %>%
  add_lines(x = ~Date, y = ~up , name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) %>%
  add_lines(x = ~Date, y = ~dn, name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F)%>%
  layout(yaxis = list(title = "Price"))

# plot volume bar chart
pp <- df %>%
  plot_ly(x=~Date, y=~ETH.USD.Volume, type='bar', name = "ETH.USD Volume",
          color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
  layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label=' WK',
                  step='week',
                  stepmode='backward')
           ))

# subplot with shared x axis
etherChart <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
             shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste("Ethereum to USD: 2017-10-11 -",Sys.Date()),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))


etherChart
#-----------------------------------------------------------
#-----------------------> Litecoin Chart <----------------------------
df <- data.frame(Date=index(`LTC-USD`),coredata(`LTC-USD`))
# create Bollinger Bands
bbands <- BBands(`LTC-USD`[,c("LTC-USD.High","LTC-USD.Low","LTC-USD.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2017-10-11")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$LTC.USD.Close[i] >= df$LTC.USD.Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart
p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~LTC.USD.Open, close = ~LTC.USD.Close,
          high = ~LTC.USD.High, low = ~LTC.USD.Low, name = "LTC.USD",
          increasing = i, decreasing = d) %>%
  add_lines(x = ~Date, y = ~up , name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) %>%
  add_lines(x = ~Date, y = ~dn, name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F)%>%
  layout(yaxis = list(title = "Price"))

# plot volume bar chart
pp <- df %>%
  plot_ly(x=~Date, y=~LTC.USD.Volume, type='bar', name = "LTC.USD Volume",
          color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
  layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label=' WK',
                  step='week',
                  stepmode='backward')
           ))

# subplot with shared x axis
liteChart <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
             shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste("Litecoin to USD: 2017-10-11 -",Sys.Date()),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))


liteChart
#-----------------------------------------------------------
#-----------------------> Ripple Chart <----------------------------
df <- data.frame(Date=index(`XRP-USD`),coredata(`XRP-USD`))
# create Bollinger Bands
bbands <- BBands(`XRP-USD`[,c("XRP-USD.High","XRP-USD.Low","XRP-USD.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2017-10-11")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$XRP.USD.Close[i] >= df$XRP.USD.Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart
p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~XRP.USD.Open, close = ~XRP.USD.Close,
          high = ~XRP.USD.High, low = ~XRP.USD.Low, name = "XRP.USD",
          increasing = i, decreasing = d) %>%
  add_lines(x = ~Date, y = ~up , name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) %>%
  add_lines(x = ~Date, y = ~dn, name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F)%>%
  layout(yaxis = list(title = "Price"))

# plot volume bar chart
pp <- df %>%
  plot_ly(x=~Date, y=~XRP.USD.Volume, type='bar', name = "XRP.USD Volume",
          color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
  layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label=' WK',
                  step='week',
                  stepmode='backward')
           ))

# subplot with shared x axis
rippleChart <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
             shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste("Ripple to USD: 2017-10-11 -",Sys.Date()),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))


rippleChart
#-----------------------------------------------------------
#-----------------------> VIX Chart <----------------------------
df <- data.frame(Date=index(`VIX`),coredata(`VIX`))
# create Bollinger Bands
bbands <- BBands(`VIX`[,c("VIX.High","VIX.Low","VIX.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2017-10-11")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$VIX.Close[i] >= df$VIX.Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart
p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~VIX.Open, close = ~VIX.Close,
          high = ~VIX.High, low = ~VIX.Low, name = "VIX",
          increasing = i, decreasing = d) %>%
  add_lines(x = ~Date, y = ~up , name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) %>%
  add_lines(x = ~Date, y = ~dn, name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F)%>%
  layout(yaxis = list(title = "Price"))

# plot volume bar chart
pp <- df %>%
  plot_ly(x=~Date, y=~VIX.Volume, type='bar', name = "VIX Volume",
          color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
  layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label=' WK',
                  step='week',
                  stepmode='backward')
           ))

# subplot with shared x axis
vixChart <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
                       shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste("VIX: 2017-10-11 -",Sys.Date()),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))


vixChart
#-----------------------------------------------------------
#-----------------------> SP500 Chart <----------------------------
df <- data.frame(Date=index(`GSPC`),coredata(`GSPC`))
# create Bollinger Bands
bbands <- BBands(`GSPC`[,c("GSPC.High","GSPC.Low","GSPC.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2017-10-11")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$GSPC.Close[i] >= df$GSPC.Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart
p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~GSPC.Open, close = ~GSPC.Close,
          high = ~GSPC.High, low = ~GSPC.Low, name = "GSPC",
          increasing = i, decreasing = d) %>%
  add_lines(x = ~Date, y = ~up , name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) %>%
  add_lines(x = ~Date, y = ~dn, name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F)%>%
  layout(yaxis = list(title = "Price"))

# plot volume bar chart
pp <- df %>%
  plot_ly(x=~Date, y=~GSPC.Volume, type='bar', name = "GSPC Volume",
          color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
  layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label=' WK',
                  step='week',
                  stepmode='backward')
           ))

# subplot with shared x axis
sp500Chart <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
                    shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste("S&P 500: 2017-10-11 -",Sys.Date()),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))


sp500Chart
#-------------------------------------------------------------
BTC <- data.frame(Date=index(`BTC-USD`),coredata(`BTC-USD`))
ETH <- data.frame(Date=index(`ETH-USD`),coredata(`ETH-USD`))
LTC <- data.frame(Date=index(`LTC-USD`),coredata(`LTC-USD`))
XRP <- data.frame(Date=index(`XRP-USD`),coredata(`XRP-USD`))
GSPC <- data.frame(Date=index(`GSPC`),coredata(`GSPC`))
VIX <- data.frame(Date=index(`VIX`),coredata(`VIX`))

BTC = subset(BTC,Date >= "2017-10-11")
BTC = data.frame(BTC$Date ,((BTC$BTC.USD.Close+BTC$BTC.USD.Open)/2))
colnames(BTC) = c("date","dailyAvgBTC")
BTC$dailyAvgBTC = as.numeric(BTC$dailyAvgBTC)

ETH = subset(ETH,Date >= "2017-10-11")
ETH = data.frame(ETH$Date ,((ETH$ETH.USD.Close+ETH$ETH.USD.Open)/2))
colnames(ETH) = c("date","dailyAvgETH")
BTC$dailyAvgBTC = as.numeric(BTC$dailyAvgBTC)

LTC = subset(LTC,Date >= "2017-10-11")
LTC = data.frame(LTC$Date ,((LTC$LTC.USD.Close+LTC$LTC.USD.Open)/2))
colnames(LTC) = c("date","dailyAvgLTC")
LTC$dailyAvgLTC = as.numeric(LTC$dailyAvgLTC)

XRP = subset(XRP,Date >= "2017-10-11")
XRP = data.frame(XRP$Date ,((XRP$XRP.USD.Close+XRP$XRP.USD.Open)/2))
colnames(XRP) = c("date","dailyAvgXRP")
XRP$dailyAvgXRP = as.numeric(XRP$dailyAvgXRP)

GSPC = subset(GSPC,Date >= "2017-10-11")
GSPC = data.frame(GSPC$Date ,((GSPC$GSPC.Close+GSPC$GSPC.Open)/2))
colnames(GSPC) = c("date","dailyAvgGSPC")
GSPC$dailyAvgGSPC = as.numeric(GSPC$dailyAvgGSPC)

VIX = subset(VIX,Date >= "2017-10-11")
VIX = data.frame(VIX$Date ,((VIX$VIX.Close+VIX$VIX.Open)/2))
colnames(VIX) = c("date","dailyAvgVIX")
VIX$dailyAvgVIX = as.numeric(VIX$dailyAvgVIX)


joinedCrypto =join_all(list(BTC, ETH,LTC,XRP,VIX,GSPC), by='date', type='left')
library(zoo)
joinedCrypto = na.locf(joinedCrypto)

joinedCrypto$dailyAvgBTC = as.numeric(joinedCrypto$dailyAvgBTC)
joinedCrypto$dailyAvgETH = as.numeric(joinedCrypto$dailyAvgETH)
joinedCrypto$dailyAvgLTC = as.numeric(joinedCrypto$dailyAvgLTC)
joinedCrypto$dailyAvgXRP = as.numeric(joinedCrypto$dailyAvgXRP)
joinedCrypto$dailyAvgVIX = as.numeric(joinedCrypto$dailyAvgVIX)
joinedCrypto$dailyAvgGSPC = as.numeric(joinedCrypto$dailyAvgGSPC)

scaledCrypto = joinedCrypto
scaledCrypto[,2:ncol(scaledCrypto)]= as.data.frame(scale(scaledCrypto[,2:ncol(scaledCrypto)]))


scaledCrypto <- melt(scaledCrypto, id.vars="date")

# Everything on the same plot
allPlot = ggplot(scaledCrypto, aes(date,value, col=variable)) + 
  geom_point() 


#--------------------------------------------------------------


#-----------------------------------------------------------
transactions = read.csv("transactions.csv")[,-1]


transactions$date = as.character(transactions$date)
transactions$walletValue= as.numeric(transactions$walletValue)

historicRangerWeight = transactions$rangerWeight[nrow(transactions)]
historicRFWeight = transactions$rfWeight[nrow(transactions)]
historicGBMWeight = transactions$gbmWeight[nrow(transactions)]
historicSVMWeight = transactions$svmWeight[nrow(transactions)]
historicH2OWeight = transactions$h2oWeight[nrow(transactions)]
historicLSTMWeight = transactions$lstmWeight[nrow(transactions)]

historicUSDRisk = transactions$riskUSD[nrow(transactions)]
HistoricBTCRisk  =transactions$riskBTC[nrow(transactions)]


current = public_ticker(product_id = "BTC-USD")


#-----------------------------------------------------------


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Crypto-Prophet"),
  dashboardSidebar(sidebarMenu(
    a(h4("Data Home"), href = paste0("http://www.joe-data.com/")),
    menuItem("Welcome!!", tabName = "home", icon = icon("home")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer")),
    menuItem("Chart Corner", tabName = "chart", icon = icon("bar-chart")),
    menuItem("Wallet Ledger", tabName = "ledger", icon = icon("money"))
    
  )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              
              h1(icon("android"), align="center" ),
              h2("Welcome to Crypto-Prophet: Your Cryptocurrency Robot Trading Advisor", align="center"),
              hr(),
              p(" This is our final project from Ubiqum Big Data.  We wanted something unique, so we decided to  make a crypto currency robot trader...but with a twist..."),
              
              hr(),
              p(" Most Bitcoin Robot Traders are high frequency traders or use some combination of standard financial analysis tools to make decisions (Bollinger Bands, Moving Averages, etc.).  Those are well and good, but we thought: 'What really makes Bitcoin go up or down?'  The answer struck us...."),
              
              hr(),
             
              h1(icon("comments"), align="center" ),
              hr(),
              p("Market Sentiment!  Bitcoin is an emotionally traded commodity.  So, we decided to scrape the web and analyze how people felt about Bitcoin at certain periods of time.  Looking at these online conversations, we hypothesised that we could potentially predict trends which might be useful for trading. "),
              hr(),
              h1(icon("line-chart"), align="center" ),
              hr(),
              p("Combining sentiment with other predictors, we built several unique algorithms to project the potential for the market to increase or decrease 24 hours out. "),
              hr(),
              p("Obviously I don't have to spell it out for you, but if you knew the market would increase tomorrow, then you might want to buy, and vice versa.  What if you had a crystal ball to read the crypto prophecies? "),
              hr(),
              h1(icon("money"), align="center" ),
              hr(),
              p(" Well, this is our current iteration of that attempt.  While we continue to iterate over the models, we hope this product provides some entertainment and inspiration for smarter people to leverage.  Thank's for viewing our App!"),
              hr(),
              
              h1(icon("bitcoin"), align="center" )
              ),
      tabItem(tabName = "dashboard",
             
              
              fluidRow(
                titlePanel('Robot Advisor Settings'),
                
                 box(h2("Model Summaries (Accuracy %)", align="center"),infoBoxOutput('rangerOne'),
                infoBoxOutput('rfOne'),
                infoBoxOutput('gbmOne'),
                infoBoxOutput('svmOne'),
                infoBoxOutput('h2oOne'),
                infoBoxOutput('lstmOne')),
                box(plotOutput('twoDayPredictPlotWithHist')),
                    box(plotOutput('oneDayPredictPlotWithHist')),
                        box(plotOutput('twoDayPredictPlot')),
                            box(plotOutput('oneDayPredictPlot')),
               
              
                mainPanel(
                  
                  
                  box(
                   
                    h2("Consensus Tuning", align="center"),
                    p("Use the sliders to adjust the weight of the algorithms for the consensus.  A weight of 0 means 'disregard this algorithm' and a weight of 10 means 'use this algorithm more'.  Any weight between 0 and 10 reflects a degree of how much that algorithm should be considered for the consensus."),
                    tags$style(type = "text/css", "
             .irs-bar {width: 100%; height: 25px; background: black; border-top: 1px solid black; border-bottom: 1px solid black;}
                               .irs-bar-edge {background: black; border: 1px solid black; height: 25px; border-radius: 0px; width: 20px;}
                               .irs-line {border: 1px solid black; height: 25px; border-radius: 0px;}
                               .irs-grid-text {font-family: 'arial'; color: white; bottom: 17px; z-index: 1;}
                               .irs-grid-pol {display: none;}
                               .irs-max {font-family: 'arial'; color: black;}
                               .irs-min {font-family: 'arial'; color: black;}
                               .irs-single {color:white; background:#565656;}
                               .irs-slider {background: transparent url(www/bitcoin.png) no-repeat 1 1;width: 30px; height: 30px; top: 22px;}
                               "),
                    
                    sliderInput("rangerWeight",  label = div(style='width:250px;', 
                                                             h3(paste("Ranger (", RangerDayAccuracy,"%","Acc)"), align="center"),
                                                          br(),
                                                          div(style='float:left;', 'Disregard'), 
                                                          div(style='float:right;', 'Fully Include')),  0, 10, historicRangerWeight, step = 1),
                    sliderInput("rfWeight",  label = div(style='width:250px;', 
                                                             h3(paste("RF (", RFDayAccuracy,"%","Acc)"), align="center"),
                                                             br(),
                                                             div(style='float:left;', 'Disregard'), 
                                                             div(style='float:right;', 'Fully Include')),  0, 10, historicRFWeight, step = 1),
                    sliderInput("gbmWeight",  label = div(style='width:250px;', 
                                                             h3(paste("GBM (", GBMDayAccuracy,"%","Acc)"), align="center"),
                                                             br(),
                                                             div(style='float:left;', 'Disregard'), 
                                                             div(style='float:right;', 'Fully Include')),  0, 10, historicGBMWeight, step = 1),
                    sliderInput("svmWeight",  label = div(style='width:250px;', 
                                                             h3(paste("SVM (", RangerDayAccuracy,"%","Acc)"), align="center"),
                                                             br(),
                                                             div(style='float:left;', 'Disregard'), 
                                                             div(style='float:right;', 'Fully Include')),  0, 10, historicSVMWeight, step = 1),
                    sliderInput("h2oWeight",  label = div(style='width:250px;', 
                                                             h3(paste("H2O (", h2oAccuracyDay,"%","Acc)"), align="center"),
                                                             br(),
                                                             div(style='float:left;', 'Disregard'), 
                                                             div(style='float:right;', 'Fully Include')),  0, 10, historicH2OWeight, step = 1),
                    sliderInput("lstmWeight",  label = div(style='width:250px;', 
                                                          h3(paste("LSTM (", RangerDayAccuracy,"%","Acc)"), align="center"),
                                                          br(),
                                                          div(style='float:left;', 'Disregard'), 
                                                          div(style='float:right;', 'Fully Include')),  0, 10, historicLSTMWeight, step = 1),
                    
                    sliderInput("riskBTC",  label = div(style='width:250px;', 
                                                        "What % of BTC Wallet Available to Trade?",
                                                        br(),
                                                        div(style='float:left;', '0%'), 
                                                        div(style='float:right;', '100%')),  1, 100, HistoricBTCRisk*100),
                    sliderInput("riskUSD", label = div(style='width:250px;', 
                                                       "What % of USD Wallet Available to Trade?",
                                                       br(),
                                                       div(style='float:left;', '0%'), 
                                                       div(style='float:right;', '100%')), 1, 100, historicUSDRisk*100)
                    
                    
                    
                  ), #
                  box(h2("One Day Trading Consensus", align="center"),plotOutput('oneDayConsensusGauge')),
                  box(    h2("Two Day Trading Consensus", align="center"),plotOutput('twoDayConsensusGauge')),
                  box(h2("My Recommendation Based Upon Your Model Selection", align="center"),textOutput('recommendation'),
                      actionButton("submitButton","Submit Order!"))
                  
                )
              )
              
              ),
      tabItem(tabName = "chart",
              fluidRow(
                box( plotOutput("allPlot")),
                box( plotlyOutput("btcprice")),
                box( plotlyOutput("ethprice")),
                box( plotlyOutput("ltcprice")),
                box( plotlyOutput("xrpprice")),
                box( plotlyOutput("vixprice")),
                box( plotlyOutput("sp500price"))
                
              )),
      tabItem(tabName = "ledger",
              
              tableOutput("values")))
    # useShinyjs(),
    # extendShinyjs(text = jscode),
   ))

# Define server logic required to draw a histogram
server <- function(input,output){
  
  sliderValues <- reactive({
 
    bid = current$bid
    ask = current$ask
    price = current$price
    vol = current$volume
    
    date =as.character(current$time)
    
    btcAlert = "No BTC Wallet Constraints!"
    usdAlert = "No USD Wallet Constraints!"
    
   
    
    
    technical=1 #input$technical  # Range of 1,.5, 0, -.5, -1  These Feed in from Algorithms
    sentiment =1# input$sentiment # Range of 1,.5, 0, -.5, -1   These Feed in from Algorithms
  
    riskBTC = input$riskBTC/100        #User defined value (basically percentage of wallet available for transaction) 
    riskUSD = input$riskUSD/100        #User defined value (basically percentage of wallet available for transaction)
    print(paste("riskUSD:",riskUSD))
    print(paste("riskBTC:",riskBTC))
    
    
    rangerWeight = input$rangerWeight
    rfWeight = input$rfWeight
    gbmWeight = input$gbmWeight
    svmWeight = input$svmWeight
    h2oWeight = input$h2oWeight
    lstmWeight = input$lstmWeight
  
    rangerOneMean = oneDayMeans[1]
    rfOneMean = oneDayMeans[2]
    gbmOneMean = oneDayMeans[3]
   svmOneMean = oneDayMeans[4]
    h2oOneMean = oneDayMeans[5]
    lstmOneMean = oneDayMeans[6]
    
    rangerAdjust = (rangerWeight/sum(rangerWeight,rfWeight, gbmWeight,svmWeight,h2oWeight,lstmWeight))*rangerOneMean
    rfAdjust = (rfWeight/sum(rangerWeight,rfWeight, gbmWeight,svmWeight,h2oWeight,lstmWeight))*rfOneMean
    gbmAdjust = (gbmWeight/sum(rangerWeight,rfWeight, gbmWeight,svmWeight,h2oWeight,lstmWeight))*gbmOneMean
    svmAdjust = (svmWeight/sum(rangerWeight,rfWeight, gbmWeight,svmWeight,h2oWeight,lstmWeight))*svmOneMean
    h2oAdjust = (h2oWeight/sum(rangerWeight,rfWeight, gbmWeight,svmWeight,h2oWeight,lstmWeight))*h2oOneMean
    lstmAdjust = (lstmWeight/sum(rangerWeight,rfWeight, gbmWeight,svmWeight,h2oWeight,lstmWeight))*lstmOneMean
    
    consensusOneDay = sum(rangerAdjust,rfAdjust,gbmAdjust,svmAdjust,h2oAdjust,lstmAdjust)
    
    sentVtech = consensusOneDay
    #User defined value (the amount of weight each algo will receive: 1 = full sentiment, 9 = full technical, 5 = equal mix)

    #---------> Combines the user defined weights to identify a multiplier <------------------
   
    
    
    combiVal = consensusOneDay
    
    
    #---------> Calculates amount of wallet available to use <------------------
    walletUSD =as.numeric(transactions$walletUSD[nrow(transactions)])
    print(paste("walletUSD:",walletUSD))
    walletBTC =as.numeric(transactions$walletBTC[nrow(transactions)])
    print(paste("walletBTC:",walletBTC))
    
    playUSD=walletUSD *riskUSD
    playBTC=walletBTC*riskBTC
    print(paste("playUSD:",playUSD))
    print(paste("playBTC:",playBTC))
    
    playUSDAvailable = playUSD
    playBTCAvailable = playBTC
    
    
    
    if((playUSD/price) < .001)
    {
      if(walletUSD > .001*price)
      {
        usdAlert = paste("Oops, you selected a USD wallet risk percentage that GDAX does not support:",round(playUSD/price, digits =4),"BTC.  I am going to default you to .001 BTC, the minimum for a trade")
        playUSDAvailable = .001*price
      } else { playUSDAvailable = 0
      usdAlert = paste("Oops, your wallet has insufficient funds to meet the .001 BTC trade minimum.  ")
      }
    }
    
    if((playBTC) < .001)
    {
      if(walletBTC > .001)
      {
        playBTCAvailable = .001
      } else { playBTCAvailable = 0
      usdAlert = paste("Oops, your wallet has insufficient funds to meet the .001 BTC trade minimum.  ")
      }
    }
    
    print(paste("playUSDAvailable:",playUSDAvailable))
    print(paste("playBTCAvailable:",playBTCAvailable))
    btcXaction = 0
    usdXaction = 0
    
    canBuy = playUSDAvailable/price 
    canSell = playBTCAvailable*price
    
    print(paste("canBuy:",canBuy))
    print(paste("canSell:",canSell))
    recommendation =0
    #----------> Calculates the appropriate move <---------------
    
    if (combiVal == 3)
    {
      recommendation =paste("Based upon your selections and the sentiment/technical indicators from the last week, it appears there will be very little change in Bitcoin price over the next 24 hours to justify any position change.  Therefore, I recommend no transactions. If you wish to log a transaction of zero, go ahead anc click the submit order button below. ")
      walletBTC = walletBTC
      walletUSD = walletUSD
    }else if(combiVal > 3 && canBuy>0)
    {
      
      walletBTC = walletBTC+canBuy
      walletUSD = walletUSD-playUSD
      recommendation =paste("I think you should buy some Bitcoin.  Based upon sentiment and technical indicators from the past week, it appears that Bitcoin price will increase over the next 24 hour period.  I recommend you buy",round(canBuy, digits = 4), "bitcoin for $", round(playUSD, digits = 2), ".  That would put your bitcoin wallet at:", round(walletBTC, digits = 4), "BTC and your USD wallet at: $", round(walletUSD, digits = 2),".  If you agree with this transaction (Buy Low), click the submit order button below.")
      btcXaction = +canBuy
      usdXaction = -playUSD
    }else if(combiVal < 3 && canSell>0)
    {
      
      walletBTC = walletBTC-playBTC
      walletUSD =walletUSD +canSell
      btcXaction = -playBTC
      usdXaction = +canSell
      recommendation = paste("I think you should sell some Bitcoin.  Based upon sentiment and technical indicators from the past week, it appears that Bitcoin price will decrease over the next 24 hour period.  I recommend you sell",round(playBTC, digits = 4), "bitcoin for $", round(canSell, digits = 2), ".  That would put your bitcoin wallet at:", round(walletBTC, digits = 4), "BTC and your USD wallet at: $", round(walletUSD, digits = 2),".  If you agree with this transaction (Sell High), click the submit order button below.")
    } else { recommendation = paste("Trade denied to to insuficient funds:", round(canBuy, digits = 4),"USD and ", round(canSell, digits = 4),"BTC")}
    
    print(paste("walletBTC:",walletBTC))
    print(paste("walletUSD:",walletUSD))
    print(paste("btcXaction:",btcXaction))
    print(paste("usdXaction:",usdXaction))
    walletValue = (price*walletBTC)+walletUSD
    print(paste("walletValue:",walletValue))
    
    delta = walletValue-as.numeric(transactions$walletValue[nrow(transactions)-1]) #change in wallet value from previous
   value = data.frame(date,  bid, ask, price, vol, technical, sentiment, consensusOneDay, riskBTC, riskUSD, btcXaction, usdXaction,walletBTC,walletUSD, walletValue, delta, recommendation,rangerWeight,rfWeight, gbmWeight,svmWeight,h2oWeight,lstmWeight,rangerOneMean,rfOneMean,gbmOneMean,svmOneMean,h2oOneMean,lstmOneMean)
 
   
    return(value)
    
   
    
  })
  output$values <- renderTable({
   data =  transactions
   data = data[,c(1,11,12,13,14,15,16)]
   data
  })
  
  output$recommendation <- renderText({
    data=sliderValues()
    response = as.character(data[1,17])
    return(response)
  })
  output$oneDayConsensusGauge = renderPlot({
    data=sliderValues()
    oneDayConsensusGaugeVal =     data$consensusOneDay
    sentNorm = (oneDayConsensusGaugeVal)*20
    oneDayConsensusGauge= gg.gauge(sentNorm,breaks=c(0,30,50,70,100))
    oneDayConsensusGauge
  })
  
  output$twoDayConsensusGaugeVal = renderPlot({
    data=sliderValues()
    twoDayConsensusGaugeVal =     data$consensusOneDay
    sentNorm = (twoDayConsensusGaugeVal)*20
    twoDayConsensusGauge= gg.gauge(sentNorm,breaks=c(0,30,50,70,100))
    twoDayConsensusGauge
  })
  
  # output$combiGauge = renderPlot({
  #   data = sliderValues()
  #   technical = data$technical
  #   sentiment = data$sentiment
  #   sentVtech = input$sentVtech       #User defined value (the amount of weight each algo will receive: 1 = full sentiment, 9 = full technical, 5 = equal mix)
  #     sentVal = sentiment*((10- sentVtech)/10)
  #   techVal = technical*(sentVtech/10)
  #   combiVal = techVal+sentVal
  #   sentNorm = ((combiVal+1)/2)*100
  #   combiGauge= gg.gauge(sentNorm,breaks=c(0,30,50,70,100))
  #   combiGauge
  # })
  #------------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  output$btcprice <- renderPlotly(
   
    bitcoinChart
  )
  output$ethprice <- renderPlotly(
    etherChart
  )
  output$ltcprice <- renderPlotly(
    liteChart
  )
  output$xrpprice <- renderPlotly(
    rippleChart
  )
  output$vixprice <- renderPlotly(
    vixChart
  )
  
  output$sp500price <- renderPlotly(
   sp500Chart
  )
  
  output$allPlot <- renderPlot(
    allPlot
  )
  

  
  writeCSV <- observe({
    if(input$submitButton == 0) return()
    data = sliderValues()
    data = data[,1:ncol(data)-1]
    data[,1] = as.character(data[,1])
    print(data)
    print(transactions)
     transactions[nrow(transactions) + 1,] = data[1,]
     print(transactions)
     write.csv(transactions, file = "transactions.csv")
     print("writing Complete")
   #  js$refresh();
  })
  
  # Invalid color: light red. Valid colors are: red, yellow, 
  # aqua, blue, light-blue, green, navy, teal, 
  # olive, lime, orange, fuchsia, purple, maroon, black.
  
  output$rangerOne <- renderInfoBox({
    mean = round(oneDayMeans[1], digits = 0)
    modelName = paste("1 Day", "Ranger(", paste0(RangerDayAccuracy,"%"),")")
    if(mean == 1)
     {
      output = infoBox(
        modelName, "Big Downward Movement", icon = icon("angle-double-down"),
        color = "red", fill = TRUE
      )
    } else  if(mean == 2)
    {
      output = infoBox(
        modelName, "Small Downward Movement", icon = icon("angle-down"),
        color = "orange", fill = TRUE
      )
    } else  if(mean == 3)
    {
      output = infoBox(
        
        modelName, "No Movement", icon = icon("angle-right"),
        color = "aqua", fill = TRUE
      )
    } else  if(mean == 4)
    {
      output = infoBox(
        modelName, "Small Upward Movement", icon = icon("angle-up"),
        color = "lime", fill = TRUE
      )
    } else  if(mean == 5)
    {
      output = infoBox(
      modelName, "Big Upward Movement", icon = icon("angle-double-up"),
        color = "green", fill = TRUE
      )
    } 
    output
    })
  
  output$rfOne <- renderInfoBox({
    mean = round(oneDayMeans[2], digits = 0)
    modelName = paste("1 Day", "RF(", paste0(RFDayAccuracy,"%"),")")
    if(mean == 1)
    {
      output = infoBox(
        modelName, "Big Downward Movement", icon = icon("angle-double-down"),
        color = "red", fill = TRUE
      )
    } else  if(mean == 2)
    {
      output = infoBox(
        modelName, "Small Downward Movement", icon = icon("angle-down"),
        color = "orange", fill = TRUE
      )
    } else  if(mean == 3)
    {
      output = infoBox(
        
        modelName, "No Movement", icon = icon("angle-right"),
        color = "aqua", fill = TRUE
      )
    } else  if(mean == 4)
    {
      output = infoBox(
        modelName, "Small Upward Movement", icon = icon("angle-up"),
        color = "lime", fill = TRUE
      )
    } else  if(mean == 5)
    {
      output = infoBox(
        modelName, "Big Upward Movement", icon = icon("angle-double-up"),
        color = "green", fill = TRUE
      )
    } 
    output
  })
  
  output$gbmOne <- renderInfoBox({
    mean = round(oneDayMeans[3], digits = 0)
    modelName = paste("1 Day", "GBM(", paste0(GBMDayAccuracy,"%"),")")
    if(mean == 1)
    {
      output = infoBox(
        modelName, "Big Downward Movement", icon = icon("angle-double-down"),
        color = "red", fill = TRUE
      )
    } else  if(mean == 2)
    {
      output = infoBox(
        modelName, "Small Downward Movement", icon = icon("angle-down"),
        color = "orange", fill = TRUE
      )
    } else  if(mean == 3)
    {
      output = infoBox(
        
        modelName, "No Movement", icon = icon("angle-right"),
        color = "aqua", fill = TRUE
      )
    } else  if(mean == 4)
    {
      output = infoBox(
        modelName, "Small Upward Movement", icon = icon("angle-up"),
        color = "lime", fill = TRUE
      )
    } else  if(mean == 5)
    {
      output = infoBox(
        modelName, "Big Upward Movement", icon = icon("angle-double-up"),
        color = "green", fill = TRUE
      )
    } 
    output
  })
  
  output$svmOne <- renderInfoBox({
    mean = round(oneDayMeans[4], digits = 0)
    modelName = paste("1 Day", "SVM(", paste0(RFDayAccuracy,"%"),")")
    if(mean == 1)
    {
      output = infoBox(
        modelName, "Big Downward Movement", icon = icon("angle-double-down"),
        color = "red", fill = TRUE
      )
    } else  if(mean == 2)
    {
      output = infoBox(
        modelName, "Small Downward Movement", icon = icon("angle-down"),
        color = "orange", fill = TRUE
      )
    } else  if(mean == 3)
    {
      output = infoBox(
        
        modelName, "No Movement", icon = icon("angle-right"),
        color = "aqua", fill = TRUE
      )
    } else  if(mean == 4)
    {
      output = infoBox(
        modelName, "Small Upward Movement", icon = icon("angle-up"),
        color = "lime", fill = TRUE
      )
    } else  if(mean == 5)
    {
      output = infoBox(
        modelName, "Big Upward Movement", icon = icon("angle-double-up"),
        color = "green", fill = TRUE
      )
    } 
    output
  })
  
  output$h2oOne <- renderInfoBox({
    mean = round(oneDayMeans[5], digits = 0)
    modelName = paste("1 Day", "H2O(", paste0(h2oAccuracyDay,"%"),")")
    if(mean == 1)
    {
      output = infoBox(
        modelName, "Big Downward Movement", icon = icon("angle-double-down"),
        color = "red", fill = TRUE
      )
    } else  if(mean == 2)
    {
      output = infoBox(
        modelName, "Small Downward Movement", icon = icon("angle-down"),
        color = "orange", fill = TRUE
      )
    } else  if(mean == 3)
    {
      output = infoBox(
        
        modelName, "No Movement", icon = icon("angle-right"),
        color = "aqua", fill = TRUE
      )
    } else  if(mean == 4)
    {
      output = infoBox(
        modelName, "Small Upward Movement", icon = icon("angle-up"),
        color = "lime", fill = TRUE
      )
    } else  if(mean == 5)
    {
      output = infoBox(
        modelName, "Big Upward Movement", icon = icon("angle-double-up"),
        color = "green", fill = TRUE
      )
    } 
    output
  })
  
  output$lstmOne <- renderInfoBox({
    mean = round(oneDayMeans[6], digits = 0)
    modelName = paste("1 Day", "LSTM (", paste0(RFDayAccuracy,"% rf for now"),")")
   
    if(mean == 1)
    {
      output = infoBox(
        modelName, "Big Downward Movement", icon = icon("angle-double-down"),
        color = "red", fill = TRUE
      )
    } else  if(mean == 2)
    {
      output = infoBox(
        modelName, "Small Downward Movement", icon = icon("angle-down"),
        color = "orange", fill = TRUE
      )
    } else  if(mean == 3)
    {
      output = infoBox(
        
        modelName, "No Movement", icon = icon("angle-right"),
        color = "aqua", fill = TRUE
      )
    } else  if(mean == 4)
    {
      output = infoBox(
        modelName, "Small Upward Movement", icon = icon("angle-up"),
        color = "lime", fill = TRUE
      )
    } else  if(mean == 5)
    {
      output = infoBox(
        modelName, "Big Upward Movement", icon = icon("angle-double-up"),
        color = "green", fill = TRUE
      )
    } 
    output
  })
  
  output$oneDayPredictPlot <- renderPlot(
    oneDayPredictPlot
   
  )
  output$twoDayPredictPlot <- renderPlot(
    
    twoDayPredictPlot
  )
  output$oneDayPredictPlotWithHist <- renderPlot(
    oneDayPredictPlotWithHist
    
  )
  output$twoDayPredictPlotWithHist <- renderPlot(
    
    twoDayPredictPlotWithHist
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


