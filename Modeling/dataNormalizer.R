#install.packages("aspace")
library(aspace)
library(lubridate)
library(gridExtra)
library(pastecs)
require(ggplot2)
require(reshape2)
library(readr)
library(data.table)
library(zoo)
library(scales)
library(qdap)
library(clusterSim)
library(data.table)

intervalTime = 30 # <<<<<-------------- Change this to adjust hourly sample interval
interval = paste(intervalTime,"mins")

toBeNormalized = read.csv(gzfile("secondary_Predictor_Pull.csv.gz"), stringsAsFactors=FALSE)
toBeNormalized = toBeNormalized[,2:ncol(toBeNormalized)] #----> Removes the stupid X column
toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned) #----> setting cleaned as datetime
source = as.character(unique(toBeNormalized$name)) #----> Building list of all the source names
sourceCut = gsub(" ", "\\.", source) 


toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned)
toBeNormalized = toBeNormalized[,-ncol(toBeNormalized)]
toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned)

toBeNormalized[ ][is.na(toBeNormalized[] ) ] = 0
sapply(toBeNormalized, function(x) sum(is.na(x)))
#####################################################################################################################
#
#            Start of Server Downtime Function (ONLY FOR MODEL TRAINING) 
#
#             DISABLE CODE BELOW for Routine Operating
######################################################################################################################
# forContinutity = read.csv("DBPull.csv.gz")
# download = unique(as.data.frame(forContinutity$time_now_gmt))
# colnames(download)="downloadTime"
# download$downloadTime = as_datetime(download$downloadTime)
# download$downloadTime = (paste0(format(download$downloadTime, format="%Y-%m-%d %H:%M")))
# downloaded = as.data.frame(unique(download$downloadTime))
# colnames(downloaded) = "date"
# downloaded$date = as_datetime(downloaded$date)
# downloaded = as.data.frame(sort(downloaded$date))
# colnames(downloaded)="downloadTime"
# downloaded$downloadTime
# downloaded$downloadTime = as_datetime(downloaded$downloadTime)
# 
# 
# # Take rows 1 to n-1 and subtract rows 2 to n:
# downloaded$gap <- c(NA, with(downloaded, downloadTime[-1] - downloadTime[-nrow(downloaded)]))
# 
# df <- data.frame(DateTime = seq(min(downloaded$downloadTime),
#                                 max(downloaded$downloadTime), 
#                                 by=(60)))
# 
# ddf = merge(downloaded, df, by.x = 1, by.y = 1, all.y =T)
# lookupTable = na.locf(ddf, fromLast = T)
# lookupTable =(lookupTable)
# lookupTable$downloadTime = as_datetime(lookupTable$downloadTime)
# 
# toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned)
# toBeNormalized$cleaned = as.character(format(toBeNormalized$cleaned, format="%Y-%m-%d %H:%M"))
# lookupTable$downloadTime = as.character(format(lookupTable$downloadTime, format="%Y-%m-%d %H:%M"))
# str(toBeNormalized$cleaned)
# test = merge(toBeNormalized,lookupTable, by.x = ncol(toBeNormalized), by.y = 1,all.x = T)
# 
# sapply(test, function(x) sum(is.na(x)))
# removeNAs = na.omit(test)
# removeServerDowntime = removeNAs[as.numeric(removeNAs$gap)<20,]
# toBeNormalized = removeServerDowntime[,-ncol(removeServerDowntime)]
# toBeNormalized$cleaned = ymd_hm(toBeNormalized$cleaned)

#          ^^^   DISABLE CODE Above for Routine Operating ^^^^
######################################################################################################################

# toBeNormalized[, 2:ncol(toBeNormalized)] = as.data.frame(sapply(toBeNormalized[, 2:ncol(toBeNormalized)], as.character))
# toBeNormalized$BTC_ETH_Volume = as.numeric(toBeNormalized$BTC_ETH_Volume)
# toBeNormalized[, 2:ncol(toBeNormalized)] = sapply(toBeNormalized[, 2:ncol(toBeNormalized)], as.numeric)
toBeNormalized$cleaned
toBeNormalized$AveragedExchange = as.numeric((toBeNormalized$KrakkenPrice+toBeNormalized$CoinbasePrice)/2)
toBeNormalized = toBeNormalized[,!(colnames(toBeNormalized) == c("CoinbaseDate", "KrakkenDate", "join_time"))]
toBeNormalized = aggregate(toBeNormalized[,3:ncol(toBeNormalized)], list(cut(toBeNormalized$cleaned, breaks= interval)), mean)

names(toBeNormalized)[names(toBeNormalized) == 'Group.1'] <- 'cleaned'
toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned)
toBeNormalized = toBeNormalized[order(toBeNormalized$cleaned),]


rownames(toBeNormalized) = NULL  


#--------------> This knocks out the first 200 entries which had some issues.  Only for server cleaning
#toBeNormalized = toBeNormalized[200:nrow(toBeNormalized),]###################  ALERT!  INTERVAL

toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned)  #----> resetting date/time format

#toBeNormalized = toBeNormalized[,c(1,3:ncol(toBeNormalized))]


#-------------> export, tweak, import csv file to yank bad values

rateNormalization = data.frame(toBeNormalized[1:nrow(toBeNormalized)-1,])  #----> splitting DF
valueNormalization = data.frame(toBeNormalized) #----> splitting DF
valueNormalization[,2:ncol(valueNormalization)] = data.Normalization(valueNormalization[,2:ncol(valueNormalization)],type="n5")
valueNormalization = valueNormalization[1:nrow(valueNormalization)-1,]






#----> RATE NORMALIZATION....Find the rate change between i and i+1 and normalize to -1 to 1 <------------------
 x = 1 # This represents the time series


pb <- txtProgressBar(min = 0, max = nrow(toBeNormalized), style = 3)
for (i in 2:ncol(rateNormalization))
{
  setTxtProgressBar(pb, i)
  rateNormalization[,i] = (diff(toBeNormalized[,i])/2)
  
}
close(pb)

rateNormalization[,2:ncol(rateNormalization)] = data.Normalization(rateNormalization[,2:ncol(rateNormalization)],type="n5")


rateNormalization$AveragedExchange = round(rateNormalization$AveragedExchange, digits = 1)

vN = setDT(valueNormalization, keep.rownames = TRUE)[]
vN = vN[,-2]
RN = setDT(rateNormalization, keep.rownames = TRUE)[]
RN = RN[,-2]
valueNormalization = vN
colnames(valueNormalization[,1]) = "cleaned"
rateNormalization = RN
colnames(rateNormalization[,1]) = "cleaned"
rateNormalization$rn = as.numeric(rateNormalization$rn)
valueNormalization$rn = as.numeric(valueNormalization$rn)



data = valueNormalization


data2 = rateNormalization

data$AveragedExchange = as.numeric(data$AveragedExchange) 

data$AveragedExchange[data$AveragedExchange<= -.5] = -1

data$AveragedExchange[data$AveragedExchange> -.5 & data$AveragedExchange< .01] = -.5

data$AveragedExchange[data$AveragedExchange>= -.2 & data$AveragedExchange<= .2] = 0


data$AveragedExchange[data$AveragedExchange<= .5 & data$AveragedExchange> .01] = .5

data$AveragedExchange[data$AveragedExchange> .5 ] = 1


data$AveragedExchange[data$AveragedExchange == 1] = 5

data$AveragedExchange[data$AveragedExchange== -1] = 1

data$AveragedExchange[data$AveragedExchange == -.5] = 2

data$AveragedExchange[data$AveragedExchange==0] = 3

data$AveragedExchange[data$AveragedExchange== .5] = 4



data$AveragedExchange = as.factor(data$AveragedExchange)
data[ ][is.na(data[] ) ] = 0


drops = c("VIX_Volume","X", "KrakkenDate", "time", "cleaned")

data = as.data.frame(data)


data = data[,!(colnames(data) %in% drops)]
# data = data[ , apply(data, 2, function(x) !any(is.na(x)))]


# nzv <- nearZeroVar(data)
# data <- data[,-nzv]
################################################################
#-----> Removing Low Res Data
#data = data[,which( colnames(data)=="article" ):ncol(data)]
################################################################
#--------------> Experimental Merging

# 

print("data 1 preprocessed Loaded")
#data2 = data2[,-3]
data2 = as.data.frame(data2)
data2 = data2[,-ncol(data2)]

#data2 = data2[,which( colnames(data2)=="article" ):ncol(data2)]
#data2 = data2[ , apply(data2, 2, function(x) !any(is.na(x)))]
data2[ ][is.na(data2[] ) ] = 0
drops2 = c("VIX_Volume","X", "KrakkenDate", "time","rn", "cleaned")
data2 = as.data.frame(data2)
data2 = data2[,!(names(data2) %in% drops2)]
data3 = cbind(data, data2)
# data3 = data3[,-1]
# 
data = data3



z <- gzfile("RoutineDataOutput.csv.gz")
write.csv(data, z)




#-------------> Plot of Value <----------------------------------------
valueMelt <- melt(valueNormalization ,  id.vars = 'rn', variable.name = 'series')

# plot on same grid, each series colored differently --
# good if the series have same scale
seismicValue = ggplot(valueMelt, aes(rn,value)) + geom_line(aes(colour = series))+
  labs(title = "Plot of 4 Hour Normalized Values")

seismicValue
# or plot on different plots
gridValue = ggplot(valueMelt, aes(rn,value)) + geom_line() +facet_wrap( ~ series, ncol = 7)+
  labs(title = "Plot of 4 Hour Normalized Values")

gridValue
#-------------> Plot of Rate <----------------------------------------
rateMelt <- melt(rateNormalization ,  id.vars = 'rn', variable.name = 'series')

# plot on same grid, each series colored differently --
# good if the series have same scale
seismicRate = ggplot(rateMelt, aes(rn,value)) + geom_line(aes(colour = series))+
  labs(title = "Plot of 4 Hour Normalized Rate (Delta from previous day + or -)")

  seismicRate
  # or plot on different plots
  gridRate =   ggplot(rateMelt, aes(rn,value)) + geom_line() +facet_wrap( ~ series, ncol = 7)+
  labs(title = "Plot of 4 Hour Normalized Rate (Delta from previous day + or -)")

gridRate
  ggsave("plots/gridValue.jpeg",gridValue, device = "jpeg", width = 15, height = 10, limitsize = F)
  ggsave("plots/gridRate.jpeg",gridRate, device = "jpeg", width = 15, height = 10, limitsize = F)
  ggsave("plots/seismicValue.jpeg",seismicValue, device = "jpeg", width = 15, height = 10, limitsize = F)
  ggsave("plots/seismicRate.jpeg",seismicRate, device = "jpeg", width = 15, height = 10, limitsize = F)







