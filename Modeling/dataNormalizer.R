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

intervalTime = 30 # <<<<<-------------- Change this to adjust hourly sample interval
interval = paste(intervalTime,"mins")


sentimentValues = read.csv(gzfile("TITLE OF YOU FILE.csv"))
sentimentValues=sentimentValues[,2:ncol(sentimentValues)]
sentimentValues$cleaned = as_datetime(sentimentValues$cleaned) #----> setting cleaned as datetime
sentimentValues[is.na(sentimentValues)] <- 0

toBeNormalized = read.csv(gzfile("secondary_Predictor_Pull.csv.gz"))

toBeNormalized = toBeNormalized[,2:ncol(toBeNormalized)] #----> Removes the stupid X column

toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned) #----> setting cleaned as datetime

source = as.character(unique(toBeNormalized$name)) #----> Building list of all the source names
sourceCut = gsub(" ", "\\.", source) 

sentimentValues =  sentimentValues[ , -which(names(sentimentValues) %in% c(sourceCut,"India.Economic.Times."))]

#tbn = toBeNormalized
#toBeNormalized =tbn
sentimentValues$cleaned = as_datetime(sentimentValues$cleaned)
toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned)
#toBeNormalized <- merge(toBeNormalized,sentimentValues,by="cleaned")
toBeNormalized = cbind(toBeNormalized,sentimentValues)
toBeNormalized = toBeNormalized[,-ncol(toBeNormalized)]
toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned)
str(sentimentValues$cleaned)
str(toBeNormalized$cleaned)

#----------------------------------- Start of Server Downtime Function ----------------------------------------
forContinutity = read.csv("bitcoinPull 2018-01-25")
download = unique(as.data.frame(forContinutity$time_now_gmt))
colnames(download)="downloadTime"
download$downloadTime = as_datetime(download$downloadTime)
download$downloadTime = (paste0(format(download$downloadTime, format="%Y-%m-%d %H:%M")))
downloaded = as.data.frame(unique(download$downloadTime))
colnames(downloaded) = "date"
downloaded$date = as_datetime(downloaded$date)
downloaded = as.data.frame(sort(downloaded$date))
colnames(downloaded)="downloadTime"
downloaded$downloadTime
downloaded$downloadTime = as_datetime(downloaded$downloadTime)


# Take rows 1 to n-1 and subtract rows 2 to n:
downloaded$gap <- c(NA, with(downloaded, downloadTime[-1] - downloadTime[-nrow(downloaded)]))

# now, how often was the gap more than some amount of time?
# gap_threshold <- 20 # let's say, 30 seconds
# downloaded$over_thresh <- downloaded$gap > gap_threshold
# downloaded


df <- data.frame(DateTime = seq(min(downloaded$downloadTime),
                                max(downloaded$downloadTime), 
                                by=(60)))

ddf = merge(downloaded, df, by.x = 1, by.y = 1, all.y =T)
lookupTable = na.locf(ddf, fromLast = T)
lookupTable =(lookupTable)
lookupTable$downloadTime = as_datetime(lookupTable$downloadTime)

#toBeNormalized = read.csv(gzfile("secondary_Predictor_Pull.csv.gz"))
toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned)
toBeNormalized$cleaned = format(toBeNormalized$cleaned, format="%Y-%m-%d %H:%M")
str(toBeNormalized$cleaned)
toBeNormalized$cleaned = ymd_hm(toBeNormalized$cleaned)
test = merge(toBeNormalized,lookupTable, by.x = 1, by.y = 1,all.x =T)
removeNAs = na.omit(test)
removeServerDowntime = removeNAs[as.numeric(removeNAs$gap)<20,]
toBeNormalized = removeServerDowntime[,-ncol(removeServerDowntime)]
toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned)

#----------------------------------- End of Server Downtime Function ----------------------------------------
#toBeNormalized = toBeNormalized[,2:ncol(toBeNormalized)] #----> Removes the stupid X column

firstSource = first(source)  #----> Calculating the first source name
lastSource = last(source)   #----> Calculating the last source name

toBeNormalized = cbind(toBeNormalized, setNames( lapply(source, function(x) x=-1), source) ) #----> sets default value to -1 for sources

firstSourceIndex = which(colnames(toBeNormalized)==firstSource) #----> finding index value for first source
lastSourceIndex = which(colnames(toBeNormalized)==lastSource)  #----> finding index value for first source

idx <- cbind(seq_len(nrow(toBeNormalized)), match(toBeNormalized$name, names(toBeNormalized)[-1]))
toBeNormalized[-1][idx] <- 1
#tot =merge.data.frame(toBeNormalized,sentimentValues,by.x = "cleaned", by.y = "cleaned" )






toBeNormalized = aggregate(toBeNormalized[,5:ncol(toBeNormalized)], list(cut(toBeNormalized$cleaned, breaks= interval)), mean)

names(toBeNormalized)[names(toBeNormalized) == 'Group.1'] <- 'cleaned'
#toBeNormalized = as.data.frame(toBeNormalized[!duplicated(toBeNormalized$cleaned),])
toBeNormalized = toBeNormalized[order(toBeNormalized$cleaned),]


rownames(toBeNormalized) = NULL  

toBeNormalized$AveragedExchange = as.numeric((toBeNormalized$KrakkenPrice+toBeNormalized$CoinbasePrice)/2)

toBeNormalized = toBeNormalized[200:nrow(toBeNormalized),]###################  ALERT!  INTERVAL

toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned)  #----> resetting date/time format


#-------------> export, tweak, import csv file to yank bad values

rateNormalization = data.frame(toBeNormalized[1:nrow(toBeNormalized)-1,])  #----> splitting DF

valueNormalization = data.frame(toBeNormalized) #----> splitting DF

firstSourceIndexMerged = which(colnames(toBeNormalized)==firstSource)  #----> finding new index value for first source




#----> VALUE NORMALIZATION....Goal is to get all spreadsheet values between -1 and 1 <------------------
pb <- txtProgressBar(min = 0, max = nrow(valueNormalization), style = 3)
for (i in 2:(firstSourceIndexMerged-1))
{
  setTxtProgressBar(pb, i)
  valueNormalization[,i] = (( valueNormalization[,i]-min( valueNormalization[,i]))-((max( valueNormalization[,i])-min( valueNormalization[,i]))/2))/(max( valueNormalization[,i])-min( valueNormalization[,i]))*2
 
}
close(pb)

for (i in ncol(valueNormalization):ncol(valueNormalization))
{
  setTxtProgressBar(pb, i)
  valueNormalization[,i] = (( valueNormalization[,i]-min( valueNormalization[,i]))-((max( valueNormalization[,i])-min( valueNormalization[,i]))/2))/(max( valueNormalization[,i])-min( valueNormalization[,i]))*2
  
}
close(pb)
#----> RATE NORMALIZATION....Find the rate change between i and i+1 and normalize to -1 to 1 <------------------
 x = 1 # This represents the time series

#rateNormalization[,1] = diff(valueNormalization[,1])
pb <- txtProgressBar(min = 0, max = nrow(valueNormalization), style = 3)
for (i in 2:ncol(rateNormalization))
{
  setTxtProgressBar(pb, i)
  rateNormalization[,i] = (diff(valueNormalization[,i])/2)
  
}
close(pb)

pb <- txtProgressBar(min = 0, max = nrow(valueNormalization), style = 3)
for (i in 2:ncol(rateNormalization))
{
  setTxtProgressBar(pb, i)
  rateNormalization[,i] = rescale(rateNormalization[,i],to=c(-1,1))
  }
close(pb)

rateNormalization$AveragedExchange = round(rateNormalization$AveragedExchange, digits = 1)



library(data.table)
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

rateNormalization = rateNormalization[,-c(2,3)]
valueNormalization = valueNormalization[,-c(2,3)]

z <- gzfile("finalRate.csv.gz")
write.csv(rateNormalization, z)

z <- gzfile("finalValue.csv.gz")
write.csv(valueNormalization,z)


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







