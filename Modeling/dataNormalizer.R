#install.packages("aspace")
library(aspace)
library(lubridate)
library(gridExtra)
library(pastecs)
require(ggplot2)
require(reshape2)

intervalTime = 4 # <<<<<-------------- Change this to adjust hourly sample interval
interval = paste(intervalTime,"hour")

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
toBeNormalized <- merge(toBeNormalized,sentimentValues,by=c("cleaned"))

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

toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned)  #----> resetting date/time format
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
z <- gzfile("finalRate.csv.gz")
write.csv(rateNormalization, z)

z <- gzfile("finalValue.csv.gz")
write.csv(valueNormalization,z)


#-------------> Plot of Value <----------------------------------------
valueMelt <- melt(valueNormalization ,  id.vars = 'cleaned', variable.name = 'series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
seismicValue = ggplot(valueMelt, aes(cleaned,value)) + geom_line(aes(colour = series))+
  labs(title = "Plot of 4 Hour Normalized Values")


# or plot on different plots
gridValue = ggplot(valueMelt, aes(cleaned,value)) + geom_line() +facet_wrap( ~ series, ncol = 7)+
  labs(title = "Plot of 4 Hour Normalized Values")

#-------------> Plot of Rate <----------------------------------------
rateMelt <- melt(rateNormalization ,  id.vars = 'cleaned', variable.name = 'series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
seismicRate = ggplot(rateMelt, aes(cleaned,value)) + geom_line(aes(colour = series))+
  labs(title = "Plot of 4 Hour Normalized Rate (Delta from previous day + or -)")
  
  
  # or plot on different plots
  gridRate =   ggplot(rateMelt, aes(cleaned,value)) + geom_line() +facet_wrap( ~ series, ncol = 7)+
  labs(title = "Plot of 4 Hour Normalized Rate (Delta from previous day + or -)")

  
  ggsave("plots/gridValue.jpeg",gridValue, device = "jpeg", width = 15, height = 10, limitsize = F)
  ggsave("plots/gridRate.jpeg",gridRate, device = "jpeg", width = 15, height = 10, limitsize = F)
  ggsave("plots/seismicValue.jpeg",seismicValue, device = "jpeg", width = 15, height = 10, limitsize = F)
  ggsave("plots/seismicRate.jpeg",seismicRate, device = "jpeg", width = 15, height = 10, limitsize = F)







