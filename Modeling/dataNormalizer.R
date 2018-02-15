#install.packages("aspace")
library(aspace)
library(lubridate)
toBeNormalized = read.csv(gzfile("secondary_Predictor_Pull.csv.gz"))
toBeNormalized = toBeNormalized[,2:ncol(toBeNormalized)]

toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned)
toBeNormalized = as.data.frame(toBeNormalized[!duplicated(toBeNormalized$cleaned),])
toBeNormalized = toBeNormalized[order(toBeNormalized$cleaned),]

rownames(toBeNormalized) = NULL

rateNormalization = data.frame(toBeNormalized)
valueNormalization = data.frame(toBeNormalized)

#----> VALUE NORMALIZATION....Goal is to get all spreadsheet values between -1 and 1 <------------------

for (i in 5:ncol(valueNormalization))
{
  valueNormalization[,i] = (( valueNormalization[,i]-min( valueNormalization[,i]))-((max( valueNormalization[,i])-min( valueNormalization[,i]))/2))/(max( valueNormalization[,i])-min( valueNormalization[,i]))*2
 
}

#####  NOT SORTED BY DATE!!!!!!!!!!!!!!!!!!!!!!!!!!

#----> RATE NORMALIZATION....Find the rate change between i and i+1 and normalize to -1 to 1 <------------------
 x = 1 # This represents the time series

for (i in 1:nrow(valueNormalization))
{
  
  rateNormalization[i,5] = valueNormalization[(i+1),5]-valueNormalization[i,5]
  
}

n = 2079
valueNormalization[(n+1),5]-valueNormalization[n,5]
valueNormalization[600,5]
length(valueNormalization)
