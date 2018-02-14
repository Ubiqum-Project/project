#install.packages("aspace")
library(aspace)
toBeNormalized = read.csv(gzfile("secondary_Predictor_Pull.csv.gz"))
toBeNormalized = toBeNormalized[!duplicated(toBeNormalized$cleaned),]



KrakkenPrice = toBeNormalized #[c("cleaned","KrakkenPrice")]
#KrakkenPrice = unique(KrakkenPrice)




for (i in 1:length(KrakkenPrice$KrakkenPrice))
{

  x = 1 # This represents the time series

  KrakkenPrice$deltaKrakken[i] = (tan_d(theta = (KrakkenPrice$KrakkenPrice[i+1]-KrakkenPrice$KrakkenPrice[i])/x))
  KrakkenPrice$deltaCoinbase[i] = (tan_d(theta = (KrakkenPrice$CoinbasePrice[i+1]-KrakkenPrice$CoinbasePrice[i])/x))
  
}

KrakkenPrice$tan= sin(as.numeric(KrakkenPrice$KrakkenPrice))

plot(KrakkenPrice$deltaKrakken, time)
time()
plot(KrakkenPrice$tan, breaks = 50)
line
plot(abs(KrakkenPrice$tan), log = "y", type = "h")
