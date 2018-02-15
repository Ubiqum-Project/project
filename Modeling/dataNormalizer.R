#install.packages("aspace")
library(aspace)
toBeNormalized = read.csv(gzfile("secondary_Predictor_Pull.csv.gz"))
toBeNormalized = toBeNormalized[!duplicated(toBeNormalized$cleaned),]



rateNormalization = toBeNormalized #[c("cleaned","KrakkenPrice")]
valueNormalization = toBeNormalized

#----> VALUE NORMALIZATION....Goal is to get all spreadsheet values between -1 and 1 <------------------

valueNormalization$gtrendBitcoin = (valueNormalization$gtrendBitcoin/50)-1
valueNormalization$gtrendBitcoinBubble = (valueNormalization$gtrendBitcoinBubble/50)-1
valueNormalization$gtrendBitcoinPrice= (valueNormalization$gtrendBitcoinPrice/50)-1
valueNormalization$gtrendBitcoinTether= (valueNormalization$gtrendBitcoinTether/50)-1
valueNormalization$gtrendEther= (valueNormalization$gtrendEther/50)-1
valueNormalization$gtrendEtherBubble= (valueNormalization$gtrendEtherBubble/50)-1
valueNormalization$gtrendEtherCrash= (valueNormalization$gtrendEtherCrash/50)-1
valueNormalization$gtrendLiteCrash= (valueNormalization$gtrendLiteCrash/50)-1

valueNormalization$KrakkenPrice = log(valueNormalization$KrakkenPrice)/10
valueNormalization$CoinbasePrice = log(valueNormalization$CoinbasePrice)/10
valueNormalization$GSPC_Adjusted= log(valueNormalization$GSPC_Adjusted)/10


valueNormalization$GSPC_Volume= log(valueNormalization$GSPC_Volume)-22

valueNormalization$GSPC_Close= log(valueNormalization$GSPC_Close)/10
valueNormalization$GSPC_Low= log(valueNormalization$GSPC_Low)/10
valueNormalization$GSPC_High= log(valueNormalization$GSPC_High)/10
valueNormalization$GSPC_Open= log(valueNormalization$GSPC_Open)/10
valueNormalization$VIX_Adjusted= log(valueNormalization$VIX_Adjusted)/10
valueNormalization$VIX_Close= log(valueNormalization$VIX_Close)/10
valueNormalization$VIX_Low= log(valueNormalization$VIX_Low)/10
valueNormalization$VIX_High= log(valueNormalization$VIX_High)/10
valueNormalization$VIX_Open= log(valueNormalization$VIX_Open)/10

valueNormalization$GOLD_Adjusted= log(valueNormalization$GOLD_Adjusted)/10
valueNormalization$GOLD_Volume= log(valueNormalization$GOLD_Volume)/10
valueNormalization$GOLD_Low= log(valueNormalization$GOLD_Low)/10
valueNormalization$GOLD_High= log(valueNormalization$GOLD_High)/10
valueNormalization$GOLD_Open= log(valueNormalization$GOLD_Open)/10

valueNormalization$BTCUSDX_Adjusted= log(valueNormalization$BTCUSDX_Adjusted)/10
valueNormalization$BTCUSDX_Volume= log(valueNormalization$BTCUSDX_Volume)/10
valueNormalization$BTCUSDX_Close= log(valueNormalization$BTCUSDX_Close)/10
valueNormalization$BTCUSDX_Low= log(valueNormalization$BTCUSDX_Low)/10
valueNormalization$BTCUSDX_High= log(valueNormalization$BTCUSDX_High)/10
valueNormalization$BTCUSDX_Open= log(valueNormalization$BTCUSDX_Open)/10

valueNormalization$BTC_LTC_Adjusted= log(valueNormalization$BTC_LTC_Adjusted)/10
valueNormalization$BTC_LTC_Volume= log(valueNormalization$BTC_LTC_Volume)/10
valueNormalization$BTC_LTC_Close= log(valueNormalization$BTC_LTC_Close)/10
valueNormalization$BTC_LTC_Low= log(valueNormalization$BTC_LTC_Low)/10
valueNormalization$BTC_LTC_High= log(valueNormalization$BTC_LTC_High)/10
valueNormalization$BTC_LTC_Open= log(valueNormalization$BTC_LTC_Open)/10

valueNormalization$BTC_ETH_Adjusted= log(valueNormalization$BTC_ETH_Adjusted)/10
valueNormalization$BTC_ETH_Volume= log(valueNormalization$BTC_ETH_Volume)/10
valueNormalization$BTC_ETH_Close= log(valueNormalization$BTC_ETH_Close)/10
valueNormalization$BTC_ETH_Low= log(valueNormalization$BTC_ETH_Low)/10
valueNormalization$BTC_ETH_High= log(valueNormalization$BTC_ETH_High)/10
valueNormalization$BTC_ETH_Open= log(valueNormalization$BTC_ETH_Open)/10

valueNormalization$KrakkenPrice = sin(valueNormalization$KrakkenPrice)
valueNormalization$KrakkenPrice = sin(valueNormalization$KrakkenPrice)
valueNormalization$KrakkenPrice = sin(valueNormalization$KrakkenPrice)
valueNormalization$KrakkenPrice = sin(valueNormalization$KrakkenPrice)
valueNormalization$KrakkenPrice = sin(valueNormalization$KrakkenPrice)
valueNormalization$KrakkenPrice = sin(valueNormalization$KrakkenPrice)


valueNormalization$CoinbasePrice

valueNormalization$gtrendBitcoin[1] = (KrakkenPrice$gtrendBitcoin[1]/50)-1
KrakkenPrice$gtrendBitcoin[1]
(46/50)-1

toBeNormalized$gtrendBitcoin[1]

#----> RATE NORMALIZATION....Find the rate change between i and i+1 and normalize to -1 to 1 <------------------
 x = 1 # This represents the time series

 
for (i in 1:length(KrakkenPrice$KrakkenPrice))
{


  KrakkenPrice$deltaKrakkenTAN[i] = tan((tan_d(theta = (KrakkenPrice$KrakkenPrice[i+1]-KrakkenPrice$KrakkenPrice[i])/x)))/900
  KrakkenPrice$deltaCoinbaseTAN[i] = tan((tan_d(theta = (KrakkenPrice$CoinbasePrice[i+1]-KrakkenPrice$CoinbasePrice[i])/x)))/10000
  

  
  
}
plot(KrakkenPrice$deltaCoinbaseTAN)
plot(KrakkenPrice$deltaKrakkenTAN)


(KrakkenPrice$gtrendBitcoin[1]-50)/10

n=70
(n/50)-1
