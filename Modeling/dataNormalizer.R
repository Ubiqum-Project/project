#install.packages("aspace")
library(aspace)
library(lubridate)
library(gridExtra)

toBeNormalized = read.csv(gzfile("secondary_Predictor_Pull.csv.gz"))
toBeNormalized = toBeNormalized[,2:ncol(toBeNormalized)]

toBeNormalized$cleaned = as_datetime(toBeNormalized$cleaned)
toBeNormalized = as.data.frame(toBeNormalized[!duplicated(toBeNormalized$cleaned),])
toBeNormalized = toBeNormalized[order(toBeNormalized$cleaned),]

rownames(toBeNormalized) = NULL

rateNormalization = data.frame(toBeNormalized[1:nrow(toBeNormalized)-1,])
valueNormalization = data.frame(toBeNormalized)

nrow(toBeNormalized)-1

#----> VALUE NORMALIZATION....Goal is to get all spreadsheet values between -1 and 1 <------------------
pb <- txtProgressBar(min = 0, max = nrow(valueNormalization), style = 3)
for (i in 5:ncol(valueNormalization))
{
  setTxtProgressBar(pb, i)
  valueNormalization[,i] = (( valueNormalization[,i]-min( valueNormalization[,i]))-((max( valueNormalization[,i])-min( valueNormalization[,i]))/2))/(max( valueNormalization[,i])-min( valueNormalization[,i]))*2
 
}
close(pb)


#----> RATE NORMALIZATION....Find the rate change between i and i+1 and normalize to -1 to 1 <------------------
 x = 1 # This represents the time series
pb <- txtProgressBar(min = 0, max = nrow(valueNormalization), style = 3)
for (i in 5:ncol(rateNormalization))
{
  setTxtProgressBar(pb, i)
  rateNormalization[,i] = diff(valueNormalization[,i])/2
  
}
close(pb)

#------------------> Begin Plotting Magic <-----------------------------
valueNormalization$index = as.numeric(row.names(valueNormalization))

KrakkenPrice = ggplot(valueNormalization, aes(x = index, y = KrakkenPrice)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value KrakkenPrice", x = "Index/Time", y="Krakken Price")

CoinbasePrice = ggplot(valueNormalization, aes(x = index, y = CoinbasePrice)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value CoinbasePrice", x = "Index/Time", y="Coinbase Price")

gtrendLiteCrash = ggplot(valueNormalization, aes(x = index, y = gtrendLiteCrash)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value gtrendLiteCrash", x = "Index/Time", y="gtrendLiteCrash")

gtrendEtherCrash = ggplot(valueNormalization, aes(x = index, y = gtrendEtherCrash)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value gtrendEtherCrash", x = "Index/Time", y="gtrendEtherCrash")

gtrendEtherBubble = ggplot(valueNormalization, aes(x = index, y = gtrendEtherBubble)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value gtrendEtherBubble", x = "Index/Time", y="gtrendEtherBubble")

gtrendEther = ggplot(valueNormalization, aes(x = index, y = gtrendEther)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value gtrendEther", x = "Index/Time", y="gtrendEther")

gtrendBitcoinPrice = ggplot(valueNormalization, aes(x = index, y = gtrendBitcoinPrice)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value gtrendBitcoinPrice", x = "Index/Time", y="gtrendBitcoinPrice")

gtrendBitcoinTether = ggplot(valueNormalization, aes(x = index, y = gtrendBitcoinTether)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value gtrendBitcoinTether", x = "Index/Time", y="gtrendBitcoinTether")

gtrendBitcoinBubble = ggplot(valueNormalization, aes(x = index, y = gtrendBitcoinBubble)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value gtrendBitcoinBubble", x = "Index/Time", y="gtrendBitcoinBubble")

gtrendBitcoin = ggplot(valueNormalization, aes(x = index, y = gtrendBitcoin)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value gtrendBitcoin", x = "Index/Time", y="gtrendBitcoin")

GSPC_Adjusted = ggplot(valueNormalization, aes(x = index, y = GSPC_Adjusted)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value GSPC_Adjusted", x = "Index/Time", y="GSPC_Adjusted")

GSPC_Volume = ggplot(valueNormalization, aes(x = index, y = GSPC_Volume)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value GSPC_Volume", x = "Index/Time", y="GSPC_Volume")

GSPC_Close = ggplot(valueNormalization, aes(x = index, y = GSPC_Close)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value GSPC_Close", x = "Index/Time", y="GSPC_Close")

GSPC_Low = ggplot(valueNormalization, aes(x = index, y = GSPC_Low)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value GSPC_Low", x = "Index/Time", y="GSPC_Low")

GSPC_High = ggplot(valueNormalization, aes(x = index, y = GSPC_High)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value GSPC_High", x = "Index/Time", y="GSPC_High")

GSPC_Open = ggplot(valueNormalization, aes(x = index, y = GSPC_Open)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value GSPC_Open", x = "Index/Time", y="GSPC_Open")

VIX_Adjusted = ggplot(valueNormalization, aes(x = index, y = VIX_Adjusted)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value VIX_Adjusted", x = "Index/Time", y="VIX_Adjusted")

VIX_Close = ggplot(valueNormalization, aes(x = index, y = VIX_Close)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value VIX_Close", x = "Index/Time", y="VIX_Close")

VIX_Low = ggplot(valueNormalization, aes(x = index, y = VIX_Low)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value VIX_Low", x = "Index/Time", y="VIX_Low")

VIX_High = ggplot(valueNormalization, aes(x = index, y = VIX_High)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value VIX_High", x = "Index/Time", y="VIX_High")

VIX_Open = ggplot(valueNormalization, aes(x = index, y = VIX_Open)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value VIX_Open", x = "Index/Time", y="VIX_Open")

GOLD_Adjusted = ggplot(valueNormalization, aes(x = index, y = GOLD_Adjusted)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value GOLD_Adjusted", x = "Index/Time", y="GOLD_Adjusted")

GOLD_Volume = ggplot(valueNormalization, aes(x = index, y = GOLD_Volume)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value GOLD_Volume", x = "Index/Time", y="GOLD_Volume")

GOLD_Close = ggplot(valueNormalization, aes(x = index, y = GOLD_Close)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value GOLD_Close", x = "Index/Time", y="GOLD_Close")

GOLD_Low = ggplot(valueNormalization, aes(x = index, y = GOLD_Low)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value GOLD_Low", x = "Index/Time", y="GOLD_Low")

GOLD_High = ggplot(valueNormalization, aes(x = index, y = GOLD_High)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value GOLD_High", x = "Index/Time", y="GOLD_High")

GOLD_Open = ggplot(valueNormalization, aes(x = index, y = GOLD_Open)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value GOLD_Open", x = "Index/Time", y="GOLD_Open")

BTCUSDX_Adjusted = ggplot(valueNormalization, aes(x = index, y = BTCUSDX_Adjusted)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTCUSDX_Adjusted", x = "Index/Time", y="BTCUSDX_Adjusted")

BTCUSDX_Volume = ggplot(valueNormalization, aes(x = index, y = BTCUSDX_Volume)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTCUSDX_Volume", x = "Index/Time", y="BTCUSDX_Volume")

BTCUSDX_Close = ggplot(valueNormalization, aes(x = index, y = BTCUSDX_Close)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTCUSDX_Close", x = "Index/Time", y="BTCUSDX_Close")

BTCUSDX_Low = ggplot(valueNormalization, aes(x = index, y = BTCUSDX_Low)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTCUSDX_Low", x = "Index/Time", y="BTCUSDX_Low")

BTCUSDX_High = ggplot(valueNormalization, aes(x = index, y = BTCUSDX_High)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTCUSDX_High", x = "Index/Time", y="BTCUSDX_High")

BTCUSDX_Open = ggplot(valueNormalization, aes(x = index, y = BTCUSDX_Open)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTCUSDX_Open", x = "Index/Time", y="BTCUSDX_Open")

BTCUSDX_High = ggplot(valueNormalization, aes(x = index, y = BTCUSDX_High)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTCUSDX_High", x = "Index/Time", y="BTCUSDX_High")

BTCUSDX_Open = ggplot(valueNormalization, aes(x = index, y = BTCUSDX_Open)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTCUSDX_Open", x = "Index/Time", y="BTCUSDX_Open")

BTC_LTC_Adjusted = ggplot(valueNormalization, aes(x = index, y = BTC_LTC_Adjusted)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTC_LTC_Adjusted", x = "Index/Time", y="BTC_LTC_Adjusted")

BTC_LTC_Volume = ggplot(valueNormalization, aes(x = index, y = BTC_LTC_Volume)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTC_LTC_Volume", x = "Index/Time", y="BTC_LTC_Volume")

BTC_LTC_Close = ggplot(valueNormalization, aes(x = index, y = BTC_LTC_Close)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTC_LTC_Close", x = "Index/Time", y="BTC_LTC_Close")

BTC_LTC_Low = ggplot(valueNormalization, aes(x = index, y = BTC_LTC_Low)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTC_LTC_Low", x = "Index/Time", y="BTC_LTC_Low")

BTC_LTC_High = ggplot(valueNormalization, aes(x = index, y = BTC_LTC_High)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTC_LTC_High", x = "Index/Time", y="BTC_LTC_High")

BTC_LTC_Open = ggplot(valueNormalization, aes(x = index, y = BTC_LTC_Open)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTC_LTC_Open", x = "Index/Time", y="BTC_LTC_Open")

BTC_ETH_Adjusted = ggplot(valueNormalization, aes(x = index, y = BTC_ETH_Adjusted)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTC_ETH_Adjusted", x = "Index/Time", y="BTC_ETH_Adjusted")

BTC_ETH_Volume = ggplot(valueNormalization, aes(x = index, y = BTC_ETH_Volume)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTC_ETH_Volume", x = "Index/Time", y="BTC_ETH_Volume")

BTC_ETH_Close = ggplot(valueNormalization, aes(x = index, y = BTC_ETH_Close)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTC_ETH_Close", x = "Index/Time", y="BTC_ETH_Close")

BTC_ETH_Low = ggplot(valueNormalization, aes(x = index, y = BTC_ETH_Low)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTC_ETH_Low", x = "Index/Time", y="BTC_ETH_Low")

BTC_ETH_High = ggplot(valueNormalization, aes(x = index, y = BTC_ETH_High)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTC_ETH_High", x = "Index/Time", y="BTC_ETH_High")

BTC_ETH_Open = ggplot(valueNormalization, aes(x = index, y = BTC_ETH_Open)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Value BTC_ETH_Open", x = "Index/Time", y="BTC_ETH_Open")



grid.arrange(KrakkenPrice, CoinbasePrice, gtrendLiteCrash, gtrendEtherCrash,gtrendEtherBubble,gtrendEther,gtrendBitcoinPrice,        
             gtrendBitcoinTether,gtrendBitcoinBubble,gtrendBitcoin,GSPC_Adjusted,GSPC_Volume,GSPC_Close,GSPC_Low,
             GSPC_High,GSPC_Open,VIX_Adjusted,VIX_Close,VIX_Low,VIX_High,VIX_Open,GOLD_Adjusted,GOLD_Volume,
             GOLD_Close,GOLD_Low,GOLD_High,GOLD_Open,BTCUSDX_Adjusted,BTCUSDX_Volume,BTCUSDX_Close,
             BTCUSDX_Low,BTCUSDX_High,BTCUSDX_Open,BTCUSDX_High,BTCUSDX_Open,BTC_LTC_Adjusted,BTC_LTC_Volume,
             BTC_LTC_Close,BTC_LTC_Low,BTC_LTC_High,BTC_LTC_Open,BTC_ETH_Adjusted,BTC_ETH_Volume,
             BTC_ETH_Close,BTC_ETH_Low,BTC_ETH_High,BTC_ETH_Open, ncol=6)


#------------------------> Rate of Change Plots <--------------------------------------------------------

rateNormalization$index = as.numeric(row.names(rateNormalization))

KrakkenPrice = ggplot(rateNormalization, aes(x = index, y = KrakkenPrice)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate KrakkenPrice", x = "Index/Time", y="Krakken Price")

CoinbasePrice = ggplot(rateNormalization, aes(x = index, y = CoinbasePrice)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate CoinbasePrice", x = "Index/Time", y="Coinbase Price")

gtrendLiteCrash = ggplot(rateNormalization, aes(x = index, y = gtrendLiteCrash)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate gtrendLiteCrash", x = "Index/Time", y="gtrendLiteCrash")

gtrendEtherCrash = ggplot(rateNormalization, aes(x = index, y = gtrendEtherCrash)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate gtrendEtherCrash", x = "Index/Time", y="gtrendEtherCrash")

gtrendEtherBubble = ggplot(rateNormalization, aes(x = index, y = gtrendEtherBubble)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate gtrendEtherBubble", x = "Index/Time", y="gtrendEtherBubble")

gtrendEther = ggplot(rateNormalization, aes(x = index, y = gtrendEther)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate gtrendEther", x = "Index/Time", y="gtrendEther")

gtrendBitcoinPrice = ggplot(rateNormalization, aes(x = index, y = gtrendBitcoinPrice)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate gtrendBitcoinPrice", x = "Index/Time", y="gtrendBitcoinPrice")

gtrendBitcoinTether = ggplot(rateNormalization, aes(x = index, y = gtrendBitcoinTether)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate gtrendBitcoinTether", x = "Index/Time", y="gtrendBitcoinTether")

gtrendBitcoinBubble = ggplot(rateNormalization, aes(x = index, y = gtrendBitcoinBubble)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate gtrendBitcoinBubble", x = "Index/Time", y="gtrendBitcoinBubble")

gtrendBitcoin = ggplot(rateNormalization, aes(x = index, y = gtrendBitcoin)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate gtrendBitcoin", x = "Index/Time", y="gtrendBitcoin")

GSPC_Adjusted = ggplot(rateNormalization, aes(x = index, y = GSPC_Adjusted)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate GSPC_Adjusted", x = "Index/Time", y="GSPC_Adjusted")

GSPC_Volume = ggplot(rateNormalization, aes(x = index, y = GSPC_Volume)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate GSPC_Volume", x = "Index/Time", y="GSPC_Volume")

GSPC_Close = ggplot(rateNormalization, aes(x = index, y = GSPC_Close)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate GSPC_Close", x = "Index/Time", y="GSPC_Close")

GSPC_Low = ggplot(rateNormalization, aes(x = index, y = GSPC_Low)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate GSPC_Low", x = "Index/Time", y="GSPC_Low")

GSPC_High = ggplot(rateNormalization, aes(x = index, y = GSPC_High)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate GSPC_High", x = "Index/Time", y="GSPC_High")

GSPC_Open = ggplot(rateNormalization, aes(x = index, y = GSPC_Open)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate GSPC_Open", x = "Index/Time", y="GSPC_Open")

VIX_Adjusted = ggplot(rateNormalization, aes(x = index, y = VIX_Adjusted)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate VIX_Adjusted", x = "Index/Time", y="VIX_Adjusted")

VIX_Close = ggplot(rateNormalization, aes(x = index, y = VIX_Close)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate VIX_Close", x = "Index/Time", y="VIX_Close")

VIX_Low = ggplot(rateNormalization, aes(x = index, y = VIX_Low)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate VIX_Low", x = "Index/Time", y="VIX_Low")

VIX_High = ggplot(rateNormalization, aes(x = index, y = VIX_High)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate VIX_High", x = "Index/Time", y="VIX_High")

VIX_Open = ggplot(rateNormalization, aes(x = index, y = VIX_Open)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate VIX_Open", x = "Index/Time", y="VIX_Open")

GOLD_Adjusted = ggplot(rateNormalization, aes(x = index, y = GOLD_Adjusted)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate GOLD_Adjusted", x = "Index/Time", y="GOLD_Adjusted")

GOLD_Volume = ggplot(rateNormalization, aes(x = index, y = GOLD_Volume)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate GOLD_Volume", x = "Index/Time", y="GOLD_Volume")

GOLD_Close = ggplot(rateNormalization, aes(x = index, y = GOLD_Close)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate GOLD_Close", x = "Index/Time", y="GOLD_Close")

GOLD_Low = ggplot(rateNormalization, aes(x = index, y = GOLD_Low)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate GOLD_Low", x = "Index/Time", y="GOLD_Low")

GOLD_High = ggplot(rateNormalization, aes(x = index, y = GOLD_High)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate GOLD_High", x = "Index/Time", y="GOLD_High")

GOLD_Open = ggplot(rateNormalization, aes(x = index, y = GOLD_Open)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate GOLD_Open", x = "Index/Time", y="GOLD_Open")

BTCUSDX_Adjusted = ggplot(rateNormalization, aes(x = index, y = BTCUSDX_Adjusted)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTCUSDX_Adjusted", x = "Index/Time", y="BTCUSDX_Adjusted")

BTCUSDX_Volume = ggplot(rateNormalization, aes(x = index, y = BTCUSDX_Volume)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTCUSDX_Volume", x = "Index/Time", y="BTCUSDX_Volume")

BTCUSDX_Close = ggplot(rateNormalization, aes(x = index, y = BTCUSDX_Close)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTCUSDX_Close", x = "Index/Time", y="BTCUSDX_Close")

BTCUSDX_Low = ggplot(rateNormalization, aes(x = index, y = BTCUSDX_Low)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTCUSDX_Low", x = "Index/Time", y="BTCUSDX_Low")

BTCUSDX_High = ggplot(rateNormalization, aes(x = index, y = BTCUSDX_High)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTCUSDX_High", x = "Index/Time", y="BTCUSDX_High")

BTCUSDX_Open = ggplot(rateNormalization, aes(x = index, y = BTCUSDX_Open)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTCUSDX_Open", x = "Index/Time", y="BTCUSDX_Open")

BTCUSDX_High = ggplot(rateNormalization, aes(x = index, y = BTCUSDX_High)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTCUSDX_High", x = "Index/Time", y="BTCUSDX_High")

BTCUSDX_Open = ggplot(rateNormalization, aes(x = index, y = BTCUSDX_Open)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTCUSDX_Open", x = "Index/Time", y="BTCUSDX_Open")

BTC_LTC_Adjusted = ggplot(rateNormalization, aes(x = index, y = BTC_LTC_Adjusted)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTC_LTC_Adjusted", x = "Index/Time", y="BTC_LTC_Adjusted")

BTC_LTC_Volume = ggplot(rateNormalization, aes(x = index, y = BTC_LTC_Volume)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTC_LTC_Volume", x = "Index/Time", y="BTC_LTC_Volume")

BTC_LTC_Close = ggplot(rateNormalization, aes(x = index, y = BTC_LTC_Close)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTC_LTC_Close", x = "Index/Time", y="BTC_LTC_Close")

BTC_LTC_Low = ggplot(rateNormalization, aes(x = index, y = BTC_LTC_Low)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTC_LTC_Low", x = "Index/Time", y="BTC_LTC_Low")

BTC_LTC_High = ggplot(rateNormalization, aes(x = index, y = BTC_LTC_High)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTC_LTC_High", x = "Index/Time", y="BTC_LTC_High")

BTC_LTC_Open = ggplot(rateNormalization, aes(x = index, y = BTC_LTC_Open)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTC_LTC_Open", x = "Index/Time", y="BTC_LTC_Open")

BTC_ETH_Adjusted = ggplot(rateNormalization, aes(x = index, y = BTC_ETH_Adjusted)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTC_ETH_Adjusted", x = "Index/Time", y="BTC_ETH_Adjusted")

BTC_ETH_Volume = ggplot(rateNormalization, aes(x = index, y = BTC_ETH_Volume)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTC_ETH_Volume", x = "Index/Time", y="BTC_ETH_Volume")

BTC_ETH_Close = ggplot(rateNormalization, aes(x = index, y = BTC_ETH_Close)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTC_ETH_Close", x = "Index/Time", y="BTC_ETH_Close")

BTC_ETH_Low = ggplot(rateNormalization, aes(x = index, y = BTC_ETH_Low)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTC_ETH_Low", x = "Index/Time", y="BTC_ETH_Low")

BTC_ETH_High = ggplot(rateNormalization, aes(x = index, y = BTC_ETH_High)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTC_ETH_High", x = "Index/Time", y="BTC_ETH_High")

BTC_ETH_Open = ggplot(rateNormalization, aes(x = index, y = BTC_ETH_Open)) +
  geom_line(aes(),color = "red") +
  theme(
  )+
  labs(title = "Rate BTC_ETH_Open", x = "Index/Time", y="BTC_ETH_Open")



grid.arrange(KrakkenPrice, CoinbasePrice, gtrendLiteCrash, gtrendEtherCrash,gtrendEtherBubble,gtrendEther,gtrendBitcoinPrice,        
             gtrendBitcoinTether,gtrendBitcoinBubble,gtrendBitcoin,GSPC_Adjusted,GSPC_Volume,GSPC_Close,GSPC_Low,
             GSPC_High,GSPC_Open,VIX_Adjusted,VIX_Close,VIX_Low,VIX_High,VIX_Open,GOLD_Adjusted,GOLD_Volume,
             GOLD_Close,GOLD_Low,GOLD_High,GOLD_Open,BTCUSDX_Adjusted,BTCUSDX_Volume,BTCUSDX_Close,
             BTCUSDX_Low,BTCUSDX_High,BTCUSDX_Open,BTCUSDX_High,BTCUSDX_Open,BTC_LTC_Adjusted,BTC_LTC_Volume,
             BTC_LTC_Close,BTC_LTC_Low,BTC_LTC_High,BTC_LTC_Open,BTC_ETH_Adjusted,BTC_ETH_Volume,
             BTC_ETH_Close,BTC_ETH_Low,BTC_ETH_High,BTC_ETH_Open, ncol=6)












