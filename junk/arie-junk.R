library(readr)

cleaned.n <- read_csv("~/Desktop/cleaned_w_Krakken_Coinbase_Cex.csv")

cleaned.n2 <- read_csv("~/Desktop/secondary_Predictor_Pull.csv")
cleaned.n3 <- read_csv(gzfile("secondary_Predictor_Pull.csv.gz"))

par(mfrow=c(1,2))
plot(cleaned.n$date_Krakken, cleaned.n$price_USD_BTC_Krakken)
plot(cleaned.n$date_Coinbase, cleaned.n$price_USD_BTC_Coinbase)
plot(cleaned.n2$cexDate, cleaned.n2$gtrendBitcoinExchangeHack)


summary(cleaned.n2$gtrendBitcoinExchangeHack)

bla <- ggplot(data = cleaned.n2, aes(x = as.POSIXct(cleaned.n2$cleaned), y = as.numeric(cleaned.n2$gtrendBitcoinBubble*8000+13000)), color = "red") + 
  geom_line(data = cleaned.n2, aes(x =cleaned.n2$cleaned, y = cleaned.n2$CoinbasePrice), color = "blue") +
  geom_line(data = cleaned.n2, aes(x =cleaned.n2$cleaned, y = cleaned.n2$gtrendBitcoinBubble*100+13000), color = "red") +
  geom_line(data = cleaned.n2, aes(x =cleaned.n2$cleaned, y = cleaned.n2$gtrendBitcoin*100+13000), color = "green") +
  geom_line(data = cleaned.n2, aes(x =cleaned.n2$cleaned, y = cleaned.n2$gtrendBitcoinBounce*100+13000), color = "blue") +
  geom_line(data = cleaned.n2, aes(x =cleaned.n2$cleaned, y = cleaned.n2$gtrendBitcoinExchangeHack*100+13000), color = "black") +
  #geom_line(data = cleaned.n2, aes(x =cleaned.n2$cleaned, y = cleaned.n2$gtrendBitcoinBubble*100+13000), color = "green") +
  #geom_line(data = cleaned.n2, aes(x = as.POSIXct(plotBing$time_now_gmt), y = as.numeric(plotBing$sentiment*100+5000)), color = "blue")+
  #geom_smooth(color = "blue") +
  #geom_line(data = plotAfinn, aes(x = as.POSIXct(plotAfinn$time_now_gmt), y = as.numeric(plotAfinn$afinnSentiment+4000)), color = "blue")+
  #geom_smooth(color = "green")+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "bubble and Price Over Time")
bla

bla3 <- ggplot(data = cleaned.n3, aes(x = as.POSIXct(cleaned.n3$cleaned), y = as.numeric(cleaned.n3$gtrendBitcoinBubble*8000+13000)), color = "red") + 
  geom_line(data = cleaned.n3, aes(x =cleaned.n3$cleaned, y = cleaned.n3$CoinbasePrice), color = "blue") +
  geom_point(data = cleaned.n3, aes(x =cleaned.n3$cleaned, y = cleaned.n3$gtrendBitcoinBubble*100+13000), color = "red") +
  geom_point(data = cleaned.n3, aes(x =cleaned.n3$cleaned, y = cleaned.n3$gtrendBitcoin*100+13000), color = "green") +
  geom_point(data = cleaned.n3, aes(x =cleaned.n3$cleaned, y = cleaned.n3$gtrendBitcoinPrice*100+13000), color = "blue") +
  geom_point(data = cleaned.n3, aes(x =cleaned.n3$cleaned, y = cleaned.n3$gtrendBitcoinTether*100+13000), color = "black") +
  #geom_line(data = cleaned.n2, aes(x =cleaned.n2$cleaned, y = cleaned.n2$gtrendBitcoinBubble*100+13000), color = "green") +
  #geom_line(data = cleaned.n2, aes(x = as.POSIXct(plotBing$time_now_gmt), y = as.numeric(plotBing$sentiment*100+5000)), color = "blue")+
  #geom_smooth(color = "blue") +
  #geom_line(data = plotAfinn, aes(x = as.POSIXct(plotAfinn$time_now_gmt), y = as.numeric(plotAfinn$afinnSentiment+4000)), color = "blue")+
  #geom_smooth(color = "green")+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "bubble and Price Over Time")
bla3

cleaned.n$mean=rowMeans(cleaned.n[,c("CoinbasePrice", "KrakkenPrice")], na.rm=TRUE)

str(cleaned.n2$date)
cleaned.n2$date <- as.POSIXct(cleaned.n2$date)


p <- ggplot(cleaned.n2, aes(cleaned.n2$cleaned, KrakkenPrice))
p + geom_line() +
geom_line(data = cleaned.n2, aes(x = cleaned.n2$cleaned, y = cleaned.n2$CoinbasePrice), color = "blue") +
geom_line(data = cleaned.n2, aes(x = cleaned.n2$date, y = 'BTCUSD=X.Close'), color = "red")

#geom_line(data = cleaned.n2, aes(x =cleaned.n2$KrakkenDate, y = cleaned.n2$mean), color = "green")

result <- na.omit(result)
x <- ggplot(result, aes(x = time, y = total_nrc))
x + geom_line() 

x <- ggplot(result, aes(x = time, y = total_bing))
x + geom_line() 

x <- ggplot(result, aes(x = time, y = total_afinn))
x + geom_line() 

x <- ggplot(result, aes(x = time, y = total_syuzhet))
x + geom_line() 

x <- ggplot(result, aes(x = time, y = total_nrc)) +
geom_line(data = result, aes(x = time, y = total_bing), color = "blue") +
  geom_line(data = result, aes(x = time, y = total_afinn), color = "green") +
  geom_line(data = result, aes(x = time, y = total_syuzhet), color = "red") 
x

result <- cbind(Text.art$time, result)
colnames(result)[1] <- "time"


