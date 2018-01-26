library(meanr)
library(RPostgreSQL)
library(readxl)
library(sentimentr)
library(syuzhet)
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(qdap)
library(gridExtra)
library(readr)

#####  Connect your csv file here ###################

cleaned = #---- read CSV Here-----------------

# colnames(cleaned$api_price_ltc)= "api_bid_btc"
# 
# # cleaned = cleanedEther
# # colnames(cleanedEther$api_price_eth)= "api_bid_btc"
# 
# cleaned = unique(setDT(newDB), by = c('title'), fromLast = FALSE)    #NOT FOR USE  THIS filters unique values only
# #cleaned = cleanedEther
# 
# cleaned = na.omit(cleaned)


cleaned$combined = paste(cleaned$title, cleaned$paragraph)

cleaned$nrcSentiment = get_sentiment(cleaned$combined, method="nrc")

cleaned$bingSentiment = get_sentiment(cleaned$combined, method="bing")

cleaned$afinnSentiment = get_sentiment(cleaned$combined, method="afinn")

cleaned$syuzhetSentiment = get_sentiment(cleaned$combined, method="syuzhet")

meanr = score(cleaned$combined)
cleaned$meanrSentiment = meanr$score

polarity = polarity(cleaned$combined)
cleaned$polaritySentiment = polarity$all$polarity
preprocessed = as.data.frame(0)
preprocessed$combined = paste(preprocessed$combined, " neutral")

sentimentr = sentiment(preprocessed$combined)

sentimentr = sentiment(cleaned$combined)

sentimentr = aggregate(.~sentimentr$element_id, data=sentimentr, mean)

cleaned$sentimentrSentiment = sentimentr$sentiment

#cleaned = bkup
#cleaned = backup 
searchTerms = c( "BBC (litecoin)")


cleaned = cleaned[cleaned$name  %in% searchTerms,]


#cleaned = cleaned[cleaned$time_now_gmt >= "2017-12-10"  ,]

plotNRC = ddply(cleaned, 'time_now_gmt', summarise, sentiment = (mean(nrcSentiment)))

plotBing = ddply(cleaned, 'time_now_gmt', summarise, sentiment = mean(bingSentiment))

plotAfinn = ddply(cleaned, 'time_now_gmt', summarise, sentiment = mean(afinnSentiment))

plotSyuzhet = ddply(cleaned, 'time_now_gmt', summarise, sentiment = mean(syuzhetSentiment))

plotSentimentr = ddply(cleaned, 'time_now_gmt', summarise, sentiment = mean(sentimentrSentiment))

plotMeanr = ddply(cleaned, 'time_now_gmt', summarise, sentiment = mean(meanrSentiment))

plotPolarity = ddply(cleaned, 'time_now_gmt', summarise, sentiment = mean(polaritySentiment))

cleaned = na.omit(cleaned)

cleaned$api_bid_btc = as.numeric(cleaned$api_bid_btc)
cleaned$time_now_gmt = as.POSIXct(cleaned$time_now_gmt)


NRC = ggplot(data = plotNRC, aes(x = as.POSIXct(plotNRC$time_now_gmt), y = as.numeric(plotNRC$sentiment*8000+11000)), color = "purple") + 
  geom_point(data = cleaned, aes(x =cleaned$time_now_gmt, y = cleaned$api_bid_btc), color = "black") + 
  geom_line(data = plotNRC, aes(x = as.POSIXct(plotNRC$time_now_gmt), y = as.numeric(plotNRC$sentiment*1000+5000)), color = "purple")+
  geom_smooth(color = "purple")+
  geom_hline(yintercept=5000, linetype="dashed", color = "black")+
  # geom_line(data = plotAfinn, aes(x = as.POSIXct(plotAfinn$time_now_gmt), y = as.numeric(plotAfinn$afinnSentiment+4000)), color = "blue")+
  # geom_smooth(color = "blue")+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "NRC and Price Over Time")
NRC

AFINN = ggplot(data = plotAfinn, aes(x = as.POSIXct(plotAfinn$time_now_gmt), y = as.numeric(plotAfinn$sentiment*8000+11000)), color = "red") + 
  geom_point(data = cleaned, aes(x =cleaned$time_now_gmt, y = cleaned$api_bid_btc), color = "black") + 
  geom_line(data = plotAfinn, aes(x = as.POSIXct(plotAfinn$time_now_gmt), y = as.numeric(plotAfinn$sentiment*100+5000)), color = "red")+
  geom_smooth(color = "red")+
  # geom_line(data = plotAfinn, aes(x = as.POSIXct(plotAfinn$time_now_gmt), y = as.numeric(plotAfinn$afinnSentiment+4000)), color = "blue")+
  # geom_smooth(color = "blue")+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "AFINN and Price Over Time")
AFINN


BING = ggplot(data = plotBing, aes(x = as.POSIXct(plotBing$time_now_gmt), y = as.numeric(plotBing$sentiment*8000+13000)), color = "blue") + 
  geom_point(data = cleaned, aes(x =cleaned$time_now_gmt, y = cleaned$api_bid_btc), color = "black") + 
  geom_line(data = plotBing, aes(x = as.POSIXct(plotBing$time_now_gmt), y = as.numeric(plotBing$sentiment*100+5000)), color = "blue")+
  geom_smooth(color = "blue")+
  # geom_line(data = plotAfinn, aes(x = as.POSIXct(plotAfinn$time_now_gmt), y = as.numeric(plotAfinn$afinnSentiment+4000)), color = "blue")+
  # geom_smooth(color = "green")+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "BING and Price Over Time")
BING

SZUZ = ggplot(data = plotSyuzhet, aes(x = as.POSIXct(plotSyuzhet$time_now_gmt), y = as.numeric(plotSyuzhet$sentiment*8000+12000)), color = "yellow") + 
  geom_point(data = cleaned, aes(x =cleaned$time_now_gmt, y = cleaned$api_bid_btc), color = "black") + 
  geom_line(data = plotSyuzhet, aes(x = as.POSIXct(plotSyuzhet$time_now_gmt), y = as.numeric(plotSyuzhet$sentiment*100+5000)), color = "yellow")+
  geom_smooth(color = "yellow")+
  # geom_line(data = plotAfinn, aes(x = as.POSIXct(plotAfinn$time_now_gmt), y = as.numeric(plotAfinn$afinnSentiment+4000)), color = "blue")+
  # geom_smooth(color = "blue")+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "SZUZ and Price Over Time")
SZUZ

SENTR = ggplot(data = plotSentimentr, aes(x = as.POSIXct(plotSentimentr$time_now_gmt), y = as.numeric(plotSentimentr$sentiment*50000+12000)), color = "green") + 
  geom_point(data = cleaned, aes(x =cleaned$time_now_gmt, y = cleaned$api_bid_btc), color = "black") + 
  geom_point(data = plotSentimentr, aes(x = as.POSIXct(plotSentimentr$time_now_gmt), y = as.numeric(plotSentimentr$sentiment*5000+5000)), color = "green")+
  geom_smooth(color = "green")+
  geom_smooth(data = cleaned, aes(color = "black", x = as.POSIXct(cleaned$time_now_gmt), y = as.numeric(cleaned$api_bid_btc)))+
  scale_y_continuous(sec.axis = sec_axis(~./50000, name = "Relative Sentiment"))+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "Sentimentr and Price Over Time")
SENTR

MEANR = ggplot(data = plotMeanr, aes(x = as.POSIXct(plotMeanr$time_now_gmt), y = as.numeric(plotMeanr$sentiment*8000+12000)), color = "plum") + 
  geom_point(data = cleaned, aes(x =cleaned$time_now_gmt, y = cleaned$api_bid_btc), color = "black") + 
  geom_line(data = plotMeanr, aes(x = as.POSIXct(plotMeanr$time_now_gmt), y = as.numeric(plotMeanr$sentiment*100+5000)), color = "plum")+
  geom_smooth(color = "plum")+
  
  
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "Meanr and Price Over Time")
MEANR

POLR = ggplot(data = plotPolarity, aes(x = as.POSIXct(plotPolarity$time_now_gmt), y = as.numeric((plotPolarity$sentiment)*80000+14000)), color = "brown") + 
  geom_point(data = cleaned, aes(x =cleaned$time_now_gmt, y = cleaned$api_bid_btc), color = "black") + 
  geom_line(data = plotPolarity, aes(x = as.POSIXct(plotPolarity$time_now_gmt), y = as.numeric(plotPolarity$sentiment*100+5000)), color = "brown")+
  geom_smooth(color = "brown")+
  
  
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "Polarity() and Price Over Time")
POLR

SENTR = ggplot(data = plotSentimentr, aes(x = as.POSIXct(plotSentimentr$time_now_gmt), y = as.numeric(plotSentimentr$sentiment*-80000+10000+8000)), color = "green") + 
  geom_point(data = cleaned, aes(x =cleaned$time_now_gmt, y = cleaned$api_bid_btc), color = "black") + 
  geom_line(data = plotSentimentr, aes(x = as.POSIXct(plotSentimentr$time_now_gmt), y = as.numeric(plotSentimentr$sentiment*100+5000)), color = "green")+
  geom_smooth(color = "green")+
  geom_smooth(data = cleaned, aes(color = "black", x = as.POSIXct(cleaned$time_now_gmt), y = as.numeric(cleaned$api_bid_btc)))+
  
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "Sentimentr and Price Over Time")
SENTR

COMB = ggplot(data = plotSentimentr, aes(x = as.POSIXct(plotSentimentr$time_now_gmt), y = as.numeric(plotSentimentr$sentiment*-80000+10000)), color = "green") + 
  geom_point(data = cleaned, aes(x =cleaned$time_now_gmt, y = cleaned$api_bid_btc), color = "black") + 
  geom_line(data = plotSentimentr, aes(x = as.POSIXct(plotSentimentr$time_now_gmt), y = as.numeric(plotSentimentr$sentiment*100+7000)), color = "green")+
  geom_smooth(color = "green")+
  #--------> AFINN
  geom_line(data = plotAfinn, aes(x = as.POSIXct(plotAfinn$time_now_gmt), y = as.numeric(plotAfinn$sentiment*100+6000)), color = "blue")+
  geom_smooth(aes(color = "blue", x = as.POSIXct(plotAfinn$time_now_gmt), y = as.numeric(plotAfinn$sentiment*-8000+11000)))+
  #--------> BING
  geom_line(data = plotBing, aes(x = as.POSIXct(plotBing$time_now_gmt), y = as.numeric(plotBing$sentiment*100+5000)), color = "red")+
  geom_smooth(aes(color = "red", x = as.POSIXct(plotBing$time_now_gmt), y = as.numeric(plotBing$sentiment*-8000+13000)))+
  #--------> NRC
  geom_line(data = plotNRC , aes(x = as.POSIXct(plotNRC$time_now_gmt), y = as.numeric(plotNRC$sentiment*100+4000)), color = "purple")+
  geom_smooth(aes(color = "purple", x = as.POSIXct(plotNRC$time_now_gmt), y = as.numeric(plotNRC$sentiment*-8000+11000)))+
  #--------> Syuzhet
  geom_line(data = plotSyuzhet, aes(x = as.POSIXct(plotSyuzhet$time_now_gmt), y = as.numeric(plotSyuzhet$sentiment*100+3000)), color = "yellow")+
  geom_smooth(aes(color = "yellow", x = as.POSIXct(plotSyuzhet$time_now_gmt), y = as.numeric(plotSyuzhet$sentiment*-8000+12000)))+
  #--------> Meanr
  geom_line(data = plotMeanr, aes(x = as.POSIXct(plotMeanr$time_now_gmt), y = as.numeric(plotMeanr$sentiment*100+2000)), color = "brown")+
  geom_smooth(aes(color = "brown", x = as.POSIXct(plotMeanr$time_now_gmt), y = as.numeric(plotMeanr$sentiment*-8000+12000)))+
  #--------> Polarity()
  geom_line(data = plotPolarity, aes(x = as.POSIXct(plotPolarity$time_now_gmt), y = as.numeric(plotPolarity$sentiment*100+1000)), color = "plum")+
  geom_smooth(aes(color = "plum", x = as.POSIXct(plotPolarity$time_now_gmt), y = as.numeric(plotPolarity$sentiment*-80000+12000)))+
  scale_y_continuous(sec.axis = sec_axis(~./8000, name = "Relative Sentiment"))+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "Combined Plot with all Models")
COMB

quadPlot = grid.arrange(NRC, AFINN, BING, SZUZ,  POLR, MEANR,SENTR, COMB,ncol = 2, nrow = 4)

quadPlot




#######################End of Database Section ####################################

