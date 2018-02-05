library(meanr)
library(RPostgreSQL)
library(readxl)
library(sentimentr)
library(syuzhet)
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
##library(qdap) <- does not work on my pc
library(gridExtra)
library(readr)
library(SentimentAnalysis)

#####  Input cleaned dataframe here ################### Why so many na's?
cleaned.x$time_now_gmt <-  as.POSIXct(cleaned.x$time_now_gmt)
df.s.t <- cleaned.x
df.s <-  na.omit(df.s.t)

#####  Sentiment ################### Currently title only! Please refer to text-analysis for changes
df.s$nrcSentiment = get_sentiment(df.s$text, method="nrc")
df.s$bingSentiment = get_sentiment(df.s$text, method="bing")
df.s$afinnSentiment = get_sentiment(df.s$text, method="afinn")
df.s$syuzhetSentiment = get_sentiment(df.s$text, method="syuzhet")

SentimentAnalysis <- with(df.s, analyzeSentiment(text))
df.s <- cbind(df.s, SentimentAnalysis)

meanr = score(df.s$text)
df.s$meanrSentiment <-  meanr$score

#for some, no text will result in NA, thus omit again
df.s <-  na.omit(df.s)

##### Sentimentr finds titles with word count 0?? More investigation needed
sentimentr = sentiment(df.s$text)
sentimentr$word_count[is.na(sentimentr$word_count)] <- 0
sentimentr = aggregate(.~sentimentr$element_id, data=sentimentr, mean)
df.s$sentimentrSentiment = sentimentr$sentiment

plotNRC = ddply(df.s, 'time_now_gmt', summarise, sentiment = (mean(nrcSentiment)))
plotBing = ddply(df.s, 'time_now_gmt', summarise, sentiment = mean(bingSentiment))
plotAfinn = ddply(df.s, 'time_now_gmt', summarise, sentiment = mean(afinnSentiment))
plotSyuzhet = ddply(df.s, 'time_now_gmt', summarise, sentiment = mean(syuzhetSentiment))
plotSentimentr = ddply(df.s, 'time_now_gmt', summarise, sentiment = mean(sentimentrSentiment))
plotMeanr = ddply(df.s, 'time_now_gmt', summarise, sentiment = mean(meanrSentiment))
plotGI= ddply(df.s, 'time_now_gmt', summarise, sentiment = mean(SentimentGI))
plotHE = ddply(df.s, 'time_now_gmt', summarise, sentiment = mean(SentimentHE))
plotQDAP = ddply(df.s, 'time_now_gmt', summarise, sentiment = mean(SentimentQDAP))
plotLM = ddply(df.s, 'time_now_gmt', summarise, sentiment = mean(SentimentLM))

#write_csv(df.s, "dfs.csv")

NRC = ggplot(data = plotNRC, aes(x = as.POSIXct(plotNRC$time_now_gmt), y = as.numeric(plotNRC$sentiment*8000+11000)), color = "purple") + 
  geom_point(data = df.s, aes(x =df.s$time_now_gmt, y = df.s$api_bid_btc), color = "black") + 
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
  geom_point(data = df.s, aes(x =df.s$time_now_gmt, y = df.s$api_bid_btc), color = "black") + 
  geom_line(data = plotAfinn, aes(x = as.POSIXct(plotAfinn$time_now_gmt), y = as.numeric(plotAfinn$sentiment*100+5000)), color = "red")+
  geom_smooth(color = "red")+
  # geom_line(data = plotAfinn, aes(x = as.POSIXct(plotAfinn$time_now_gmt), y = as.numeric(plotAfinn$afinnSentiment+4000)), color = "blue")+
  # geom_smooth(color = "blue")+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "AFINN and Price Over Time")
AFINN


BING = ggplot(data = plotBing, aes(x = as.POSIXct(plotBing$time_now_gmt), y = as.numeric(plotBing$sentiment*8000+13000)), color = "blue") + 
  geom_point(data = df.s, aes(x =df.s$time_now_gmt, y = df.s$api_bid_btc), color = "black") + 
  geom_line(data = plotBing, aes(x = as.POSIXct(plotBing$time_now_gmt), y = as.numeric(plotBing$sentiment*100+5000)), color = "blue")+
  geom_smooth(color = "blue")+
  # geom_line(data = plotAfinn, aes(x = as.POSIXct(plotAfinn$time_now_gmt), y = as.numeric(plotAfinn$afinnSentiment+4000)), color = "blue")+
  # geom_smooth(color = "green")+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "BING and Price Over Time")
BING

SZUZ = ggplot(data = plotSyuzhet, aes(x = as.POSIXct(plotSyuzhet$time_now_gmt), y = as.numeric(plotSyuzhet$sentiment*8000+12000)), color = "yellow") + 
  geom_point(data = df.s, aes(x =df.s$time_now_gmt, y = df.s$api_bid_btc), color = "black") + 
  geom_line(data = plotSyuzhet, aes(x = as.POSIXct(plotSyuzhet$time_now_gmt), y = as.numeric(plotSyuzhet$sentiment*100+5000)), color = "yellow")+
  geom_smooth(color = "yellow")+
  # geom_line(data = plotAfinn, aes(x = as.POSIXct(plotAfinn$time_now_gmt), y = as.numeric(plotAfinn$afinnSentiment+4000)), color = "blue")+
  # geom_smooth(color = "blue")+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "SZUZ and Price Over Time")
SZUZ


MEANR = ggplot(data = plotMeanr, aes(x = as.POSIXct(plotMeanr$time_now_gmt), y = as.numeric(plotMeanr$sentiment*8000+12000)), color = "plum") + 
  geom_point(data = df.s, aes(x =df.s$time_now_gmt, y = df.s$api_bid_btc), color = "black") + 
  geom_line(data = plotMeanr, aes(x = as.POSIXct(plotMeanr$time_now_gmt), y = as.numeric(plotMeanr$sentiment*100+5000)), color = "plum")+
  geom_smooth(color = "plum")+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "Meanr and Price Over Time")
MEANR


SENTR = ggplot(data = plotSentimentr, aes(x = as.POSIXct(plotSentimentr$time_now_gmt), y = as.numeric(plotSentimentr$sentiment*-80000+10000+8000)), color = "green") + 
  geom_point(data = df.s, aes(x =df.s$time_now_gmt, y = df.s$api_bid_btc), color = "black") + 
  geom_line(data = plotSentimentr, aes(x = as.POSIXct(plotSentimentr$time_now_gmt), y = as.numeric(plotSentimentr$sentiment*100+5000)), color = "green")+
  geom_smooth(color = "green")+
  geom_smooth(data = df.s, aes(color = "black", x = as.POSIXct(df.s$time_now_gmt), y = as.numeric(df.s$api_bid_btc)))+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "Sentimentr and Price Over Time")
SENTR

#newly added
GI = ggplot(data = plotGI, aes(x = as.POSIXct(plotSentimentr$time_now_gmt), y = as.numeric(plotGI$sentiment*-80000+10000+8000)), color = "green") + 
  geom_point(data = df.s, aes(x =df.s$time_now_gmt, y = df.s$api_bid_btc), color = "black") + 
  geom_line(data = plotSentimentr, aes(x = as.POSIXct(plotGI$time_now_gmt), y = as.numeric(plotGI$sentiment*100+5000)), color = "green")+
  geom_smooth(color = "green")+
  geom_smooth(data = df.s, aes(color = "black", x = as.POSIXct(df.s$time_now_gmt), y = as.numeric(df.s$api_bid_btc)))+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "GI and Price Over Time")
GI

HE = ggplot(data = plotGI, aes(x = as.POSIXct(plotHE$time_now_gmt), y = as.numeric(plotHE$sentiment*-80000+10000+8000)), color = "green") + 
  geom_point(data = df.s, aes(x =df.s$time_now_gmt, y = df.s$api_bid_btc), color = "black") + 
  geom_line(data = plotSentimentr, aes(x = as.POSIXct(plotHE$time_now_gmt), y = as.numeric(plotHE$sentiment*100+5000)), color = "green")+
  geom_smooth(color = "green")+
  geom_smooth(data = df.s, aes(color = "black", x = as.POSIXct(df.s$time_now_gmt), y = as.numeric(df.s$api_bid_btc)))+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "HE and Price Over Time")
HE

LM = ggplot(data = plotGI, aes(x = as.POSIXct(plotLM$time_now_gmt), y = as.numeric(plotLM$sentiment*-80000+10000+8000)), color = "green") + 
  geom_point(data = df.s, aes(x =df.s$time_now_gmt, y = df.s$api_bid_btc), color = "black") + 
  geom_line(data = plotSentimentr, aes(x = as.POSIXct(plotLM$time_now_gmt), y = as.numeric(plotLM$sentiment*100+5000)), color = "green")+
  geom_smooth(color = "green")+
  geom_smooth(data = df.s, aes(color = "black", x = as.POSIXct(df.s$time_now_gmt), y = as.numeric(df.s$api_bid_btc)))+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "LM and Price Over Time")
LM

QDAP = ggplot(data = plotGI, aes(x = as.POSIXct(plotQDAP$time_now_gmt), y = as.numeric(plotQDAP$sentiment*-80000+10000+8000)), color = "green") + 
  geom_point(data = df.s, aes(x =df.s$time_now_gmt, y = df.s$api_bid_btc), color = "black") + 
  geom_line(data = plotSentimentr, aes(x = as.POSIXct(plotQDAP$time_now_gmt), y = as.numeric(plotQDAP$sentiment*100+5000)), color = "green")+
  geom_smooth(color = "green")+
  geom_smooth(data = df.s, aes(color = "black", x = as.POSIXct(df.s$time_now_gmt), y = as.numeric(df.s$api_bid_btc)))+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "QDAP and Price Over Time")
QDAP

#combining
COMB = ggplot(data = plotSentimentr, aes(x = as.POSIXct(plotSentimentr$time_now_gmt), y = as.numeric(plotSentimentr$sentiment*-80000+10000)), color = "green") + 
  geom_point(data = df.s, aes(x =df.s$time_now_gmt, y = df.s$api_bid_btc), color = "black") + 
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
  scale_y_continuous(sec.axis = sec_axis(~./8000, name = "Relative Sentiment"))+
  xlab('Date') +
  ylab('Bitcoin Price')+
  labs(title = "Combined Plot with all Models")
COMB

quadPlot = grid.arrange(NRC, AFINN, BING, SZUZ, MEANR,SENTR, COMB,ncol = 2, nrow = 4)
quadPlot

#######################End ####################################

