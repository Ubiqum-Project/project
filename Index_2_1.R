#INDEX 2_1 

#Index2_1 <- function (sentiment.used,Text.art$text) 

#List of data used
result <- cbind(Text.art$article, as.data.frame(get_sentiment(Text.art$text, method="bing")))
result <- cbind(result, as.data.frame(get_sentiment(Text.art$text, method="nrc")))
result <- cbind(result, as.data.frame(get_sentiment(Text.art$text, method="afinn")))
result <- cbind(result, as.data.frame(get_sentiment(Text.art$text, method="syuzhet")))
sentimentr <- sentiment(Text.art$text)
result <- cbind(result,sentimentr[,4])
rm(sentimentr)

colnames(result)[1] <- "article"
colnames(result)[2] <- "total_bing"
colnames(result)[3] <- "total_nrc"
colnames(result)[4] <- "total_afinn"
colnames(result)[5] <- "total_syuzhet"
colnames(result)[6] <- "total_sentimentr"


