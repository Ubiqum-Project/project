#Do not used the NegationWords text
Working_table<-Text.art%>%
  filter(grepl(paste(NegationWords[1],"\\w"),text )==FALSE)

for(i in 2:length(NegationWords))
{Working_Table<-Text.art%>%
  filter(grepl(paste(NegationWords[i],"\\w"),text )==FALSE)

Working_table<-Working_table%>%
  full_join(Working_Table)
}


#TIME
T1<-Sys.time()

print("bing 1/4")
result <- cbind(Working_Table$article, as.data.frame(get_sentiment(Working_Table$text, method="bing")))
#TIME
print(difftime(T1, Sys.time()))

print("nrc 2/4")
result <- cbind(result, as.data.frame(get_sentiment(Working_Table$text, method="nrc")))
#TIME
print(difftime(T1, Sys.time()))

print("afinn 3/4")
result <- cbind(result, as.data.frame(get_sentiment(Working_Table$text, method="afinn")))
#TIME
print(difftime(T1, Sys.time()))

print("syuzhet 4/4")
result <- cbind(result, as.data.frame(get_sentiment(Working_Table$text, method="syuzhet")))
#TIME
print(difftime(T1, Sys.time()))

print("merge")
#sentimentr <- sentiment(Working_Table$text)
#result <- cbind(result,sentimentr[,4])
#rm(sentimentr)
colnames(result)[1] <- "article"
colnames(result)[2] <- "total_bing"
colnames(result)[3] <- "total_nrc"
colnames(result)[4] <- "total_afinn"
colnames(result)[5] <- "total_syuzhet"
#colnames(result)[6] <- "total_sentimentr"

#TIME
print(difftime(T1, Sys.time()))


return(result)