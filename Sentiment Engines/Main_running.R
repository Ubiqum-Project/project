#MAIN RUNNING

#RUN Index Common
Final.table<-Index_0(Text.art)

#RUN INDEX SOURCE 1
Final.table<-Final.table%>%
  left_join(Index1_1(Text.art,Sentiment.list[1],TARGET_WORDS.count,quatrigrams_filtered))%>%
  left_join(Index1_1(Text.art,Sentiment.list[2],TARGET_WORDS.count,quatrigrams_filtered))%>%
  left_join(Index1_1(Text.art,Sentiment.list[3],TARGET_WORDS.count,quatrigrams_filtered))%>%
  left_join(Index1_1(Text.art,Sentiment.list[4],TARGET_WORDS.count,quatrigrams_filtered))

#RUN INDEX SOURCE 2


#Function used+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{
  #IndexMain
  
#Index0
  {
    Index_0 <- function(Text.art) 
    {
      #TIME
      T1<-Sys.time()
      
      print("bing 1/4")
      result <- cbind(Text.art$article, as.data.frame(get_sentiment(Text.art$text, method="bing")))
      #TIME
      print(difftime(T1, Sys.time()))
      
      print("nrc 2/4")
      result <- cbind(result, as.data.frame(get_sentiment(Text.art$text, method="nrc")))
      #TIME
      print(difftime(T1, Sys.time()))
      
      print("afinn 3/4")
      result <- cbind(result, as.data.frame(get_sentiment(Text.art$text, method="afinn")))
      #TIME
      print(difftime(T1, Sys.time()))
      
      print("syuzhet 4/4")
      result <- cbind(result, as.data.frame(get_sentiment(Text.art$text, method="syuzhet")))
      #TIME
      print(difftime(T1, Sys.time()))
      
      print("merge")
      #sentimentr <- sentiment(Text.art$text)
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
    }
  }
  
#Index1_1
  {
    Index1_1 <- function(Text.art,sentiment.used,TARGET_WORDS.count,quatrigrams_filtered) 
    {  
      #TIME
      T1<-Sys.time()
      #Base
      print(sentiment.used)
      #1)
      Working_Table<-TARGET_WORDS.count%>%
        top_n(3)
      colnames(Working_Table)[2]<-"tword"
      
      #2)
      get_sentiment(sentiment.used)
      
      #ISNOT TEST : List of trigram with ISNOT or negation
      {
        IsNot<-quatrigrams_filtered%>%
          mutate(nTrigram = 1:n())%>%
          unnest_tokens(word, trigram)%>%
          filter(word%in% NegationWords)%>%
          count(nTrigram)
        colnames(IsNot)[2]<-"TestIsNot"
        
        IsNot.end<-quatrigrams_filtered.end%>%
          mutate(nTrigram = 1:n())%>%
          unnest_tokens(word, trigram)%>%
          filter(word%in% NegationWords)%>%
          count(nTrigram)
        colnames(IsNot.end)[2]<-"TestIsNot"
      }
      
      #Sentiment material
      
      sentim<-quatrigrams_filtered%>%
        mutate(nTrigram = 1:n())%>%
        left_join(IsNot)%>%
        mutate(sentiment=get_sentiment(trigram, method=sentiment.used))%>%
        group_by(nTrigram,time,word1)%>%
        summarise(score=sum(sentiment*ifelse(is.na(TestIsNot),1,-1)))
      
      sentim.end<-quatrigrams_filtered.end%>%
        mutate(nTrigram = 1:n())%>%
        left_join(IsNot.end)%>%
        mutate(sentiment=get_sentiment(trigram, method=sentiment.used))%>%
        group_by(nTrigram,time,word4)%>%
        summarise(score=sum(sentiment*ifelse(is.na(TestIsNot),1,-1)))
      
      colnames(sentim)[3]<-"tword"
      colnames(sentim.end)[3]<-"tword"
      
      sentim.around <- full_join(sentim, sentim.end)
      rm(sentim,sentim.end)
      
      sentim.around<-sentim.around%>%
        group_by(time,tword)%>%
        summarise(score=sum(score))
      
      Working_Table<-Working_Table%>%
        left_join(sentim.around)
      
      #3)Index formula
      Index<-Working_Table%>%
        group_by(time)%>%
        summarise(index.value=mean(score,na.rm=TRUE))#Not a weighted average because the weight is already comprise in the score
      
      Result<-Text.art%>%
        left_join(Index)%>%
        select(article,index.value)
      
      colnames(Result)[2]<-paste(c("Indexe1_1",sentiment.used),collapse="_")
      #TIME
      print(difftime(T1, Sys.time()))
      
      return(Result)
    }
  }
}
