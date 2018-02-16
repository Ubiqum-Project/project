#MAIN RUNNING


#TIME
T1<-Sys.time()

#RUN Index Common
  Final.table<-Index_0(Text.art) 
  #Sources dummys
  Final.table <- cbind(Final.table,createDummyFeatures(Text.art$source, cols = "name-dummy"))
  #Maturity/Countarticle last 4 hours/Last 24h
  Final.table<-Final.table%>%
    left_join(Index_0_maturity(Text.art,Time.art))%>%
    left_join(Index_0_countart(Text.art,Time.art,hour=4))%>%
    left_join(Index_0_countart(Text.art,Time.art,hour=24))
 
#RUN INDEX SOURCE 1
  target_name<-"Country"
  #INDEX1_0
  Final.table<-Final.table%>%
    left_join(Index1_0(TARGET_WORDS.article, Text.art,target_name))%>%
    left_join(Index1_0_TOP(TARGET_WORDS.count, Text.word,target_name))
  
  #INDEX1_1
  Final.table<-Final.table%>%
    left_join(Index1_1(Text.art,Sentiment.list[1],TARGET_WORDS.count,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,target_name))%>%
    left_join(Index1_1(Text.art,Sentiment.list[2],TARGET_WORDS.count,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,target_name))%>%
    left_join(Index1_1(Text.art,Sentiment.list[3],TARGET_WORDS.count,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,target_name))%>%
    left_join(Index1_1(Text.art,Sentiment.list[4],TARGET_WORDS.count,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,target_name))
  #INDEX1_2
  Final.table<-Final.table%>%
    left_join(Index1_2(Text.art,Sentiment.list[1],Text.quatrigram,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,target_name))%>%
    left_join(Index1_2(Text.art,Sentiment.list[2],Text.quatrigram,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,target_name))%>%
    left_join(Index1_2(Text.art,Sentiment.list[3],Text.quatrigram,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,target_name))%>%
    left_join(Index1_2(Text.art,Sentiment.list[4],Text.quatrigram,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,target_name))

#RUN INDEX SOURCE 2
  target_name<-"Influencer"

#RUN DUMMY VARIABLES INDICATING SOURCE



#TIME
print(difftime(T1, Sys.time()))


#Function used+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{
#IndexMain

#Index0____________________________________________________________________________________________
  
  Index_0 <- function(Text.art,Text.art) 
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
  
  
  Index_0_maturity <- function(Text.art,Time.art)
  {
    Working_Table<-Time.art%>%
      mutate(maturity=as.POSIXlt(Sys.time())-Time.art$time_Posixt)%>%
      select(article,maturity)
    
    result<-Text.art%>%
      left_join(Working_Table)%>%
      select(article,maturity)
    
    return(result)
  }
  
  Index_0_countart <- function(Text.art,Time.art,hour=24)
  {
    Working_Table<-Time.art%>%
      mutate(maturity=difftime(Sys.time(), Time.art$time_Posixt,units = "hours"))%>%
      select(article,maturity)%>%
      arrange(maturity)%>%
      mutate(article_TOrder = 1:n())%>%
      mutate(maturity=round(maturity,0))%>%
      mutate(lastday=maturity+hour)
    
    Working_Table2<-Working_Table%>%
      select(maturity,article_TOrder)%>%
      group_by(maturity)%>%
      summarise(article_n=min(article_TOrder))
    
    colnames(Working_Table2)[1]<-colnames(Working_Table)[4]
    
    Working_Table<-Working_Table%>%
      left_join(Working_Table2)%>%
      mutate(article_n=rev(na.locf(rev(article_n),na.rm=FALSE)))%>%
      mutate(count_art=article_n-article_TOrder)%>%
      select(article,count_art)
    
    result<-Text.art%>%
      left_join(Working_Table)%>%
      select(article,count_art)
    
    colnames(result)[2]<-paste(c("count_art",hour),collapse="_")
    return(result)
  }
  
#Index_1_0____________________________________________________________________________________________
  
  Index1_0 <- function (TARGET_WORDS.article, Text.art,target_name)
   {
    
  working.table <- TARGET_WORDS.article  %>%
    mutate(Test = 1)
  
  result <- Text.art   %>%
    left_join (working.table)   %>%
    select(article,Test)
  
  colnames(result)[2]<-paste(c(target_name,"Mention"),collapse="_")
  return(result)
  }
#Index_1_0TOP___________________________________________________________________________________________
  
  Index1_0_TOP <- function (TARGET_WORDS.count, Text.word,target_name)
  {
    Working_Table<-TARGET_WORDS.count%>%
      top_n(3)
    
    Working_Table2<-Text.word %>%
      filter(word%in% TARGET_WORDS)
    
    result <- Working_Table%>%
      left_join (Working_Table2)%>%
      group_by(article)%>%
      count(article)%>%
      mutate(Test=1)%>%
      select(article,Test)
    
    colnames(result)[2]<-paste(c(target_name,"TOP_Mention"),collapse="_")
    return(result)
    
  }
    
#Index1_1____________________________________________________________________________________________
  Index1_1 <- function(Text.art,sentiment.used,TARGET_WORDS.count,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,target_name) 
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
        summarise(score1=sum(sentiment*ifelse(is.na(TestIsNot),1,-1)))%>%
        group_by(time,word1)%>%
        summarise(score1=sum(score1))
      
      sentim.end<-quatrigrams_filtered.end%>%
        mutate(nTrigram = 1:n())%>%
        left_join(IsNot.end)%>%
        mutate(sentiment=get_sentiment(trigram, method=sentiment.used))%>%
        group_by(nTrigram,time,word4)%>%
        summarise(score2=sum(sentiment*ifelse(is.na(TestIsNot),1,-1)))%>%
        group_by(time,word4)%>%
        summarise(score2=sum(score2))
      
      colnames(sentim)[2]<-"tword"
      colnames(sentim.end)[2]<-"tword"
      
      sentim.around <- full_join(sentim, sentim.end)%>%
        mutate(score=ifelse(is.na(score1),0,score1)+ifelse(is.na(score2),0,score2))%>%
        select(time,tword,score)
      
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
      
      colnames(Result)[2]<-paste(c(target_name,"Indexe1_1",sentiment.used),collapse="_")
      #TIME
      print(difftime(T1, Sys.time()))
      
      return(Result)
    }
  
#Index1_2____________________________________________________________________________________________
  
  Index1_2 <- function(Text.art,sentiment.used,Text.quatrigram,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,target_name) 
    {
      #TIME
      T1<-Sys.time()
      
      Working_Table<-Text.quatrigram
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
        group_by(article)%>%
        summarise(score1=sum(sentiment*ifelse(is.na(TestIsNot),1,-1)))
      
      sentim.end<-quatrigrams_filtered.end%>%
        mutate(nTrigram = 1:n())%>%
        left_join(IsNot.end)%>%
        mutate(sentiment=get_sentiment(trigram, method=sentiment.used))%>%
        group_by(article)%>%
        summarise(score2=sum(sentiment*ifelse(is.na(TestIsNot),1,-1)))
      
      sentim.around <- full_join(sentim, sentim.end)%>%
        mutate(score=ifelse(is.na(score1),0,score1)+ifelse(is.na(score2),0,score2))%>%
        select(article,score)
      
      rm(sentim,sentim.end)
      #3)Index formula
      Index<-sentim.around
      
      Result<-Text.art%>%
        left_join(Index)%>%
        select(article,score)
      
      colnames(Result)[2]<-paste(c(target_name,"Indexe1_2",sentiment.used),collapse="_")
      
      return(Result)
      #TIME
      print(difftime(T1, Sys.time()))
    }
  
}
