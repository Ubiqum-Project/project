#Function Used

#Function used+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{
  #Index_calculation
  
  Index_daily <- function(Final.table, variable)
  {
    working.table <-   Final.table %>% 
      select(time, variable) %>%
      group_by(time) %>%
      summarise_all(.funs = c(mean="mean"))
    
    result <-  working.table
    return(result)
  }
  
  #IndexMain
  
  #Index0____________________________________________________________________________________________
  
  Index_0 <- function(Text.art,NegationWords) 
  {
    
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
  }
  
  Index_0_maturity <- function(Text.art,Time.art)
  {
    Working_Table<-Time.art%>%
      mutate(maturity=as.POSIXlt(Sys.time())-Time.art$time_Posixt)%>%
      select(article,maturity)
    
    result<-Text.art%>%
      left_join(Working_Table)%>%
      select(article,maturity,time)
    
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
  
  Index0_SearchWord<-function(Text.art,Text.word,SEARCH_WORD)
  {
    SEARCH_WORD.article<-Text.word %>%
      filter(word%in% SEARCH_WORD)  %>%
      group_by(time) %>%
      count(word)
    
    working.table<-SEARCH_WORD.article%>%
      filter(word%in% SEARCH_WORD[1])%>%
      select(time,n)
    
    colnames(working.table)[2] <- SEARCH_WORD[1]
    
    
    for(i in 2:length(SEARCH_WORD)){
      working.table_tmp<-SEARCH_WORD.article%>%
        filter(word%in% SEARCH_WORD[i])%>%
        select(time,n)
      
      colnames(working.table_tmp)[2] <- SEARCH_WORD[i]
      
      working.table<-working.table%>%
        left_join(working.table_tmp)
    }
    Result<-Text.art%>%
      left_join(working.table)%>%
      select(article,bubble,tether,hack,crisis,record,fork)
    return(Result)
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