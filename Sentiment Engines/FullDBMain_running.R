#MAIN RUNNING

#TIME
T1<-Sys.time()
library(dplyr)
#Main Source
source("Sentiment Engines/Full_DB_Index_common.R")
source("Sentiment Engines/Function Used.R")
source("Sentiment Engines/List_TargetWords.R")
#TIME
print(paste("Main sources and functions : ",difftime(T1, Sys.time())))

#RUN Index Common--------------------------------------------------------------------------------------------------------------
  #--------Time for the Index Common Start
  T2<-Sys.time()
  #Final Table creation
  Final.table<- Text.art%>%
    dplyr::select(time,article)
  #Sources dummys
  Final.table <- cbind(Final.table,createDummyFeatures(Text.art$source, cols = "name-dummy"))
  #Number of words
  Final.table<-Final.table%>%
    dplyr::left_join(Index_0_nbwords(Text.art))
  #Maturity/Countarticle last 4 hours/Last 24h
  Final.table<-Final.table%>%
    left_join(Index_0_maturity(Text.art,Time.art))%>%
    left_join(Index_0_countart(Text.art,Time.art,hour=4))%>%
    left_join(Index_0_countart(Text.art,Time.art,hour=24))
  #FIRST ANALYSIS
  #Main sentiment analysis
  Final.table<-Final.table%>%
    left_join(Index_0(Text.art,NegationWords))
  #Indexes 0 search words
  Final.table<-Final.table%>%
    left_join(Index0_SearchWord(Text.art,Text.word,SEARCH_WORD))
  #FIRST ANALYSIS VARIABLE of VARIABLE
  Final.table<-Final.table%>%
    left_join(Index_daily(Final.table,c("total_bing","total_nrc","total_afinn","total_syuzhet")))
  
  #--------Time for the Index Common end
  print(paste("Main sources and functions : ",difftime(T2, Sys.time())))
  
    
#RUN INDEX SOURCE Country--------------------------------------------------------------------------------------------------------------
  #--------Time for the Indexes source 1 Start
  T2<-Sys.time()
    TARGET_WORDS<-TARGET_COUNTRY
    target_name<-"Country"
    source("Sentiment Engines/Index Source.R")
    #Time for the Indexes source
    print(paste("Indexes sources 1==>data frame creation : ",difftime(T2, Sys.time())))
    
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
  #INDEX1_3
  Final.table<-Final.table%>%
    left_join(Index1_3(Sentiment.list,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,ImpactWords,target_name) )
  
  #--------Time for the Indexes source 1 End
  print(paste("Time for Indexes Source 1 : ",difftime(T2, Sys.time())))
  
  
#RUN INDEX SOURCE Influencer--------------------------------------------------------------------------------------------------------------
  TARGET_WORDS<-TARGET_INFLUENCER
  target_name<-"Influencer"
  source("Sentiment Engines/Index Source.R")
  #INDEX1_0
  Final.table<-Final.table%>%
    left_join(Index1_0(TARGET_WORDS.article, Text.art,target_name))

  
#RUN INDEX SOURCE Bitcoin related--------------------------------------------------------------------------------------------------------------
  TARGET_WORDS<-TARGET_BITCOIN
  target_name<-"BitcoinRelated"
  source("Sentiment Engines/Index Source.R")
  
  #INDEX1_0
  Final.table<-Final.table%>%
    left_join(Index1_0(TARGET_WORDS.article, Text.art,target_name))
  #INDEX1_1
  Final.table<-Final.table%>%
    left_join(Index1_1(Text.art,Sentiment.list[1],TARGET_WORDS.count,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,target_name))%>%
    left_join(Index1_1(Text.art,Sentiment.list[2],TARGET_WORDS.count,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,target_name))%>%
    left_join(Index1_1(Text.art,Sentiment.list[3],TARGET_WORDS.count,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,target_name))%>%
    left_join(Index1_1(Text.art,Sentiment.list[4],TARGET_WORDS.count,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,target_name))
  
#RUN INDEX SOURCE Institutions--------------------------------------------------------------------------------------------------------------
  TARGET_WORDS<-TARGET_INSTITUTION
  target_name<-"Institutions"
  source("Sentiment Engines/Index Source.R")
  
  #INDEX1_0
  Final.table<-Final.table%>%
    left_join(Index1_0(TARGET_WORDS.article, Text.art,target_name))
  #INDEX1_3
  Final.table<-Final.table%>%
    left_join(Index1_3(Sentiment.list,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,approbation.word,target_name) )
  
  
#RUN INDEX SOURCE Market related--------------------------------------------------------------------------------------------------------------
  TARGET_WORDS<-TARGET_MKTINDICATOR
  target_name<-"Market"
  source("Sentiment Engines/Index Source.R")
  
  #INDEX1_0
  Final.table<-Final.table%>%
    left_join(Index1_0(TARGET_WORDS.article, Text.art,target_name))
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
  #INDEX1_3
  Final.table<-Final.table%>%
    left_join(Index1_3(Sentiment.list,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,Marketdirection.word,target_name) )
  

# FINAL TIME--------------------------------------------------------------------------------------------------------------
  print(paste("TOTAL TIME PROGRAM : ",difftime(T1, Sys.time())))
  
text.art2<-Text.art%>%
  dplyr::select(article,text)

Merge_Table<-Final.table%>%
  left_join(Time.art2)%>%
  left_join(text.art2)


Merge_Table = Merge_Table[!duplicated(Merge_Table$text),]
   z <- gzfile("Full_DB_FINAL.csv.gz")
    write.csv(Merge_Table, z)

