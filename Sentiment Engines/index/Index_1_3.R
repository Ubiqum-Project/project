#INDEX1_3

#Mention of country with a impact word
ImpactWords
quatrigrams_filtered
quatrigrams_filtered.end
Sentiment.list
NegationWords
target_name

#Index1_3 <- function(Sentiment.list,quatrigrams_filtered,quatrigrams_filtered.end,NegationWords,ImpactWords,target_name) 
  

#Sentiment over all IMPACT words
Sentiment.Impact<-data.frame(ImpactWords,afinn=get_sentiment(ImpactWords, method=Sentiment.list[1]),
                           bing=get_sentiment(ImpactWords, method=Sentiment.list[2]),
                           syuzhet=get_sentiment(ImpactWords, method=Sentiment.list[3]),
                           nrc=get_sentiment(ImpactWords, method=Sentiment.list[4]))

#Sentiment over all IMPACT words / numerical / select only the one who are identified 2 times at least
#Negative  
  Negative_Impact<-(Sentiment.Impact[,2:5]<0)*1
  Negative_Impact<-tbl_df(Negative_Impact)
  Negative_Impact<-Negative_Impact%>%
    mutate(negative=afinn+bing+syuzhet+nrc)%>%
    mutate(negative=(negative>=2))%>%
    select(negative)
#Positive  
  Positive_Impact<-(Sentiment.Impact[,2:5]>0)*1
  Positive_Impact<-tbl_df(Positive_Impact)
  Positive_Impact<-Positive_Impact%>%
    mutate(positive=afinn+bing+syuzhet+nrc)%>%
    mutate(positive=(positive>=2))%>%
    select(positive)
  
  #Create the list of positive and negative words list
Positive_Impact.list<-data.frame(ImpactWords,positive=Positive_Impact,negative=Negative_Impact)%>%
  filter(positive==TRUE)%>%
  select(ImpactWords)%>%
  filter(ImpactWords!="govern")#Eleminate impact words


Negative_Impact.list<-data.frame(ImpactWords,positive=Positive_Impact,negative=Negative_Impact)%>%
  filter(negative==TRUE)%>%
  select(ImpactWords)%>%
  filter(ImpactWords!="bank")#Eleminate impact words

#Merge Quadrigram and Quadrigram.end
quatrigram_merge.impact<-quatrigrams_filtered%>%
  full_join(quatrigrams_filtered.end)%>%
  select(time,trigram)

#Initialization of the table
Result<-quatrigram_merge.impact%>%
  count(time)%>%
  mutate(Positive=0)%>%
  mutate(Negative=0)%>%
  select(time,Positive,Negative)


#Create the list of positive and negative words list
#Positive

for(i in 1:length(Positive_Impact.list$ImpactWords)){
  #Positive  => Positive
  Working_Table<-quatrigram_merge.impact%>%
    filter(grepl(Positive_Impact.list$ImpactWords[i],trigram ))%>%
    group_by(time)%>%
    count(trigram)%>%
    summarise(Positivetmp=sum(n))

  Result<-Result%>%
    left_join(Working_Table)%>%
    mutate(Positive=Positive+ifelse(is.na(Positivetmp),0,Positivetmp))%>%
    select(time,Positive,Negative)
  
  #Positive + negation => Negative
for(j in 1:length(NegationWords)){
    print(j)
    Working_Table<-quatrigram_merge.impact%>%
      filter(grepl(Positive_Impact.list$ImpactWords[i],trigram ))%>%
      filter(grepl(paste("\\b",NegationWords[j],"\\b"),trigram )==TRUE)%>%
      group_by(time)%>%
      count(trigram)%>%
      summarise(Negativetmp=sum(n))
    
    Result<-Result%>%
      left_join(Working_Table)%>%
      mutate(Negative=Negative+ifelse(is.na(Negativetmp),0,Negativetmp))%>%
      select(time,Positive,Negative)
}
}
#negative

for(i in 1:length(Negative_Impact.list$ImpactWords)){
  #Negative  => Negative
  Working_Table<-quatrigram_merge.impact%>%
    filter(grepl(Negative_Impact.list$ImpactWords[i],trigram ))%>%
    group_by(time)%>%
    count(trigram)%>%
    summarise(Negativetmp=sum(n))
  
  Result<-Result%>%
    left_join(Working_Table)%>%
    mutate(Negative=Negative+ifelse(is.na(Negativetmp),0,Negativetmp))%>%
    select(time,Positive,Negative)
  
  #Negative + negation => Positive
  for(j in 1:length(NegationWords)){
    print(j)
    Working_Table<-quatrigram_merge.impact%>%
      filter(grepl(Negative_Impact.list$ImpactWords[i],trigram ))%>%
      filter(grepl(paste("\\b",NegationWords[j],"\\b"),trigram )==TRUE)%>%
      group_by(time)%>%
      count(trigram)%>%
      summarise(Positivetmp=sum(n))
    
    Result<-Result%>%
      left_join(Working_Table)%>%
      mutate(Positive=Positive+ifelse(is.na(Positivetmp),0,Positivetmp))%>%
      select(time,Positive,Negative)
    
  }
}

colnames(Result)[2]<-paste(c(target_name,"Positive_Impact1_3.d"),collapse="_")
colnames(Result)[3]<-paste(c(target_name,"Negative_Impact1_3.d"),collapse="_")


#return(result)

#ANALYSIS
for(i in 1:length(Negative_Impact.list$ImpactWords)){
  #Negative  => Negative
  Working_Table<-quatrigram_merge.impact%>%
    filter(grepl(Negative_Impact.list$ImpactWords[i],trigram ))

  x<-x%>%
    full_join(Working_Table)
}
for(i in 1:length(Positive_Impact.list$ImpactWords)){
  #Negative  => Negative
  Working_Table<-quatrigram_merge.impact%>%
    filter(grepl(Positive_Impact.list$ImpactWords[i],trigram ))
  
  y<-y%>%
    full_join(Working_Table)
}


