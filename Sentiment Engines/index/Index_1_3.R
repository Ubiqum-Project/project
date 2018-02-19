#INDEX1_3

#Mention of country with a impact word
ImpactWords
quatrigrams_filtered
quatrigrams_filtered.end

Sentiment.list
sentiment.used<- Sentiment.list[1]

Working_Table1<-data.frame(ImpactWords,afinn=get_sentiment(ImpactWords, method=Sentiment.list[1]),
                           bing=get_sentiment(ImpactWords, method=Sentiment.list[2]),
                           syuzhet=get_sentiment(ImpactWords, method=Sentiment.list[3]),
                           nrc=get_sentiment(ImpactWords, method=Sentiment.list[4]))


Negative_Impact<-(Working_Table1[,2:5]<0)*1
Negative_Impact<-tbl_df(Negative_Impact)
Negative_Impact<-Negative_Impact%>%
  mutate(negative=afinn+bing+syuzhet+nrc)%>%
  mutate(negative=(negative>=2))%>%
  select(negative)

Positive_Impact<-(Working_Table1[,2:5]>0)*1
Positive_Impact<-tbl_df(Positive_Impact)
Positive_Impact<-Positive_Impact%>%
  mutate(positive=afinn+bing+syuzhet+nrc)%>%
  mutate(positive=(positive>=1))%>%
  select(positive)


Working_Table1.pos<-data.frame(ImpactWords,positive=Positive_Impact,negative=Negative_Impact)%>%
  filter(positive==TRUE)%>%
  select(ImpactWords)
Working_Table1.neg<-data.frame(ImpactWords,positive=Positive_Impact,negative=Negative_Impact)%>%
  filter(negative==TRUE)%>%
  select(ImpactWords)

Result<-quatrigrams_filtered%>%
  count(time)%>%
  mutate(Positive=0)%>%
  mutate(Negative=0)%>%
  select(time,Positive,Negative)


for(i in 1:length(Working_Table1.pos$ImpactWords)){
  Working_Table<-quatrigrams_filtered%>%
    filter(grepl(Working_Table1.pos$ImpactWords[i],trigram ))%>%
    group_by(time)%>%
    count(trigram)%>%
    summarise(Positivetmp=sum(n))

  Result<-Result%>%
    left_join(Working_Table)%>%
    mutate(Positive=Positive+ifelse(is.na(Positivetmp),0,Positivetmp))%>%
    select(time,Positive,Negative)
  
  #Analyze
  Positive_tigramtmp<-quatrigrams_filtered%>%
    filter(grepl(Working_Table1.pos$ImpactWords[i],trigram ))%>%
    select(article,trigram)
  Positive_tigram<- Positive_tigramtmp%>%
    
}

for(i in 1:length(Working_Table1.neg$ImpactWords)){
  Working_Table<-quatrigrams_filtered%>%
    filter(grepl(Working_Table1.neg$ImpactWords[i],trigram ))%>%
    group_by(time)%>%
    count(trigram)%>%
    summarise(Negativetmp=sum(n))
  
  Result<-Result%>%
    left_join(Working_Table)%>%
    mutate(Negative=Negative+ifelse(is.na(Negativetmp),0,Negativetmp))%>%
    select(time,Positive,Negative)
}

Result<- Result%>%
  mutate(RatioPosNeg=ifelse(Positive== 0,0,Positive/Negative))%>%
  select(time,RatioPosNeg)
  
