#INDEX 1_2


#PROCESS & RESULT
#The specific sentiment when We talk about countries in the article
Text.quatrigram
quatrigrams_filtered
quatrigrams_filtered.end
NegationWords

#Index1_2 <- function(sentiment.used,TARGET_WORDS.count,quatrigrams_filtered) 
#Base

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


#explanation checking
{
  Used.table <- trigrams_filtered %>% 
    count(word1, bigram,time)
  colnames(Used.table)[1]<-"tword"
  colnames(Used.table)[4]<-"nword"
  
  explaination<-Working_Table%>%
    left_join(Used.table)%>%
    arrange(desc(time))%>%
    arrange(desc(nword))%>%
    arrange(desc(tword))
  explaination
  
  Used.table2 <- trigrams_filtered.end %>% 
    count(word3, bigram,time)
  colnames(Used.table2)[1]<-"tword"
  colnames(Used.table2)[4]<-"nword"
  
  explaination<-Working_Table%>%
    left_join(Used.table2)%>%
    arrange(desc(time))%>%
    arrange(desc(nword))%>%
    arrange(desc(tword))
  
  }

#3)Index formula
Index<-sentim.around

Result<-Text.art%>%
  left_join(Index)%>%
  select(article,score)

colnames(Result)[2]<-paste(c("Indexe1_1",sentiment.used),collapse="_")

#return(Result)



