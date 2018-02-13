#INDEX 1_1 


#PROCESS & RESULT
#These indicators will focus on the highlighted countries of the days
#1)Take the top 3 most talked countries every days
#2)Look the sentiment related to these country 
#a)--Take the table quatrigrams and give a score using the sentiment analysis dictionnary
#b)--take the bigram or quatrigram to understand the reason give a index for reason
#Result: a sentiment score of the top 3 most used countries name every day + An index factor giving the reason of the mention


#Index1_1 <- function(sentiment.used,Source,TARGET_WORDS.count,quatrigrams_filtered) 
#Base
sentiment.used<-"afinn"
TARGET_WORDS.count
quatrigrams_filtered
quatrigrams_filtered.end
NegationWords


#1)
Working_Table<-TARGET_WORDS.count%>%
  top_n(3)
colnames(Working_Table)[2]<-"tword"

#2)
get_sentiments(sentiment.used)

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
  left_join(source.ratio)%>%
  mutate(nTrigram = 1:n())%>%
  left_join(IsNot)%>%
  unnest_tokens(word, trigram)%>%
  inner_join(get_sentiments(sentiment.used), by = "word")%>%
  group_by(nTrigram,time,word1)%>%
  summarise(score=sum(score*ratio*ifelse(is.na(TestIsNot),1,-1)))

sentim.end<-quatrigrams_filtered.end%>%
  left_join(source.ratio)%>%
  mutate(nTrigram = 1:n())%>%
  left_join(IsNot.end)%>%
  unnest_tokens(word, trigram)%>%
  inner_join(get_sentiments(sentiment.used), by = "word")%>%
  group_by(nTrigram,time,word4)%>%
  summarise(score=sum(score*ratio*ifelse(is.na(TestIsNot),1,-1)))

colnames(sentim)[3]<-"tword"
colnames(sentim.end)[3]<-"tword"

sentim.around <- full_join(sentim, sentim.end)
rm(sentim,sentim.end)

sentim.around<-sentim.around%>%
  group_by(time,tword)%>%
  summarise(score=sum(score))

Working_Table<-Working_Table%>%
  left_join(sentim.around)

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

#Index formula
Index<-Working_Table%>%
  group_by(time)%>%
  summarise(index.value=mean(score,na.rm=TRUE))#Not a weighted average because the weight is already comprise in the score


Index.article<-full_join(quatrigrams_filtered, quatrigrams_filtered.end)%>%
  grou


#return(Index.article)



