#INDEX 1_1 


#PROCESS & RESULT
#These indicators will focus on the highlighted countries of the days
#1)Take the top 3 most talked countries every days
#2)Look the sentiment related to these country 
#a)--Take the table quatrigrams and give a score using the sentiment analysis dictionnary
#b)--take the bigram or quatrigram to understand the reason give a index for reason
#Result: a sentiment score of the top 3 most used countries name every day + An index factor giving the reason of the mention


#Index1_1 <- function(sentiment.used,TARGET_WORDS.count,quatrigrams_filtered) 
#Base
sentiment.used<-"bing"

Text.art
TARGET_WORDS.count
quatrigrams_filtered
quatrigrams_filtered.end
NegationWords


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
  mutate(score=score1+score2)%>%
  select(time,tword,score)
  
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

#3)Index formula
Working_Table
Index<-Working_Table%>%
  group_by(time)%>%
  summarise(index.value=mean(score,na.rm=TRUE))#Not a weighted average because the weight is already comprise in the score

# Index.article<-full_join(quatrigrams_filtered, quatrigrams_filtered.end)%>%
#   left_join(Index)%>%
#   count(article,index.value)%>%
#   select(article, index.value)

Result<-Text.art%>%
  left_join(Index)%>%
  select(article,index.value)

Result
colnames(Result)[2]<-paste(c("Indexe1_1",sentiment.used),collapse="_")

#return(Result)



