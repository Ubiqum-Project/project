#INDEX1_3

#Mention of country with a impact word
ImpactWords
quatrigrams_filtered
quatrigrams_filtered.end

Sentiment.list
sentiment.used<- Sentiment.list[3]

get_sentiment(ImpactWords, method=sentiment.used)
ImpactWords
Working_Table<-quatrigrams_filtered%>%
  filter(grepl(ImpactWords[1],trigram ))%>%
  group_by(article)%>%
  count(trigram)
  
