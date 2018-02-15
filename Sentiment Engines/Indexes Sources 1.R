
#INDEX 1 Countries
#
TARGET_COUNTRY<-tolower(countrycode_data$country.name.en)
TARGET_COUNTRY<-c(TARGET_COUNTRY,"south-korean","vietnam","usa","unitedstates")
TARGET_WORDS<-TARGET_COUNTRY

#Number of appearance of theses words per day
TARGET_WORDS.count<-Text.word %>%
  anti_join(stop_words) %>%
  group_by(time)%>%
  count(word, sort = TRUE)%>%
  filter(word%in% TARGET_WORDS) 

TARGET_WORDS.article<-Text.word %>%
  filter(word%in% TARGET_WORDS)%>%
  count(article)%>%
  select(article)

#===================================================================
#GENERAL PREPROCESS
{
  #Find the related words
  #BIGRAM
  {
    #Target word + word2
    bigrams_filtered <- Text.bigram.separated %>%
      filter(word1 %in% TARGET_WORDS)#Word 1 or 2
    
    bigrams_united <- bigrams_filtered %>%
      unite(bigram, word1, word2, sep = " ")
    
    #word1 + target word
    bigrams_filtered.end <- Text.bigram.separated %>%
      filter(word2 %in% TARGET_WORDS)#Word 1 or 2
    
    bigrams_united.end <- bigrams_filtered.end %>%
      unite(bigram, word1, word2, sep = " ")
    
  }
  #TRIGRAM
  {
    #Word 1 +bigram
    trigrams_filtered <- Text.trigram.bigram%>%
      filter(word1 %in% TARGET_WORDS)
    
    trigrams_united <- trigrams_filtered %>%
      unite(trigram, word1, bigram, sep = " ")
    
    #bigram + word 3
    
    trigrams_filtered.end <- Text.trigram.bigram.end%>%
      filter(word3 %in% TARGET_WORDS)
    
    trigrams_united.end <- trigrams_filtered.end %>%
      unite(trigram, bigram,word3, sep = " ")
  }
  #QUATRIGRAM
  {
    #Word 1 +trigram
    quatrigrams_filtered <- Text.quatrigram.trigram%>%
      filter(word1 %in% TARGET_WORDS)
    
    quatrigrams_united <- quatrigrams_filtered %>%
      unite(quatrigram, word1, trigram, sep = " ")
    
    #trigram + word 3
    
    quatrigrams_filtered.end <- Text.quatrigram.trigram.end%>%
      filter(word4 %in% TARGET_WORDS)
    
    quatrigrams_united.end <- quatrigrams_filtered.end %>%
      unite(quatrigram, trigram,word4, sep = " ")
  }
}

#------------------------------------------------------------------------------------------------
#PREPROCESS: CREATE SPECIFIC TABLE NUMBER OF APPEARANCE
#No need for quatrigram
{
  #BIGRAM
  {
    # Proportion of bigram appearance in Daily basis
    #Target word + word2
    bigrams_united.count<-bigrams_united %>%
      group_by(time)%>%
      count(bigram, sort = TRUE)%>%
      left_join(Text.bigram.Daily.Total) %>%#Change proportion over all word or over sentiments
      mutate(ratio = n/nbigram) %>%
      ungroup()
    bigrams_united.count<-bigrams_united.count[order(as.Date(bigrams_united.count$time)),]
    bigrams_united.count
    #word1 + target word
    bigrams_united.count.end<-bigrams_united.end %>%
      group_by(time)%>%
      count(bigram, sort = TRUE)%>%
      left_join(Text.bigram.Daily.Total) %>%#Change proportion over all word or over sentiments
      mutate(ratio = n/nbigram) %>%
      ungroup()
    bigrams_united.count.end<-bigrams_united.count.end[order(as.Date(bigrams_united.count.end$time)),]
    bigrams_united.count.end
  }
  #TRIGRAM
  {
    #Word 1 +bigram
    # Proportion of trigram appearance in Daily basis
    trigrams_united.count<-trigrams_united %>%
      group_by(time)%>%
      count(trigram, sort = TRUE)%>%
      left_join(Text.trigram.Daily.Total) %>%#Change proportion over all word or over sentiments
      mutate(ratio = n/ntrigram) %>%
      ungroup()
    trigrams_united.count<-trigrams_united.count[order(as.Date(trigrams_united.count$time)),]
    trigrams_united.count
    #bigram +word 3
    # Proportion of trigram appearance in Daily basis
    trigrams_united.count.end<-trigrams_united.end %>%
      group_by(time)%>%
      count(trigram, sort = TRUE)%>%
      left_join(Text.trigram.Daily.Total) %>%#Change proportion over all word or over sentiments
      mutate(ratio = n/ntrigram) %>%
      ungroup()
    trigrams_united.count.end<-trigrams_united.count.end[order(as.Date(trigrams_united.count.end$time)),]
    trigrams_united.count.end
  }
}

#PREPROCESS: Analyze sentiment around the Target word
{
  
}
