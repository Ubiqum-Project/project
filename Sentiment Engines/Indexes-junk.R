
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

#GRAPH==============================================================
ggplot()+geom_line(data=TARGET_WORDS.count,aes(x=time,y=n*100+10000,color=word))+
  geom_line(data=Price,aes(x = Date,y=Close))+scale_x_date(breaks = date_breaks("3 days"),labels=date_format("%d-%m"))
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

#GRAPH==============================================================
#BIGRAM
#Target word +word 2
{
  
  Used.table <- bigrams_filtered %>% 
    count(word1, word2, sort = TRUE)
  
  bigram_graph <- Used.table%>%
    filter(n > 5) %>%
    graph_from_data_frame()
  
  #GRAPH1
  set.seed(2017)
  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)
  
  #GRAPH2
  set.seed(2016)
  
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
  
  #GRAPH3 Research over time a specific bigram
  Used.table<-bigrams_united.count[which(bigrams_united.count$bigram==" bubble"),]
  Used.table<-bigrams_united.count[which(grepl("burst",bigrams_united.count$bigram )),]
  Used.table
  
  ggplot(Used.table,aes(time, ratio,color=bigram)) +
    geom_line(size = 1.3) +
    labs(x = NULL, y = "Word frequency")
  
  ggplot(Used.table,aes(time, ratio*100*60000+10000)) +
    geom_line(size = 1.3) +
    labs(x = NULL, y = "Word frequency")+
    geom_line(data=Price,aes(x = Date,y=Close))+scale_x_date(breaks = date_breaks("3 days"),labels=date_format("%d-%m"))
  
  
  #GRAPH 4
  ggplot(data=bigrams_united.count,aes(x=time,y=ratio*100*60000+10000))+geom_point()+geom_text(aes(label=ifelse(n>2,bigram,'')),hjust=0, vjust=0)+
    geom_line(data=Price,aes(x = Date,y=Close))+scale_x_date(breaks = date_breaks("3 days"),labels=date_format("%d-%m"))
  
}
#word1 + target word
{
  
  Used.table <- bigrams_filtered.end %>% 
    count(word1, word2, sort = TRUE)
  
  bigram_graph <- Used.table%>%
    filter(n > 5) %>%
    graph_from_data_frame()
  
  #GRAPH1
  set.seed(2017)
  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)
  
  #GRAPH2
  set.seed(2016)
  
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
  
  #GRAPH3 Research over time a specific bigram
  Used.table<-bigrams_united.count.end[which(grepl("massive",bigrams_united.count.end$bigram )),]
  Used.table
  
  ggplot(Used.table,aes(time, ratio,color=bigram)) +
    geom_line(size = 1.3) +
    labs(x = NULL, y = "Word frequency")
  
  ggplot(Used.table,aes(time, appearance*100*60000+10000)) +
    geom_line(size = 1.3) +
    labs(x = NULL, y = "Word frequency")+
    geom_line(data=Price,aes(x = Date,y=Close))+scale_x_date(breaks = date_breaks("3 days"),labels=date_format("%d-%m"))
  
  #GRAPH4
  ggplot(data=bigrams_united.count.end,aes(x=time,y=ratio*100*60000+10000))+geom_point()+geom_text(aes(label=ifelse(n>2,bigram,'')),hjust=0, vjust=0)+
    geom_line(data=Price,aes(x = Date,y=Close))+scale_x_date(breaks = date_breaks("3 days"),labels=date_format("%d-%m"))
  
}

#TRIGRAM
#WORD 1 + BIGRAM
{
  #Word 1 +bigram
  Used.table <- trigrams_filtered %>% 
    count(word1, bigram, sort = TRUE)
  
  trigram_graph <- Used.table%>%
    filter(n > 5) %>%
    graph_from_data_frame()
  
  #GRAPH1
  set.seed(2017)
  
  ggraph(trigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)
  
  #GRAPH2
  set.seed(2016)
  
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  ggraph(trigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
  
  #GRAPH 3
  Used.table<-trigrams_united.count[which(trigrams_united.count$trigram=="dangerous speculative bubble"),]
  Used.table<-trigrams_united.count[which(startsWith(trigrams_united.count$trigram, "dangerous")),]
  Used.table<-trigrams_united.count[which(grepl("up",trigrams_united.count$trigram )),]
  Used.table
  
  ggplot(Used.table,aes(time, ratio,color=trigram)) +
    geom_line(size = 1.3) +
    labs(x = NULL, y = "Word frequency")
  
  Used.table<-Used.table%>%
    group_by(time)%>%
    summarize(appearance = sum(ratio, na.rm = TRUE))
  
  ggplot(Used.table,aes(time, ratio*100*60000+10000)) +
    geom_line(size = 1.3) +
    labs(x = NULL, y = "Word frequency")+
    geom_line(data=Price,aes(x = Date,y=Close))+scale_x_date(breaks = date_breaks("3 days"),labels=date_format("%d-%m"))
  
  
  #GRAPH4
  ggplot(data=trigrams_united.count,aes(x=time,y=ratio*100*60000+10000))+geom_point()+geom_text(aes(label=ifelse(n>2,trigram,'')),hjust=0, vjust=0)+
    geom_line(data=Price,aes(x = Date,y=Close))+scale_x_date(breaks = date_breaks("3 days"),labels=date_format("%d-%m"))
}
#BIGRAM + WORD 3
{
  
  #bigram +word 3
  Used.table <- trigrams_filtered.end %>% 
    count(word3, bigram, sort = TRUE)
  
  #!!!!!!!! CHOOSE ONE THE USED TABLE ABOVE
  trigram_graph <- Used.table%>%
    filter(n > 5) %>%
    graph_from_data_frame()
  
  #GRAPH1
  set.seed(2017)
  
  ggraph(trigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)
  
  #GRAPH2
  set.seed(2016)
  
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  ggraph(trigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
  
  #GRAPH 3
  Used.table<-trigrams_united.count.end[which(trigrams_united.count.end$trigram=="dangerous speculative bubble"),]
  Used.table<-trigrams_united.count.end[which(startsWith(trigrams_united.count.end$trigram, "dangerous")),]
  Used.table<-trigrams_united.count.end[which(grepl("sell",trigrams_united.count.end$trigram )),]
  Used.table
  
  ggplot(Used.table,aes(time, ratio,color=trigram)) +
    geom_line(size = 1.3) +
    labs(x = NULL, y = "Word frequency")
  
  Used.table<-Used.table%>%
    group_by(time)%>%
    summarize(appearance = sum(ratio, na.rm = TRUE))
  
  ggplot(Used.table,aes(time, appearance*100*60000+10000)) +
    geom_line(size = 1.3) +
    labs(x = NULL, y = "Word frequency")+
    geom_line(data=Price,aes(x = Date,y=Close))+scale_x_date(breaks = date_breaks("3 days"),labels=date_format("%d-%m"))
  
  
  #GRAPH4
  ggplot(data=trigrams_united.count.end,aes(x=time,y=ratio*100*60000+10000))+geom_point()+geom_text(aes(label=ifelse(n>2,trigram,'')),hjust=0, vjust=0)+
    geom_line(data=Price,aes(x = Date,y=Close))+scale_x_date(breaks = date_breaks("3 days"),labels=date_format("%d-%m"))
  
}
#===================================================================
