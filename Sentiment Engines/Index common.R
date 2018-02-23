#INDEX COMMON#INDEX COMON DATA BASE
#Library
{
  usePackage <- function(p) 
  {
    if (!is.element(p, installed.packages()[,1]))
      install.packages(p, dep = TRUE)
    require(p, character.only = TRUE)
  }
  
  usePackage("ggplot2")
  usePackage("dplyr")
  usePackage("readr")
  usePackage("tidytext")
  usePackage("tidyverse")    # data manipulation & plotting
  usePackage("stringr")        # text cleaning and regular expressions
  usePackage("tidytext")
  usePackage("tm")
  usePackage("widyr")
  usePackage("countrycode")
  usePackage("scales")
  usePackage("igraph")
  usePackage("ggraph")
  usePackage("syuzhet")
  usePackage("SentimentAnalysis")
  usePackage("sentimentr")
  usePackage("mlr")
  usePackage("zoo")
  
}

#DATA ---------------------------------------------------------------------------------
#SOURCE
{
  #cleaned <- read_csv("~/Desktop/Bitcoin project/Project bitcoin/cleaned.csv")
  cleaned <- read_csv(gzfile("cleaned_with_dates.csv.gz"))
}
#Other information
{
  #DUMMIES for sources
    #Sources list
    source.name<-levels(factor(cleaned$name))
    #Ratio Table 
    source.ratio<-data.frame(source=source.name)
    source.ratio$ratio<-1

  #SENTIMENT INDICATOR
  Sentiment.list<-c("afinn","bing","syuzhet","nrc")
  #NEGATIVE WORDS
  NegationWords<-c("isnt","cannot","cant","wont","wasnt","ends","doesnt","not")
  #SEARCH WORDS
  SEARCH_WORD<-c("bubble", "tether", "hack", "crisis", "record", "fork", "whale", "ban", "bankruptcy","legal")
  
 
  
}
#INDEX TABLE CREATION-------------------------------------------------------------------
{
  #BASE DATA FRAME : TEXT+TIME+ARTICLE+SOURCE
  {
    Text.art<-tibble(article=seq_along(cleaned$text),source=cleaned$name,text=cleaned$text,time=cleaned$cleaned)
    Text.art$source <- factor(Text.art$source)
    Text.art$time <- as.Date(Text.art$time)

    #Time article Table
    
    Time.art<-tibble(article=seq_along(cleaned$text),time_Posixt=cleaned$cleaned)
    Time.art$time_Posixt<-as.POSIXlt(Time.art$time_Posixt)
    
    #Time article Table
    Time.art2<-tibble(article=seq_along(cleaned$text),cleaned=cleaned$cleaned)
  }
  #Remove cleaned for memory
  rm(cleaned)
  # ---ONE WORD--- TOKENS +++++++++++++++++++++++++++++++
  {
    #ALL Word used list 
    Text.word<-Text.art %>%
      #anti_join(stop_words)%>%
      unnest_tokens(word, text)
    
    #Daily count each word (possible to change the time frame)
    Text.word.Daily.count<-Text.word%>%
      group_by(time)%>%
      count(word, sort = TRUE)
    Text.word.Daily.count<-Text.word.Daily.count[order(as.Date(Text.word.Daily.count$time)),]
    
    #Total words every day
    Text.word.Daily.Total<-Text.word%>%
      group_by(time)%>%
      summarize(nwords = n())
    Text.word.Daily.Total<-Text.word.Daily.Total[order(as.Date(Text.word.Daily.Total$time)),]
    
  }
  # ---BIGRAM--- TOKENS +++++++++++++++++++++++++++++++
  {
    #ALL Bigram used list 
    Text.bigram <- Text.art %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2)
    
    #ALL Bigram used list word separed
    Text.bigram.separated <- Text.bigram %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    
    #Daily count each bigram (possible to change the time frame)
    Text.bigram.Daily.count<-Text.bigram%>%
      group_by(time)%>%
      count(bigram, sort = TRUE)
    Text.bigram.Daily.count<-Text.bigram.Daily.count[order(as.Date(Text.bigram.Daily.count$time)),]
    
    #Total bigram every day
    Text.bigram.Daily.Total<-Text.bigram%>%
      group_by(time)%>%
      summarize(nbigram = n())
    Text.bigram.Daily.Total<-Text.bigram.Daily.Total[order(as.Date(Text.bigram.Daily.Total$time)),]
  }
  # ---TRIGRAM--- TOKENS +++++++++++++++++++++++++++++++
  {
    #ALL Bigram used list 
    Text.trigram <- Text.art %>%
      unnest_tokens(trigram, text, token = "ngrams", n = 3)
    
    #ALL trigram used list word separated
    Text.trigram.separated <- Text.trigram %>%
      separate(trigram, c("word1", "word2","word3"), sep = " ")
    
    #Word 1 with bigram
    Text.trigram.bigram<- Text.trigram.separated %>%
      unite(bigram,word2, word3, sep = " ")
    
    #Word 3 with bigram
    Text.trigram.bigram.end<- Text.trigram.separated %>%
      unite(bigram,word1, word2, sep = " ")
    
    #Daily count each trigram (possible to change the time frame)
    Text.trigram.Daily.count<-Text.trigram%>%
      group_by(time)%>%
      count(trigram, sort = TRUE)
    Text.trigram.Daily.count<-Text.trigram.Daily.count[order(as.Date(Text.trigram.Daily.count$time)),]
    
    #Total trigram every day
    Text.trigram.Daily.Total<-Text.trigram%>%
      group_by(time)%>%
      summarize(ntrigram = n())
    Text.trigram.Daily.Total<-Text.trigram.Daily.Total[order(as.Date(Text.trigram.Daily.Total$time)),]
  }
  # ---QUADRIGRAM--- TOKENS +++++++++++++++++++++++++++++++ For pure sentiment analysis
  {
    #ALL quatrigram used list 
    Text.quatrigram <- Text.art %>%
      unnest_tokens(quatrigram, text, token = "ngrams", n = 4)
    
    #ALL quatrigram used list word separated
    Text.quatrigram.separated <- Text.quatrigram %>%
      separate(quatrigram, c("word1", "word2","word3","word4"), sep = " ")
    
    #Word 1 with trigram
    Text.quatrigram.trigram<- Text.quatrigram.separated %>%
      unite(trigram,word2, word3,word4, sep = " ")
    
    #Word 3 with bigram
    Text.quatrigram.trigram.end<- Text.quatrigram.separated %>%
      unite(trigram,word1, word2,word3, sep = " ")
    
    #Daily count each quatrigram (possible to change the time frame)
    Text.quatrigram.Daily.count<-Text.quatrigram%>%
      group_by(time)%>%
      count(quatrigram, sort = TRUE)
    Text.quatrigram.Daily.count<-Text.quatrigram.Daily.count[order(as.Date(Text.quatrigram.Daily.count$time)),]
    
    #Total quatrigram every day
    Text.quatrigram.Daily.Total<-Text.quatrigram%>%
      group_by(time)%>%
      summarize(nquatrigram = n())
    Text.quatrigram.Daily.Total<-Text.quatrigram.Daily.Total[order(as.Date(Text.quatrigram.Daily.Total$time)),]
  }
}




