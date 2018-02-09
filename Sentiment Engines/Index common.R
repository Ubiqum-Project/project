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
  usePackage("caret")
  usePackage("readr")
  usePackage("tidytext")
  usePackage("tidyverse")    # data manipulation & plotting
  usePackage("stringr")        # text cleaning and regular expressions
  usePackage("tidytext")
  usePackage("tm")
  usePackage("widyr")
  usePackage("countrycode")
  usePackage("wordnet")
  usePackage("scales")
  usePackage("igraph")
  usePackage("ggraph")  
  
  
}

#SOURCE

cleaned <- read_csv("~/Desktop/Bitcoin project/cleaned.csv")

source.name<-levels(factor(cleaned$name))

Text.art<-tibble(article=seq_along(cleaned$paragraph),source=cleaned$name,text=cleaned$paragraph,time=cleaned$time_downloaded_gmt)
Text.art$source <- factor(Text.art$source, levels = rev(source))

Text.word<-Text.art %>%
  unnest_tokens(word, text)

Text.word %>%
  count(word, sort = TRUE)

Text.word2<-Text.word
Text.word2$time<-as.Date(Text.word2$time)
name<-source.name[2]

Text.word2<-Text.word2%>%
  anti_join(stop_words) 


#BIGRAM

bigrams.text <- cleaned %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams.tilte <- cleaned %>%
  unnest_tokens(bigram, title, token = "ngrams", n = 2)
bigrams_separated <- bigrams.text %>%
  separate(bigram, c("word1", "word2"), sep = " ")
rm(bigrams.text)
rm(bigrams.tilte)

#TRIGRAM

trigrams.text <- cleaned %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3)
trigrams_separated <- trigrams.text %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ")

trigrams.tilte <- cleaned %>%
  unnest_tokens(bigram, title, token = "ngrams", n = 3)
trigrams_separated <- trigrams.title %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ")
rm(trigrams.text)
rm(trigrams.tilte)

