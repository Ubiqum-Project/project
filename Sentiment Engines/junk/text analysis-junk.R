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
  usePackage("tm")
  usePackage("widyr")
  usePackage("countrycode")
}

##### text analysis ##### 
#Data cleaned
#cleaned <- read_csv("~/Desktop/cleaned.csv")
source.name<-levels(factor(cleaned$name))

Text.title<-tibble(article=seq_along(cleaned$text),source=cleaned$name,text=cleaned$text,time=cleaned$time_downloaded_gmt)
#Text.title$source <- factor(Text.title$source, levels = rev(Text.title$source))

Title.word<-Text.title %>%
  unnest_tokens(word, text)

Title.word %>%
  count(word, sort = TRUE)

# pattern<-Title.word %>%
#   filter(source=="BBC") %>%
#   anti_join(stop_words) %>%
#   group_by(source) %>%
#   count(word, sort = TRUE) %>%
#   top_n(10)

#TOP 10 words per sources 
for(i in 1:length(source.name)){
  name<-source.name[i]
  print(Title.word %>%
          anti_join(stop_words) %>%
          group_by(source) %>%
          filter(source==name) %>%
          count(word, sort = TRUE) %>%
          filter(word != c("bitcoin")) %>%
          top_n(10) %>%
          ungroup() %>%
          mutate(source = factor(source),
                 text_order = nrow(.):1) %>%
          ggplot(aes(reorder(word, text_order), n, fill = source)) +
          geom_bar(stat = "identity") +
          facet_wrap(~ source, scales = "free_y") +
          labs(x = "NULL", y = "Frequency") +
          coord_flip() +
          theme(legend.position="none")
  )
  Sys.sleep(2)
}
