#INDEX 0_1

#Number of appearance of theses words per day
TARGET_WORDS.article<-Text.word %>%
  filter(word%in% TARGET_WORDS)  %>%
    count(article) %>%
      select(article)

working.table <- TARGET_WORDS.article  %>%
  mutate(countryT = 1)

result <- Text.art   %>%
  left_join (working.table)   %>%
    select(article,countryT)
