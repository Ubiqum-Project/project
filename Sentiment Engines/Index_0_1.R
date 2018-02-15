#INDEX 0_1

#Number of appearance of theses words per day
TARGET_WORDS.article<-Text.word %>%
filter(word%in% TARGET_WORDS)  %>%
count(article) %>%
select(article)
