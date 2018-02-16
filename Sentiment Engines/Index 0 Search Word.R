#Function 0

SEARCH_WORD.article<-Text.word %>%
  filter(word%in% SEARCH_WORD)  %>%
  group_by(time) %>%
  count(word) %>%

working.table<-SEARCH_WORD.article%>%
  filter(word%in% SEARCH_WORD[1])%>%
  select(time,n)

colnames(working.table)[2] <- SEARCH_WORD[1]


for(i in 2:length(SEARCH_WORD)){
  working.table_tmp<-SEARCH_WORD.article%>%
    filter(word%in% SEARCH_WORD[i])%>%
  select(time,n)
  
  colnames(working.table_tmp)[2] <- SEARCH_WORD[i]
  
  working.table<-working.table%>%
    left_join(working.table_tmp)

}
