#Function 0
TARGET_WORD<-c("bubble", "tether", "hack", "crisis", "record", "fork")


TARGET_WORD.article<-Text.word %>%
  filter(word%in% TARGET_WORD)  %>%
  group_by(time) %>%
  count(word)

working.table<-TARGET_WORD.article%>%
  filter(word%in% TARGET_WORD[1])%>%
  select(time,n)

colnames(working.table)[2] <- TARGET_WORD[1]


for(i in 2:length(TARGET_WORD)){
  working.table_tmp<-TARGET_WORD.article%>%
    filter(word%in% TARGET_WORD[i])%>%
  select(time,n)
  
  colnames(working.table_tmp)[2] <- TARGET_WORD[i]
  
  working.table<-working.table%>%
    left_join(working.table_tmp)

}
