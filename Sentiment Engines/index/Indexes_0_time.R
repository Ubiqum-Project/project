#Indexes_0_Time
#Indexes based on precise time series

#1) Maturity of the article
Time.art

Working_Table<-Time.art%>%
  mutate(maturity=as.POSIXlt(Sys.time())-Time.art$time_Posixt)%>%
  select(article,maturity)

result<-Text.art%>%
  left_join(Working_Table)%>%
  select(article,maturity)

#2) Nb of news the last 24hours

Time.art
hour=4

Working_Table<-Time.art%>%
  mutate(maturity=difftime(Sys.time(), Time.art$time_Posixt,units = "hours"))%>%
  select(article,maturity)%>%
  arrange(maturity)%>%
  mutate(article_TOrder = 1:n())%>%
  mutate(maturity=round(maturity,0))%>%
  mutate(lastday=maturity+hour)
  
Working_Table2<-Working_Table%>%
  select(maturity,article_TOrder)%>%
  group_by(maturity)%>%
  summarise(article_n=min(article_TOrder))
  
colnames(Working_Table2)[1]<-colnames(Working_Table)[4]

Working_Table<-Working_Table%>%
  left_join(Working_Table2)%>%
  mutate(article_n=rev(na.locf(rev(article_n),na.rm=FALSE)))%>%
  mutate(count_art=article_n-article_TOrder)%>%
  select(article,count_art)

result<-Text.art%>%
  left_join(Working_Table)%>%
  select(article,count_art)

summary(result)

