#Index_daily <- function(Final.table, variable)
variable<-c("total_bing","total_nrc","total_afinn","total_syuzhet")

  working.table <-   Final.table %>% 
    select(time, variable) %>%
    group_by(time) %>%
    summarise_all(.funs = c(mean="mean"))
  
  result <-  working.table
  colnames(result)[2:length(result)]
  
  for(i in 2:length(result))
    colnames(result)[i]<-paste(c(colnames(result)[i],"d"),collapse="_")
 
  # return(result)
  result

