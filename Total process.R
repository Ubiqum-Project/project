## COOL RUNNINGS (h.TTps://en.wikipedia.org/wiki/Cool_Runnings)

#TIME
.TT<-Sys.time()
.TX<-Sys.time()

#Cleaning and streamlining
print(paste("Cleaning and streamlining"))
source("Cleaning/Date Time Cleaning/StreamliningDateTime.R")
print(paste("TOTAL TIME PROGRAM : ", round(difftime(.TT, Sys.time(),units = "mins"),2),"minutes"))
print(paste("Cleaning and streamlining TIME : ",round(difftime(.TX, Sys.time(),units = "mins"),2),"minutes"))
.TX<-Sys.time()
rm(list=ls())

#Sentiment
print(paste("Sentiment"))
source("Sentiment Engines/Main_running.R")
print(paste("TOTAL TIME PROGRAM : ", round(difftime(.TT, Sys.time(),units = "mins"),2),"minutes"))
print(paste("Sentiment TIME : ",round(difftime(.TX, Sys.time(),units = "mins"),2),"minutes"))
.TX<-Sys.time()
rm(list=ls())

#Secondary predictors
print(paste("Secondary predictors"))
source("Other Predictors/Stock Puller.R")
print(paste("TOTAL TIME PROGRAM : ", round(difftime(.TT, Sys.time(),units = "mins"),2),"minutes"))
print(paste("Secondary predictors TIME : ",round(difftime(.TX, Sys.time(),units = "mins"),2),"minutes"))
.TX<-Sys.time()
rm(list=ls())

#Data Normalization
print(paste("Data Normalization"))
source("Modeling/dataNormalizer.R")
print(paste("TOTAL TIME PROGRAM : ", round(difftime(.TT, Sys.time(),units = "mins"),2),"minutes"))
print(paste("Data Normalization TIME : ",round(difftime(.TX, Sys.time(),units = "mins"),2),"minutes"))
.TX<-Sys.time()
rm(list=ls())

#Models
# print(paste("Models"))
# source("XXXXXXXXXXXXX.R")
# print(paste("TOTAL TIME PROGRAM : ", round(difftime(.TT, Sys.time(),units = "mins"),2),"minutes"))
# print(paste("Models TIME : ",round(difftime(.TX, Sys.time(),units = "mins"),2),"minutes"))
# .TX<-Sys.time()

#TOTAL TIME
print(paste("TOTAL TIME PROGRAM : ", round(difftime(.TT, Sys.time(),units = "mins"),2),"minutes"))


