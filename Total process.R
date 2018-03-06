## COOL RUNNINGS (https://en.wikipedia.org/wiki/Cool_Runnings)

#TIME
TT<-Sys.time()
TX<-Sys.time()

#Cleaning and streamlining
print(paste("Cleaning and streamlining"))
source("Cleaning/Date Time Cleaning/StreamliningDateTime.R")
print(paste("TOTAL TIME PROGRAM : ", round(difftime(TT, Sys.time(),units = "mins"),2),"minutes"))
print(paste("Cleaning and streamlining : ",round(difftime(TX, Sys.time(),units = "mins"),2),"minutes"))
TX<-Sys.time()

#Sentiment
print(paste("Sentiment"))
source("Sentiment Engines/Main Running.R")
print(paste("TOTAL TIME PROGRAM : ", round(difftime(TT, Sys.time(),units = "mins"),2),"minutes"))
print(paste("Sentiment : ",round(difftime(TX, Sys.time(),units = "mins"),2),"minutes"))
TX<-Sys.time()

#Secondary predictors
print(paste("Secondary predictors"))
source("Other Predictors/Stock Puller.R")
print(paste("TOTAL TIME PROGRAM : ", round(difftime(TT, Sys.time(),units = "mins"),2),"minutes"))
print(paste("Secondary predictors : ",round(difftime(TX, Sys.time(),units = "mins"),2),"minutes"))
TX<-Sys.time()

#Data Normalization
print(paste("Data Normalization"))
source("Modeling/dataNormalizer.R")
print(paste("TOTAL TIME PROGRAM : ", round(difftime(TT, Sys.time(),units = "mins"),2),"minutes"))
print(paste("Data Normalization : ",round(difftime(TX, Sys.time(),units = "mins"),2),"minutes"))
TX<-Sys.time()

#Models
print(paste("Models"))
source("XXXXXXXXXXXXX.R")
print(paste("TOTAL TIME PROGRAM : ", round(difftime(TT, Sys.time(),units = "mins"),2),"minutes"))
print(paste("Models : ",round(difftime(TX, Sys.time(),units = "mins"),2),"minutes"))
TX<-Sys.time()

#TOTAL TIME
print(paste("TOTAL TIME PROGRAM : ", round(difftime(TT, Sys.time(),units = "mins"),2),"minutes"))
