## COOL RUNNINGS (https://en.wikipedia.org/wiki/Cool_Runnings)

#TIME
TT<-Sys.time()
TX<-Sys.time()

#Cleaning and streamlining
print(paste("Cleaning and streamlining"))
source("Cleaning/Date Time Cleaning/StreamliningDateTime.R")
print(paste("TOTAL TIME PROGRAM : ",difftime(TT, Sys.time(),units = "auto")))
print(paste("Cleaning and streamlining : ",difftime(TX, Sys.time())))
TX<-Sys.time()

#Sentiment
print(paste("Sentiment"))
source("Sentiment Engines/Main Running.R")
print(paste("TOTAL TIME PROGRAM : ",difftime(TT, Sys.time(),units = "auto")))
print(paste("Sentiment : ",difftime(TX, Sys.time())))
TX<-Sys.time()

#Secondary predictors
print(paste("Secondary predictors"))
source("Other Predictors/Stock Puller.R")
print(paste("TOTAL TIME PROGRAM : ",difftime(TT, Sys.time(),units = "auto")))
print(paste("Secondary predictors : ",difftime(TX, Sys.time())))
TX<-Sys.time()

#Data Normalization
print(paste("Data Normalization"))
source("Modeling/dataNormalizer.R")
print(paste("TOTAL TIME PROGRAM : ",difftime(TT, Sys.time(),units = "auto")))
print(paste("Data Normalization : ",difftime(TX, Sys.time())))
TX<-Sys.time()

#Models
print(paste("Models"))
source("XXXXXXXXXXXXX.R")
print(paste("TOTAL TIME PROGRAM : ",difftime(TT, Sys.time(),units = "auto")))
print(paste("Models : ",difftime(TX, Sys.time())))
TX<-Sys.time()

#TOTAL TIME
print(paste("TOTAL TIME PROGRAM : ",difftime(TT, Sys.time(),units = "auto")))
