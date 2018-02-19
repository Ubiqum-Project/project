#FUNCTION PRICE AND WORD
DeltaCol <- function(Table,Attribute) 
{

  Delta.attribute<-diff(Attribute)/Attribute[-length(Attribute)]
  Delta<-c(1:nrow(Table))
  Table<-data.frame(Table,Delta)
  Table$Delta<-NA
  Table$Delta<-c(0,Delta.attribute)
  
  return(Table)
}
DeltaCol.sub <- function(Table,Attribute) 
{
  
  Delta.attribute<-diff(Attribute)-Attribute[-length(Attribute)]
  Delta<-c(1:nrow(Table))
  Table<-data.frame(Table,Delta)
  Table$Delta<-NA
  Table$Delta<-c(0,Delta.attribute)
  
  return(Table)
}

