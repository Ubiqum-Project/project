library(readxl)
library(chron)
library(lubridate)

#-----> Importing and DF Setup
time = read_excel("Sentiment Engines/Date Time Cleaning/test formats.xlsx")
time$error = "Not Processed"
time$articleTime = as.character(unlist(data.frame(time$articleTime)))
time = as.data.frame(time)
time[,3]
time$datez = time[,3]
time$datez =as.character(time$datez)
now = time$timeNOWGMT[1]

#------------> DF Setup and ready----------


y = split(time, time$name)
bbc = as.data.frame(y["BBC"])
bcn = as.data.frame(y["Bitcoin News"])
cd = as.data.frame(y["China Daily"])
cnbc = as.data.frame(y["CNBC"])
cndk= as.data.frame(y["Coin Desk"])
fbbtc= as.data.frame(y["Facebook BTC Group"])
fbsrch= as.data.frame(y["Facebook Search"])
fr= as.data.frame(y["Free Republic"])
gf= as.data.frame(y["Google Finance"])
iet= as.data.frame(y["India Economic Times"])
rba= as.data.frame(y["Redit BTC All"])
reu= as.data.frame(y["Reuters"])
scmp= as.data.frame(y["South China Morning Post"])
tw= as.data.frame(y["Twitter"])
yn= as.data.frame(y["Yahoo News"])
you= as.data.frame(y["Youtube"])
zh= as.data.frame(y["Zero Hedge"])



#timeConv = -6

spanishMonthLibrary = c(
  
  "enero",
  "febrero",
  "marzo",
  "abril",
  "mayo",
  "junio",
  "julio",
  "agosto" ,
  "septiembre" ,
  "octubre",
  "noviembre" ,
  "diciembre"
)
germanMonthLibrary = c(
  
  "Januar",
  "Februar",
  "M?rz",
  "April",
  "Mai",
  "Juni",
  "Juli",
  "August" ,
  "September" ,
  "Oktober",
  "November" ,
  "Dezember"
)

googlePlusLibrary = c(
  "1m",
  "2m",
  "3m",
  "4m",
  "5m",
  "6m",
  "7m",
  "8m",
  "9m",
  "10m",
  "11m",
  "12m",
  "13m",
  "14m",
  "15m",
  "16m",
  "17m",
  "18m",
  "19m",
  "20m",
  "21m",
  "22m",
  "23m",
  "24m",
  "25m",
  "26m",
  "27m",
  "28m",
  "29m",
  "30m",
  "31m",
  "32m",
  "33m",
  "34m",
  "35m",
  "36m",
  "37m",
  "38m",
  "39m",
  "40m",
  "41m",
  "42m",
  "43m",
  "44m",
  "45m",
  "46m",
  "47m",
  "48m",
  "49m",
  "50m",
  "51m",
  "52m",
  "53m",
  "54m",
  "55m",
  "56m",
  "57m",
  "58m",
  "59m",
  "60m",
  "1h",
  "2h",
  "3h",
  "4h",
  "5h",
  "6h",
  "7h",
  "8h",
  "9h",
  "10h",
  "11h",
  "12h",
  "13h",
  "14h",
  "15h",
  "16h",
  "17h",
  "18h",
  "19h",
  "20h",
  "21h",
  "22h",
  "23h",
  "24h"
)

englishMonthLibrary = c(
  
  "January",
  "February",
  "March",
  "April",
  "May",
  "June",
  "July",
  "August" ,
  "September" ,
  "October",
  "November" ,
  "December"
)

nowLibrary = c(
  "now",
  "Now",
  "NOW",
  "a second ago",
  "A second ago",
  "a minute ago",
  "Moments",
  "a few seconds ago",
  "a few minutes ago",
  "just now",
  "Just Now",
  "SekundenKeine" 
)

yesterdayLibrary=c(
  "ayer a las",
  "Ayer a las",
  "yesterday at",
  "Yesterday at",
  "Gestern" 
  
)

dateLibrary <- c("Aug",
                 "AUG",
                 "august",
                 "August",
                 "AUGUST",
                 "sep",
                 "Sep",
                 "SEP",
                 "sept",
                 "Sept",
                 "SEPT",
                 "september",
                 "September",
                 "SEPTEMBER",
                 "oct",
                 "Oct",
                 "OCT",
                 "october",
                 "October",
                 "OCTOBER",
                 "nov",
                 "Nov",
                 "NOV",
                 "November",
                 "NOVEMBER",
                 "dec",
                 "DEC",
                 "december",
                 "December",
                 "DECEMBER",
                 "/2017",
                 "/2018",
                 "/2016",
                 "/2015")

googleFinanceLibrary = c(
  " - "
)

hourLibrary <- c("hr",
                 "Hr",
                 "HR",
                 "hrs",
                 "Hrs",
                 "HRS",
                 "hour",
                 "Hour",
                 "HOUR",
                 "hours",
                 "Hours",
                 "HOURS",
                 "hora",
                 "horas",
                 "Hora",
                 "HORA",
                 "Horas",
                 "HORAS",
                 " - 8 hours ago",
                 "Stunde"
                 
)

minuteLibrary <- c("min",
                   "Min",
                   "MIN",
                   "mins",
                   "Mins",
                   "MINS",
                   "minute",
                   "Minute",
                   "MINUTE",
                   "minutes",
                   "Minutes",
                   "MINUTES",
                   "Minuten"
)

secondLibrary <- c("sec",
                   "Sec",
                   "SEC",
                   "secs",
                   "Secs",
                   "SECS",
                   "second",
                   "Second",
                   "SECOND",
                   "seconds",
                   "Seconds",
                   "SECONDS"
)
dayLibrary <- c("day",
                "Day",
                "DAY",
                "days",
                "Days",
                "DAYS"
)


dateFunction <- function(i){
  testSplit = do.call('rbind',strsplit(as.character(time$datez[i]),',',fixed=TRUE))
  splitTime = testSplit[1]
  splitTime = substr(splitTime,1,nchar(splitTime)-6)
  splitTime = do.call('rbind',strsplit(as.character(splitTime),' ',fixed=TRUE))
  splitDate = dmy(testSplit[2])
  #if(splitTime[2]=="PM"){time$offset = 12*60*60}
  #time$offset = time$offset
  splitTime[1] = paste(splitTime[1], ":00", sep = "")
  x = paste(splitDate, splitTime[1], sep=" ")
  x = as.character(x)
  return(x)
}



googleFinanceFunction <- function(i){
  testSplit = do.call('rbind',strsplit(as.character(time$datez[19]),' - ',fixed=TRUE))
  x = testSplit[2]
  x =  as.character(mdy(x))
  x = as.character(paste(x, "12:00", sep = " "))
  return(x)
}

googleFinanceFunction(time)

dayFunction <- function(i){
  x = as.character(now - as.numeric(unlist(regmatches(time$datez[i],gregexpr('[0-9]+',time$datez[i]))))*60*60*24)
  return(x)
}

hourFunction <- function(i){
  x = as.character(now - as.numeric(unlist(regmatches(dirtyDate,gregexpr('[0-9]+',dirtyDate))))*60*60)
  return(x)
}

minuteFunction <- function(i){
  x = as.character( now - as.numeric(unlist(regmatches(time$datez[i],gregexpr('[0-9]+',time$datez[i]))))*60)
  return(x)
}

secondFunction <- function(i){
  x = as.character(now - as.numeric(unlist(regmatches(time$datez[i],gregexpr('[0-9]+',time$datez[i])))))
  return(x)
}

justNowFunction <- function(i){
  x = as.character(now)
  return(x)
}

yesterdayAtFunction <- function(i){
  testSplit = do.call('rbind',strsplit(as.character(time$datez[i]),   " "   ,fixed=TRUE))
  yesterdayTime = testSplit[length(testSplit)]
  yesterdayTime
  yesterdayDate = as.character(Sys.Date()-1)
  yesterdayDate
  x = paste(yesterdayDate, yesterdayTime, sep=" ")
  x = as.character(x)
  x
  return(x)
}

spanishMonthFunction <- function(i){
  
  ########### FOR FACEBOOK ###########################
  clean =gsub("a las", "",time$datez[i], ignore.case = T)
  clean =gsub("de", "",clean, ignore.case = T)
  clean =gsub("enero", "January 2018",clean, ignore.case = T)
  clean =gsub("febrero", "February 2018",clean, ignore.case = T)
  clean =gsub("marzo", "March 2018",clean, ignore.case = T)
  clean =gsub("abril", "April 2018",clean, ignore.case = T)
  clean =gsub("mayo", "May 2018",clean, ignore.case = T)
  clean =gsub("junio", "June 2018",clean, ignore.case = T)
  clean =gsub("julio", "July 2018",clean, ignore.case = T)
  clean =gsub("agosto", "August 2018",clean, ignore.case = T)
  clean =gsub("septiembre", "September 2018",clean, ignore.case = T)
  clean =gsub("octubre", "October 2017",clean, ignore.case = T)
  clean =gsub("noviembre", "November 2017",clean, ignore.case = T)
  clean =gsub("diciembre", "December 2017",clean, ignore.case = T)
  x = as.character(unlist(dmy_hm(clean)))
  x= paste(x,"","")
  x = unlist(x)
  x = as.character(x)
  
  
  return(x)
}

germanMonthFunction <- function(i){
  
  
  clean =gsub("um", "",time$datez[i], ignore.case = T)
  clean =gsub(".", "",clean, ignore.case = T)
  clean =gsub("Januar", "January 2018",clean, ignore.case = T)
  clean =gsub("Februar", "February 2018",clean, ignore.case = T)
  clean =gsub("Marz", "March 2018",clean, ignore.case = T)
  clean =gsub("April", "April 2018",clean, ignore.case = T)
  clean =gsub("Mai", "May 2018",clean, ignore.case = T)
  clean =gsub("Juni", "June 2018",clean, ignore.case = T)
  clean =gsub("Juli", "July 2018",clean, ignore.case = T)
  clean =gsub("August", "August 2018",clean, ignore.case = T)
  clean =gsub("September", "September 2018",clean, ignore.case = T)
  clean =gsub("Oktober", "October 2017",clean, ignore.case = T)
  clean =gsub("November", "November 2017",clean, ignore.case = T)
  clean =gsub("Dezember", "December 2017",clean, ignore.case = T)
  x = as.character(unlist(dmy_hm(clean)))
  x= paste(x,"","")
  x = unlist(x)
  x = as.character(x)
  
  
  return(x)
}

for (i in 1:length(tw$Twitter.datez))
{
  dirtyDate = tw$Twitter.datez[i]
  dirtyDate
  tw$cleaned[i]=  googlePlusFunction(dirtyDate)
  
  
}

googlePlusFunction <- function(dirtyDate){
  clean =gsub("1h", "60",dirtyDate, ignore.case = T)
  clean =gsub("2h", "120",clean, ignore.case = T)
  clean =gsub("3h", "180",clean, ignore.case = T)
  clean =gsub("4h", "240",clean, ignore.case = T)
  clean =gsub("5h", "300",clean, ignore.case = T)
  clean =gsub("6h", "360",clean, ignore.case = T)
  clean =gsub("7h", "420",clean, ignore.case = T)
  clean =gsub("8h", "480",clean, ignore.case = T)
  clean =gsub("9h", "540",clean, ignore.case = T)
  clean =gsub("10h", "600",clean, ignore.case = T)
  clean =gsub("11h", "660",clean, ignore.case = T)
  clean =gsub("12h", "720",clean, ignore.case = T)
  clean =gsub("13h", "780",clean, ignore.case = T)
  clean =gsub("14h", "840",clean, ignore.case = T)
  clean =gsub("15h", "900",clean, ignore.case = T)
  clean =gsub("16h", "960",clean, ignore.case = T)
  clean =gsub("17h", "1020",clean, ignore.case = T)
  clean =gsub("18h", "1080",clean, ignore.case = T)
  clean =gsub("19h", "1140",clean, ignore.case = T)
  clean =gsub("20h", "1200",clean, ignore.case = T)
  clean =gsub("21h", "1260",clean, ignore.case = T)
  clean =gsub("22h", "1320",clean, ignore.case = T)
  clean =gsub("23h", "1380",clean, ignore.case = T)
  clean =gsub("24h", "1440",clean, ignore.case = T)
  clean =gsub("1m", "1",clean, ignore.case = T)
  clean =gsub("2m", "2",clean, ignore.case = T)
  clean =gsub("3m", "3",clean, ignore.case = T)
  clean =gsub("4m", "4",clean, ignore.case = T)
  clean =gsub("5m", "5",clean, ignore.case = T)
  clean =gsub("6m", "6",clean, ignore.case = T)
  clean =gsub("7m", "7",clean, ignore.case = T)
  clean =gsub("8m", "8",clean, ignore.case = T)
  clean =gsub("9m", "9",clean, ignore.case = T)
  clean =gsub("10m", "10",clean, ignore.case = T)
  clean =gsub("11m", "11",clean, ignore.case = T)
  clean =gsub("12m", "12",clean, ignore.case = T)
  clean =gsub("13m", "13",clean, ignore.case = T)
  clean =gsub("14m", "14",clean, ignore.case = T)
  clean =gsub("15m", "15",clean, ignore.case = T)
  clean =gsub("16m", "16",clean, ignore.case = T)
  clean =gsub("17m", "17",clean, ignore.case = T)
  clean =gsub("18m", "18",clean, ignore.case = T)
  clean =gsub("19m", "19",clean, ignore.case = T)
  clean =gsub("20m", "20",clean, ignore.case = T)
  clean =gsub("21m", "21",clean, ignore.case = T)
  clean =gsub("22m", "22",clean, ignore.case = T)
  clean =gsub("23m", "23",clean, ignore.case = T)
  clean =gsub("24m", "24",clean, ignore.case = T)
  clean =gsub("25m", "25",clean, ignore.case = T)
  clean =gsub("26m", "26",clean, ignore.case = T)
  clean =gsub("27m", "27",clean, ignore.case = T)
  clean =gsub("28m", "28",clean, ignore.case = T)
  clean =gsub("29m", "29",clean, ignore.case = T)
  clean =gsub("30m", "30",clean, ignore.case = T)
  clean =gsub("31m", "31",clean, ignore.case = T)
  clean =gsub("32m", "32",clean, ignore.case = T)
  clean =gsub("33m", "33",clean, ignore.case = T)
  clean =gsub("34m", "34",clean, ignore.case = T)
  clean =gsub("35m", "35",clean, ignore.case = T)
  clean =gsub("36m", "36",clean, ignore.case = T)
  clean =gsub("37m", "37",clean, ignore.case = T)
  clean =gsub("38m", "38",clean, ignore.case = T)
  clean =gsub("39m", "39",clean, ignore.case = T)
  clean =gsub("40m", "40",clean, ignore.case = T)
  clean =gsub("41m", "41",clean, ignore.case = T)
  clean =gsub("42m", "42",clean, ignore.case = T)
  clean =gsub("43m", "43",clean, ignore.case = T)
  clean =gsub("44m", "44",clean, ignore.case = T)
  clean =gsub("45m", "45",clean, ignore.case = T)
  clean =gsub("46m", "46",clean, ignore.case = T)
  clean =gsub("47m", "47",clean, ignore.case = T)
  clean =gsub("48m", "48",clean, ignore.case = T)
  clean =gsub("49m", "49",clean, ignore.case = T)
  clean =gsub("50m", "50",clean, ignore.case = T)
  clean =gsub("51m", "51",clean, ignore.case = T)
  clean =gsub("52m", "52",clean, ignore.case = T)
  clean =gsub("53m", "53",clean, ignore.case = T)
  clean =gsub("54m", "54",clean, ignore.case = T)
  clean =gsub("55m", "55",clean, ignore.case = T)
  clean =gsub("56m", "56",clean, ignore.case = T)
  clean =gsub("57m", "57",clean, ignore.case = T)
  clean =gsub("58m", "58",clean, ignore.case = T)
  clean =gsub("59m", "59",clean, ignore.case = T)
  clean =gsub("60m", "60",clean, ignore.case = T)
  clean = as.numeric(clean)
  x = as.character(now-clean*60)
  x
  return(x)
}

str(time$datez)


#--------------->  BBC Function  1 Dec 2017 	NewsBusiness   

bbcFunction<- function(i){
  {
  }
  
  bbcFunction2<- function(i){
    {if(grepl("\\d", bbc$datez[i]))
    {    bbc$error[i] = "Success"  }
      else {bbc$error[i] = "Missing Date..Defaulted to Download Date/Time" }}
  }
  #-----------------> End BBC Function -----------------------------#############
  
  #--------------->  Bitcoin News Function Formats: 11/29/2017  7:23:28 PM-------------------
  bcnFunction<- function(i){
    {if(grepl("\\d", time$datez[i]))
    {
      for (i in 1:length(time$datez))
      {
        x= replace(time$datez[i],grep(paste(hourLibrary,collapse="|"),time$datez[i]),hourFunction(i))
      }
      
      
      
    }
      
      else {time$datez[i] = as.character(time_downloaded) }}
  }
  
  bcnFunction2<- function(i){
    {if(grepl("\\d", time$datez[i]))
    {    time$error[i] = "Success"  }
      else {time$error[i] = "Missing Date..Defaulted to Download Date/Time" }}
  }
  
  
  #-----------------> End Bitcoin News Function -----------------------------#############
  
  ############-------RESET------------------
  time = as.data.frame(time)
  time[,3]
  time$datez = time[,3]
  time$datez =as.character(time$datez)
  now =mdy_hms(time$timeNOWGMT[1])
  time_downloaded = now
  ############-------RESET------------------
  
  
  #BBC Cleaner
  for (i in 1:length(bbc$datez))
  {
    
    try(bbc$cleaned[i]=dmy(bbc$datez[i]))
    # grepl("\\d", bbc$datez[2])
    # if(grepl("\\d", bbc$datez[i]))
    # {
    #  bbc$cleaned[i] = as.character(as.POSIXct(as.character(dmy(bbc$datez[i]) ))+(time_downloaded - as.POSIXct(as.Date(time_downloaded))))
    #   
    # }
    # 
    # else {bbc$cleaned[i] = as.character(time_downloaded) }}
    
  }
  
  #Bitcoin news Cleaner---> bcnFunction
  for (i in 1:length(time$datez))
  {
    #time$error[i]=  replace(time$datez[i],grep(paste("Bitcoin News",collapse="|"),time$name[i]),bcnFunction2(i))# Not working
    time$cleaned[i]= replace(time$datez[i],grep(paste("Bitcoin News",collapse="|"),time$name[i]),bcnFunction(i))# Not working
    
    
  }
  
  
  
  
  for (i in 1:length(time$datez))
  {
    time$cleaned[i]= replace(time$datez[i],grep(paste(googleFinanceLibrary,collapse="|"),time$datez[i]),googleFinanceFunction(i))# Not working
    
  }
  
  for (i in 1:length(time$datez))
  {
    time$cleaned[i]= replace(time$datez[i],grep(paste(spanishMonthLibrary,collapse="|"),time$datez[i]),spanishMonthFunction(i))# Not working
    
  }
  
  for (i in 1:length(time$datez))
  {
    time$cleaned[i]= replace(time$datez[i],grep(paste(spanishMonthLibrary,collapse="|"),time$datez[i]),germanMonthFunction(i))# Not working
    
  }
  
  for (i in 1:length(time$datez))
  {
    time$cleaned[i]= replace(time$datez[i],grep(paste(yesterdayLibrary,collapse="|"),time$datez[i]),yesterdayAtFunction(i))# Not working
  }
  
  for (i in 1:length(time$datez))
  {
    time$cleaned[i]= replace(time$datez[i],grep(paste(nowLibrary,collapse="|"),time$datez[i]),justNowFunction(i))
  }
  
  for (i in 1:length(time$datez))
  {
    time$cleaned[i]= replace(time$datez[i],grep(paste(hourLibrary,collapse="|"),time$datez[i]),hourFunction(i))
  }
  
  
  str(time$datez)
  
  for (i in 1:length(time$datez))
  {
    time$cleaned[i]= replace(time$datez[i],grep(paste(minuteLibrary,collapse="|"),time$datez[i]),minuteFunction(i))
  }
  
  str(time$datez)
  
  for (i in 1:length(time$datez))
  {
    time$cleaned[i]= replace(time$datez[i],grep(paste(secondLibrary,collapse="|"),time$datez[i]),secondFunction(i)) #not working
  }
  
  str(time$datez)
  
  for (i in 1:length(time$datez))
  {
    time$cleaned[i]= replace(time$datez[i],grep(paste(dateLibrary,collapse="|"),time$datez[i]),dateFunction(i))
  }
  
  str(time$datez)
  
  for (i in 1:length(time$datez))
  {
    time$cleaned[i]= replace(time$datez[i],grep(paste(dayLibrary,collapse="|"),time$datez[i]),dayFunction(i))# Not working
  }
  
  
  
  
  
  str(time$datez)
  
  
  time$fixedTime = as.POSIXct(time$datez)
  