
library(readxl)
library(chron)
library(lubridate)
library(stringi)
library(readr)
library(zoo)
library(RPostgreSQL)

#####################################################################################################
####################################### Pull From Database ##########################################


# #Enter the values for you database connection
# dsn_database = "bitcoin"            # e.g. "compose"
# dsn_hostname = "165.227.167.6" # e.g.: "aws-us-east-1-portal.4.dblayer.com"
# dsn_port = "5432"                 # e.g. 11101 
# dsn_uid = "postgres"        # e.g. "admin"
# dsn_pwd = "840RanchoCircle!!"      # e.g. "xxx"
# 
# tryCatch({
#   drv <- dbDriver("PostgreSQL")
#   print("Connecting to database")
#   conn <- dbConnect(drv, 
#                     dbname = dsn_database,
#                     host = dsn_hostname, 
#                     port = dsn_port,
#                     user = dsn_uid, 
#                     password = dsn_pwd)
#   print("Connected!")
# },
# error=function(cond) {
#   print("Unable to connect to database.")
# })
# 
# 
# # 
# bitcoinDB <- dbGetQuery(conn, "SELECT * from bitcoin")   #NOT FOR USE  THIS DOWNLOADS THE ENTIRE DATABASE
# dbDisconnect(conn)
# cleaned = unique(setDT(bitcoinDB), by = c('title'), fromLast = FALSE)    #NOT FOR USE  THIS filters unique values only


#######################################  Or Pull From File  #########################################
#####################################################################################################


cleaned <- read.csv("bitcoinPull 2018-01-25")

#####################################################################################################
#####################################################################################################


# cleaned <- cleaned[ grep("year", cleaned$article_time, invert = TRUE) , ]
# cleaned <- cleaned[ grep("years", cleaned$article_time, invert = TRUE) , ] 
# cleaned <- cleaned[ grep("months", cleaned$article_time, invert = TRUE) , ]
Sys.setlocale("LC_ALL", "C")
cleaned$paragraph <- as.character(cleaned$paragraph)
cleaned$title <- as.character(cleaned$title)

cleaned$combination<-cleaned$title
{cleaned$combination[which(cleaned$title != cleaned$paragraph)]=
    paste(cleaned$title[which(cleaned$title != cleaned$paragraph)],cleaned$paragraph
          [which(cleaned$title != cleaned$paragraph)])}

docs <- Corpus(VectorSource(cleaned$combination))

#cleaned.x <- cleaned[1:100000,]
#cleaned.x$combination <- paste(cleaned.x$title, cleaned.x$paragraph)
#docs.x <- Corpus(VectorSource(cleaned.x$combination))

#convert various characters to spaces, split up the words in the text, remove urls etc.
toSpace.t <- content_transformer(function (x, pattern) gsub(pattern, "http://", x, fixed=TRUE))
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x, fixed=TRUE))
removeURL  <- function(x) gsub (" ?(f|ht)(tp)s?(://)(\\S*)[./](\\S*)", "", x)
docs <- tm_map(docs, toSpace.t, "pic.twitter")
docs <- tm_map(docs, removeURL)
docs <- tm_map(docs, toSpace, "/r/btc")
docs <- tm_map(docs, toSpace, "/r")
docs <- tm_map(docs, toSpace, "S9")
docs <- tm_map(docs, toSpace, "s9")
docs <- tm_map(docs, toSpace, "s5")
docs <- tm_map(docs, toSpace, "s7")
docs <- tm_map(docs, toSpace, "S5")
docs <- tm_map(docs, toSpace, "S7")
docs <- tm_map(docs, toSpace, ")")
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "\\")
docs <- tm_map(docs, toSpace, "(")
docs <- tm_map(docs, toSpace, "#")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "_")
docs <- tm_map(docs, toSpace, "\r")
docs <- tm_map(docs, toSpace, "\n")
docs <- tm_map(docs, function(x) iconv(enc2utf8(docs$content), sub = "byte"))

# united states correction
us <- content_transformer(function (x, pattern) gsub(pattern, "unitedstates", x, fixed=TRUE))
docs <- tm_map(docs, us, "U.S.")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers (not required?)
docs <- tm_map(docs, removeNumbers)
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("commentsharesavehidereport", "commentssharesavehidereportloading",
                                    "commentsharesavehidereportloading","commentssharesavehidereport",
                                    "submitted", "reddit", "redditcom", "hour ago", "hours ago", 
                                    "pictwittercom", "just", "now", "minutes", "ago", "bitcoinallbot1",
                                    "wsj", "pm","city","pic.twitter.com","http","https","wrap","live",
                                    "cointelegraph", "am","bloomberg","selfcryptocurrency","ireddit",
                                    "selfbitcoinmining","selfbitcointmarkets","discussiondaily",
                                    "selfbitcoinbayarea","bitcointallbotcommentsharesavehidereport",
                                    "dailycommentsharesavehidereport", "usethebitcoincom",
                                    "andix3commentsharesavehidereport", "selfbitcoinmarkets",
                                    "bitcoinallbotcommentsharesavehidereport", "newsbtc","read","cnbc","video",
                                    "cryptodailycouk","coindoocom","andixcommentsharesavehidereport",
                                    "selfbitcoinxt","lhicocommentsharesavehidereport","bitcoinallbot",
                                    "iimgurcom", "minute", "months", "month","day","days","10k", "10K", "8k",
                                    "9k","7k","8K","9K","7K")) 
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Text stemming
#docs <- tm_map(docs, stemDocument)
# other country corrections
s.korea <- content_transformer(function (x, pattern) gsub(pattern, "south-korea", x, fixed=TRUE))
docs <- tm_map(docs, s.korea, "south korea")
s.korean <- content_transformer(function (x, pattern) gsub(pattern, "south-korea", x, fixed=TRUE))
docs <- tm_map(docs, s.korean, "south korean")
n.korea <- content_transformer(function (x, pattern) gsub(pattern, "north-korea", x, fixed=TRUE))
docs <- tm_map(docs, n.korea, "north korea")
s.africa <- content_transformer(function (x, pattern) gsub(pattern, "south-africa", x, fixed=TRUE))
docs <- tm_map(docs, s.africa, "south africa")
s.sudan <- content_transformer(function (x, pattern) gsub(pattern, "south-sudan", x, fixed=TRUE))
docs <- tm_map(docs, s.sudan, "south sudan")
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

#put text back into cleaned df
text.df <- data.frame(text = sapply(docs, paste, collapse = " "), stringsAsFactors = FALSE)
text.df$text <- sapply(text.df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
cleaned <- as.data.frame(cbind(cleaned,text.df))

#removes duplicates
dupes <- which(duplicated(cleaned$text))
if (dupes != 0) cleaned <- cleaned[-dupes, ]

# cleaned.puerto <- cleaned[ grep("threat global", cleaned$text, invert = FALSE) , ]
# write_csv(cleaned.puerto, "puerto.csv")

####### further cleanup ####### 
#input values for missings
cleaned$price_gf_delta_btc <- na.replace(cleaned$price_gf_delta_btc, 0)
cleaned$title <- na.replace(cleaned$title, "x")
cleaned$paragraph <- na.replace(cleaned$paragraph, "x")
# cleaned$article_time <- na.replace(cleaned$article_time, "x")
# cleaned$time_downloaded_gmt[is.na(cleaned$time_downloaded_gmt)] <- cleaned$time_now_gmt[is.na(cleaned$time_downloaded_gmt)]

sapply(cleaned, function(x) sum(is.na(x)))
cleaned <- na.omit(cleaned)

#####################################################################################################
#########################  LIBRARY AND FUNCTION DEFINITIONS ########################################

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
  "Marz",
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
                 "hours ago",
                 "Stunde",
                 "Std"
                 
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

googlePlusFunction <- function(i){
  # time$article[1]
  # i=1  
  clean =gsub("60s", "60",time$article[i], ignore.case = T)
  clean =gsub("59s", "59",clean, ignore.case = T)
  clean =gsub("58s", "58",clean, ignore.case = T)
  clean =gsub("57s", "57",clean, ignore.case = T)
  clean =gsub("56s", "56",clean, ignore.case = T)
  clean =gsub("55s", "55",clean, ignore.case = T)
  clean =gsub("54s", "54",clean, ignore.case = T)
  clean =gsub("53s", "53",clean, ignore.case = T)
  clean =gsub("52s", "52",clean, ignore.case = T)
  clean =gsub("51s", "51",clean, ignore.case = T)
  clean =gsub("50s", "50",clean, ignore.case = T)
  clean =gsub("49s", "49",clean, ignore.case = T)
  clean =gsub("48s", "48",clean, ignore.case = T)
  clean =gsub("47s", "47",clean, ignore.case = T)
  clean =gsub("46s", "46",clean, ignore.case = T)
  clean =gsub("45s", "45",clean, ignore.case = T)
  clean =gsub("44s", "44",clean, ignore.case = T)
  clean =gsub("43s", "43",clean, ignore.case = T)
  clean =gsub("42s", "42",clean, ignore.case = T)
  clean =gsub("41s", "41",clean, ignore.case = T)
  clean =gsub("40s", "40",clean, ignore.case = T)
  clean =gsub("39s", "39",clean, ignore.case = T)
  clean =gsub("38s", "38",clean, ignore.case = T)
  clean =gsub("37s", "37",clean, ignore.case = T)
  clean =gsub("36s", "36",clean, ignore.case = T)
  clean =gsub("35s", "35",clean, ignore.case = T)
  clean =gsub("34s", "34",clean, ignore.case = T)
  clean =gsub("33s", "33",clean, ignore.case = T)
  clean =gsub("32s", "32",clean, ignore.case = T)
  clean =gsub("31s", "31",clean, ignore.case = T)
  clean =gsub("30s", "30",clean, ignore.case = T)
  clean =gsub("29s", "29",clean, ignore.case = T)
  clean =gsub("28s", "28",clean, ignore.case = T)
  clean =gsub("27s", "27",clean, ignore.case = T)
  clean =gsub("26s", "26",clean, ignore.case = T)
  clean =gsub("25s", "25",clean, ignore.case = T)
  clean =gsub("24s", "24",clean, ignore.case = T)
  clean =gsub("23s", "23",clean, ignore.case = T)
  clean =gsub("22s", "22",clean, ignore.case = T)
  clean =gsub("21s", "21",clean, ignore.case = T)
  clean =gsub("20s", "20",clean, ignore.case = T)
  clean =gsub("19s", "19",clean, ignore.case = T)
  clean =gsub("18s", "18",clean, ignore.case = T)
  clean =gsub("17s", "17",clean, ignore.case = T)
  clean =gsub("16s", "16",clean, ignore.case = T)
  clean =gsub("15s", "15",clean, ignore.case = T)
  clean =gsub("14s", "14",clean, ignore.case = T)
  clean =gsub("13s", "13",clean, ignore.case = T)
  clean =gsub("12s", "12",clean, ignore.case = T)
  clean =gsub("11s", "11",clean, ignore.case = T)
  clean =gsub("10s", "10",clean, ignore.case = T)
  clean =gsub("9s", "9",clean, ignore.case = T)
  clean =gsub("8s", "8",clean, ignore.case = T)
  clean =gsub("7s", "7",clean, ignore.case = T)
  clean =gsub("6s", "6",clean, ignore.case = T)
  clean =gsub("5s", "5",clean, ignore.case = T)
  clean =gsub("4s", "4",clean, ignore.case = T)
  clean =gsub("3s", "3",clean, ignore.case = T)
  clean =gsub("2s", "2",clean, ignore.case = T)
  clean =gsub("1s", "1",clean, ignore.case = T)
    clean =gsub("60m", "60",clean, ignore.case = T)
  clean =gsub("59m", "59",clean, ignore.case = T)
  clean =gsub("58m", "58",clean, ignore.case = T)
  clean =gsub("57m", "57",clean, ignore.case = T)
  clean =gsub("56m", "56",clean, ignore.case = T)
  clean =gsub("55m", "55",clean, ignore.case = T)
  clean =gsub("54m", "54",clean, ignore.case = T)
  clean =gsub("53m", "53",clean, ignore.case = T)
  clean =gsub("52m", "52",clean, ignore.case = T)
  clean =gsub("51m", "51",clean, ignore.case = T)
  clean =gsub("50m", "50",clean, ignore.case = T)
  clean =gsub("49m", "49",clean, ignore.case = T)
  clean =gsub("48m", "48",clean, ignore.case = T)
  clean =gsub("47m", "47",clean, ignore.case = T)
  clean =gsub("46m", "46",clean, ignore.case = T)
  clean =gsub("45m", "45",clean, ignore.case = T)
  clean =gsub("44m", "44",clean, ignore.case = T)
  clean =gsub("43m", "43",clean, ignore.case = T)
  clean =gsub("42m", "42",clean, ignore.case = T)
  clean =gsub("41m", "41",clean, ignore.case = T)
  clean =gsub("40m", "40",clean, ignore.case = T)
  clean =gsub("39m", "39",clean, ignore.case = T)
  clean =gsub("38m", "38",clean, ignore.case = T)
  clean =gsub("37m", "37",clean, ignore.case = T)
  clean =gsub("36m", "36",clean, ignore.case = T)
  clean =gsub("35m", "35",clean, ignore.case = T)
  clean =gsub("34m", "34",clean, ignore.case = T)
  clean =gsub("33m", "33",clean, ignore.case = T)
  clean =gsub("32m", "32",clean, ignore.case = T)
  clean =gsub("31m", "31",clean, ignore.case = T)
  clean =gsub("30m", "30",clean, ignore.case = T)
  clean =gsub("29m", "29",clean, ignore.case = T)
  clean =gsub("28m", "28",clean, ignore.case = T)
  clean =gsub("27m", "27",clean, ignore.case = T)
  clean =gsub("26m", "26",clean, ignore.case = T)
  clean =gsub("25m", "25",clean, ignore.case = T)
  clean =gsub("24m", "24",clean, ignore.case = T)
  clean =gsub("23m", "23",clean, ignore.case = T)
  clean =gsub("22m", "22",clean, ignore.case = T)
  clean =gsub("21m", "21",clean, ignore.case = T)
  clean =gsub("20m", "20",clean, ignore.case = T)
  clean =gsub("19m", "19",clean, ignore.case = T)
  clean =gsub("18m", "18",clean, ignore.case = T)
  clean =gsub("17m", "17",clean, ignore.case = T)
  clean =gsub("16m", "16",clean, ignore.case = T)
  clean =gsub("15m", "15",clean, ignore.case = T)
  clean =gsub("14m", "14",clean, ignore.case = T)
  clean =gsub("13m", "13",clean, ignore.case = T)
  clean =gsub("12m", "12",clean, ignore.case = T)
  clean =gsub("11m", "11",clean, ignore.case = T)
  clean =gsub("10m", "10",clean, ignore.case = T)
  clean =gsub("9m", "9",clean, ignore.case = T)
  clean =gsub("8m", "8",clean, ignore.case = T)
  clean =gsub("7m", "7",clean, ignore.case = T)
  clean =gsub("6m", "6",clean, ignore.case = T)
  clean =gsub("5m", "5",clean, ignore.case = T)
  clean =gsub("4m", "4",clean, ignore.case = T)
  clean =gsub("3m", "3",clean, ignore.case = T)
  clean =gsub("2m", "2",clean, ignore.case = T)
  clean =gsub("1m", "1",clean, ignore.case = T)
  clean =gsub("24h", "1440",clean, ignore.case = T)
  clean =gsub("23h", "1380",clean, ignore.case = T)
  clean =gsub("22h", "1320",clean, ignore.case = T)
  clean =gsub("21h", "1260",clean, ignore.case = T)
  clean =gsub("20h", "1200",clean, ignore.case = T)
  clean =gsub("19h", "1140",clean, ignore.case = T)
  clean =gsub("18h", "1080",clean, ignore.case = T)
  clean =gsub("17h", "1020",clean, ignore.case = T)
  clean =gsub("16h", "960",clean, ignore.case = T)
  clean =gsub("15h", "900",clean, ignore.case = T)
  clean =gsub("14h", "840",clean, ignore.case = T)
  clean =gsub("13h", "780",clean, ignore.case = T)
  clean =gsub("12h", "720",clean, ignore.case = T)
  clean =gsub("11h", "660",clean, ignore.case = T)
  clean =gsub("10h", "600",clean, ignore.case = T)
  clean =gsub("9h", "540",clean, ignore.case = T)
  clean =gsub("8h", "480",clean, ignore.case = T)
  clean =gsub("7h", "420",clean, ignore.case = T)
  clean =gsub("6h", "360",clean, ignore.case = T)
  clean =gsub("5h", "300",clean, ignore.case = T)
  clean =gsub("4h", "240",clean, ignore.case = T)
  clean =gsub("3h", "180",clean, ignore.case = T)
  clean =gsub("2h", "120",clean, ignore.case = T)
  clean =gsub("1h", "60",clean, ignore.case = T)
  clean =gsub("16d", "18840",clean, ignore.case = T)
  clean =gsub("15d", "17700",clean, ignore.case = T)
  clean =gsub("14d", "16560",clean, ignore.case = T)
  clean =gsub("13d", "15420",clean, ignore.case = T)
  clean =gsub("12d", "14280",clean, ignore.case = T)
  clean =gsub("11d", "13140",clean, ignore.case = T)
  clean =gsub("10d", "12000",clean, ignore.case = T)
  clean =gsub("9d", "10860",clean, ignore.case = T)
  clean =gsub("8d", "9720",clean, ignore.case = T)
  clean =gsub("7d", "8580",clean, ignore.case = T)
  clean =gsub("6d", "7440",clean, ignore.case = T)
  clean =gsub("5d", "6300",clean, ignore.case = T)
  clean =gsub("4d", "5160",clean, ignore.case = T)
  clean =gsub("3d", "4020",clean, ignore.case = T)
  clean =gsub("2d", "2880",clean, ignore.case = T)
  clean =gsub("1d", "1440",clean, ignore.case = T)
  clean = as.numeric(clean)
  x = as_datetime(time$download[i]-clean*60)
  x
  return(x)
}

dayFunction <- function(i){

  x = as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60*60*24)
 
  return(x)
 
}

hourFunction <- function(i){
  x = as.character(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60*60+1*60*60)
 
  return(x)
}


minuteFunction <- function(i){
  
  x = as.character(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60+1*60*60)

  return(x)
}

secondFunction <- function(i){
  x = as.character(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i]))))

  return(x)
}

justNowFunction <- function(i){
  x = time$download[i]
  return(x)
}

yesterdayAtFunction <- function(i){
  testSplit = do.call('rbind',strsplit(as.character(time$article[i]),   " "   ,fixed=TRUE))
  yesterdayTime = testSplit[length(testSplit)]
  yesterdayTime
   yesterdayDate = as.character(as.Date((time$download[i]-(60*60*24))))
  yesterdayDate
  x = paste(yesterdayDate, yesterdayTime, sep=" ")
  x
 # x = ymd_hm(as.character(x))
  x
  return(x)
}

spanishMonthFunction <- function(i){
  
  ########### FOR FACEBOOK ###########################
  clean =gsub("a las", "",time$article[i], ignore.case = T)
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

   test = time$article[i]
  clean =gsub("um", "",test, ignore.case = T)
  clean =gsub("\\.", "",clean)
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
  
  x = as_datetime(as.character(unlist(dmy_hm(clean))))
     return(x)
}

germanMonthFunctionNoTime <- function(i){
  downloadTime= time$download[i]
  downloadTime =as.character(format(as.POSIXct(downloadTime, format="%Y-%m-%d %H:%M:%S"), format="%H:%M:%S"))
  clean =gsub("um", "",time$article[i], ignore.case = T)
  clean =gsub("\\.", "",clean)
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
  clean = paste(clean,downloadTime)
  x = as_datetime(as.character(unlist(dmy_hms(clean))))
  return(x)
}

#######################################################################################################
#-----> Importing and DF Setup
#import = read_csv("~/Ubiqum Data Science/cryptnami/cryptnami/www/bitcoinPull 2018-01-25")

import = cleaned
#----> 

#time = read_excel("Sentiment Engines/Date Time Cleaning/test formats.xlsx")

#time = data.frame()

#-----------------------


timeNOWGMT = import$time_now_gmt
name = import$name
articleTime = import$article_time

time = data.frame(timeNOWGMT,name,articleTime)

time$title =  import$title
time$paragraph =import$paragraph
time$source =import$source
time$price_gfbtc =import$price_gfbtc
time$price_gf_delta_btc =import$price_gf_delta_btc
time$api_last_btc =import$api_last_btc
time$api_vol_btc =import$api_vol_btc
time$api_bid_btc =import$api_bid_btc
time$combination = import$combination
time$text = import$text
rm(import)

#-------------------------

time$datez =as.character(unlist(data.frame(time$articleTime)))
time$timeNOWGMT = ymd_hms(as.character(time$timeNOWGMT))

y = split(time, time$name)


blm =  as.data.frame(y["Bloomberg"])
bcn = as.data.frame(y["Bitcoin News"])              #### 1 Hours left of UTC
cd = as.data.frame(y["China Daily"])
cnbc = as.data.frame(y["CNBC"])                     #### 5 hours left of UTC
cndk= as.data.frame(y["Coin Desk"])
fbbtc= as.data.frame(y["Facebook BTC Group"])       #### 0 hours right of UTC
fbsrch= as.data.frame(y["Facebook Search"])         #### 0 hours right of UTC
fr= as.data.frame(y["Free Republic"])               #### 8 hours left of UTC
gf= as.data.frame(y["Google Finance"])
rba= as.data.frame(y["Redit BTC All"])              #### 0 hours right of UTC
rbmine = as.data.frame(y["Redit BTC Mining"])       #### 0 hours right of UTC
reu= as.data.frame(y["Reuters"])                    #### 5 hours left of UTC
scmp= as.data.frame(y["South China Morning Post"])  #### 8 hours right of UTC
tw= as.data.frame(y["Twitter"])
yn= as.data.frame(y["Yahoo News"])
you= as.data.frame(y["Youtube"])
zh= as.data.frame(y["Zero Hedge"])                  #### 0 hours right of UTC
wsj =  as.data.frame(y["Wall Street Journal"])      #### 5 hours left of UTC
rbb = as.data.frame(y["Redit BTC Bay"])             #### 0 hours right of UTC
rbm = as.data.frame(y["Redit BTC Markets"])         #### 0 hours right of UTC
rbc  = as.data.frame(y["Redit Crypto"])             #### 0 hours right of UTC
bbc = as.data.frame(y["BBC"])
gplus = as.data.frame(y["Google Plus"])
iet= as.data.frame(y["India Economic Times:"])      #### 5.5 hours right of UTC
ccn = as.data.frame(y["Crypto Coin News"])          #### 0 hours right of UTC
rbx =as.data.frame(y["Redit BTC XT"])               #### 0 hours right of UTC
frt = as.data.frame(y["Fortune"])                   #### 0 hours right of UTC
#------------------------------------------------------------









#----------->BBC Date Cleaning Function -------------------------
bbc$cleaned = as.POSIXct(bbc$BBC.timeNOWGMT)
downloadTime =  as.data.frame(bbc$BBC.timeNOWGMT)
articleTime =  as.data.frame(bbc$BBC.datez)

time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)
for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl('\\d{5,6}', time$article[i]))
  {    bbc$error[i] = "Invalid Date"  
  bbc$cleaned[i] = NA
  }
  else if(grepl("\\d", time$article[i]))
  {    bbc$error[i] = "date"  
      bbc$cleaned[i] = ymd_hms(paste(dmy(time$article[i]),strftime(as.character(time$download[i]), '%H:%M:%S')))#-- adds download time to just a date
   }
  else {bbc$error[i] = "Default"
  bbc$cleaned[i]=time$download[i]
  
  }
}
close(pb)
#----------->BCN Date Cleaning Function -------------------------
bcn$cleaned = as.POSIXct(bcn$Bitcoin.News.timeNOWGMT)

downloadTime =  as.data.frame(bcn$Bitcoin.News.timeNOWGMT)
articleTime =  as.data.frame(bcn$Bitcoin.News.datez)

time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl(paste(hourLibrary,collapse="|"), time$article[i]))
  {    bcn$error[i] = "Hours"  
  bcn$cleaned[i] = hourFunction(i)
  }
  
  else if(grepl(paste(dayLibrary,collapse="|"), time$article[i]))
  {    bcn$error[i] = "Days"  
   bcn$cleaned[i] = dayFunction(i)
  }
  else if(grepl("\\d", time$article[i]))
  {    bcn$error[i] = "dates"  
   bcn$cleaned[i] = mdy_hms(as.character(time$article[i]))+1*60*60
  }else{
  bcn$error[i] = "default"
  bcn$cleaned[i] = as.POSIXct(time$download[i])+1*60*60
  }
 }
close(pb)
#----------->cd Date Cleaning Function -------------------------

cd$cleaned = as.POSIXct(cd$China.Daily.timeNOWGMT)
downloadTime =  as.data.frame(cd$China.Daily.timeNOWGMT)
articleTime =  as.data.frame(cd$China.Daily.datez)

time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)
for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
   if(grepl("\\d", time$article[i]))
  {    cd$error[i] = "dates"  
  cd$cleaned[i] = as.POSIXct(time$download[i])
   }
  else{cd$cleaned[i] = time$download[i]
  cd$error[i] = "default"
  }
}
close(pb)
#----------->cnbc Date Cleaning Function -------------------------

cnbc$cleaned = as.POSIXct(cnbc$CNBC.timeNOWGMT)

downloadTime =  as.data.frame(cnbc$CNBC.timeNOWGMT)
articleTime =  as.data.frame(cnbc$CNBC.datez)

time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

as.POSIXct(time$download[10])
time$download[10]
pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)
for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl(paste(hourLibrary,collapse="|"), time$article[i]))
  {    cnbc$error[i] = "Hours"  
  cnbc$cleaned[i] = hourFunction(i)
  }
  
  else if(grepl(paste(minuteLibrary,collapse="|"), time$article[i]))
  {    cnbc$error[i] = "Minutes"  
  cnbc$cleaned[i] = minuteFunction(i)
  }
  else if(grepl(paste(nowLibrary,collapse="|"), time$article[i]))
  {    cnbc$error[i] = "Now"  
   cnbc$cleaned[i] =as.POSIXct(time$download[i])
  }
  else if(grepl("\\d", time$article[i]))
  {    cnbc$error[i] = "dates"  
  date = time$article[i]
  m = regexpr("^([0-9]|([1][0-2])):[0-5][0-9][[:space:]][[:space:]]?([ap][m]?|[AP][M]?)", date)
  timeStrip = as.character(regmatches(date, m))
  d = regexpr("\\d{1,2}\\s\\w+\\s\\d{4}", date)
  dateStrip = as.character(regmatches(date, d))
  cnbc$cleaned[i]=as.POSIXct(parse_date_time(paste(dateStrip,timeStrip),"dmy IM  p")+5*60*60)
  }else{
    cnbc$error[i] = "default"
    cnbc$cleaned[i] = as.POSIXct(time$download[i])
  }
}
close(pb)
#----------->cndk Date Cleaning Function -------------------------
cndk$cleaned = as.POSIXct(cndk$Coin.Desk.timeNOWGMT)
downloadTime =  as.data.frame(cndk$Coin.Desk.timeNOWGMT)
articleTime =  as.data.frame(cndk$Coin.Desk.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl("\\d", time$article[i]))
  {    cndk$error[i] = "dates"  
  cndk$cleaned[i] = mdy_hm(as.character(time$article[i]))
  }
  else{cndk$cleaned[i] = time$download[i]
  cndk$error[i] = "default"
  }
}

close(pb)
#----------->fbbtc Date Cleaning Function (German) -------------------------

fbbtc$cleaned = as.POSIXct(fbbtc$Facebook.BTC.Group.timeNOWGMT)
downloadTime =  as.data.frame(fbbtc$Facebook.BTC.Group.timeNOWGMT)
articleTime =  as.data.frame(fbbtc$Facebook.BTC.Group.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
if(grepl(paste(germanMonthLibrary,collapse="|"), time$article[i]))
{ if(grepl(":", time$article[i]))   {
  fbbtc$error[i] = "Month with Time"
  fbbtc$cleaned[i] = germanMonthFunction(i)
}else { 
  fbbtc$error[i] = "Month No Time"
  fbbtc$cleaned[i] =germanMonthFunctionNoTime(i)
  }

}else if(grepl(paste(hourLibrary,collapse="|"), time$article[i]))
{
  fbbtc$error[i] = "Hours"
  fbbtc$cleaned[i] = hourFunction(i)
}else if(grepl(paste(minuteLibrary,collapse="|"), time$article[i]))
{
  fbbtc$error[i] = "Minutes"
  fbbtc$cleaned[i] = minuteFunction(i)
}else if(grepl(paste(nowLibrary,collapse="|"), time$article[i]))
{
  fbbtc$error[i] = "Time Meow"
  fbbtc$cleaned[i] =as.POSIXct(time$download[i])
}else if(grepl(paste(yesterdayLibrary,collapse="|"), time$article[i]))
{
  fbbtc$error[i] = "Yesterday at __"
  fbbtc$cleaned[i] =ymd_hm(yesterdayAtFunction(i))
}else
{
  fbbtc$error[i] = "Default"
  fbbtc$cleaned[i] = time$download[i]
}
}
close(pb)

#----------->fbbtc Date Cleaning Function -------------------------


fbsrch$cleaned = as.POSIXct(fbsrch$Facebook.Search.timeNOWGMT)
downloadTime =  as.data.frame(fbsrch$Facebook.Search.timeNOWGMT)
articleTime =  as.data.frame(fbsrch$Facebook.Search.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)
for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl(paste(germanMonthLibrary,collapse="|"), time$article[i]))
  { if(grepl(":", time$article[i]))   {
    fbsrch$error[i] = "Month with Time"
    fbsrch$cleaned[i] = germanMonthFunction(i)
  }else { 
    fbsrch$error[i] = "Month No Time"
    fbsrch$cleaned[i] =germanMonthFunctionNoTime(i)
  }
    
  }else if(grepl(paste(hourLibrary,collapse="|"), time$article[i]))
  {
    fbsrch$error[i] = "Hours"
    fbsrch$cleaned[i] = hourFunction(i)
  }else if(grepl(paste(minuteLibrary,collapse="|"), time$article[i]))
  {
    fbsrch$error[i] = "Minutes"
    fbsrch$cleaned[i] = minuteFunction(i)
  }else if(grepl(paste(nowLibrary,collapse="|"), time$article[i]))
  {
    fbsrch$error[i] = "Time Meow"
    fbsrch$cleaned[i] =as.POSIXct(time$download[i])
  }else if(grepl(paste(yesterdayLibrary,collapse="|"), time$article[i]))
  {
    fbsrch$error[i] = "Yesterday at __"
    fbsrch$cleaned[i] =ymd_hm(yesterdayAtFunction(i))
  }else
  {
    fbsrch$error[i] = "Default"
    fbsrch$cleaned[i] = time$download[i]
  }
}
close(pb)
#----------->fr Date Cleaning Function -------------------------

fr$cleaned = as.POSIXct(fr$Free.Republic.timeNOWGMT)
downloadTime =  as.data.frame(fr$Free.Republic.timeNOWGMT)
articleTime =  as.data.frame(fr$Free.Republic.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")
pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl("\\d", time$article[i]))
  {    fr$error[i] = "dates"  
  fr$cleaned[i]=as.POSIXct(parse_date_time(time$article[i],"m/d/y IMS  p")+5*60*60)
  }
  else{fr$cleaned[i] = time$download[i]
  fr$error[i] = "default"
  }
}
close(pb)
#----------->gf Date Cleaning Function -------------------------

gf$Google.Finance.datez= gsub("^.*?-","",gf$Google.Finance.datez)
gf$cleaned = as.POSIXct(gf$Google.Finance.timeNOWGMT)
downloadTime =  as.data.frame(gf$Google.Finance.timeNOWGMT)
articleTime =  as.data.frame(gf$Google.Finance.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")
pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
if(grepl(paste(hourLibrary,collapse="|"), time$article[i]))
{
 # print(hourFunction(1))
  gf$error[i] = "Hours"
  gf$cleaned[i] =  as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60*60)
  
  
}else if(grepl(paste(minuteLibrary,collapse="|"), time$article[i]))
{
  gf$error[i] = "Minutes"
  gf$cleaned[i] = as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60)

}else if(grepl(paste(nowLibrary,collapse="|"), time$article[i]))
{
  gf$error[i] = "Time Meow"
  gf$cleaned[i] =as.POSIXct(time$download[i])
}else if(grepl(paste(yesterdayLibrary,collapse="|"), time$article[i]))
{
  gf$error[i] = "Yesterday at __"
  gf$cleaned[i] =ymd_hm(yesterdayAtFunction(i))
}else
{
  gf$error[i] = "Default"
  gf$cleaned[i] = time$download[i]
}
}
close(pb)
#----------->gplus Date Cleaning Function -------------------------


gplus$cleaned = as.POSIXct(gplus$Google.Plus.timeNOWGMT)
downloadTime =  as.data.frame(gplus$Google.Plus.timeNOWGMT)
articleTime =  as.data.frame(gplus$Google.Plus.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl("\\d", time$article[i]))
  {    gplus$error[i] = "dates"  
  gplus$cleaned[i]=googlePlusFunction(i)
  }
  else{gplus$cleaned[i] = time$download[i]
  gplus$error[i] = "default"
  }
}
close(pb)
#----------->rba Date Cleaning Function -------------------------

rba$cleaned = as.POSIXct(rba$Redit.BTC.All.timeNOWGMT)
downloadTime =  as.data.frame(rba$Redit.BTC.All.timeNOWGMT)
articleTime =  as.data.frame(rba$Redit.BTC.All.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl(paste(hourLibrary,collapse="|"), time$article[i]))
  {
    # print(hourFunction(1))
    rba$error[i] = "Hours"
    rba$cleaned[i] =  as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60*60)
    
    
  }else if(grepl(paste(minuteLibrary,collapse="|"), time$article[i]))
  {
    rba$error[i] = "Minutes"
    rba$cleaned[i] = as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60)
    
  }else if(grepl(paste(nowLibrary,collapse="|"), time$article[i]))
  {
    rba$error[i] = "Time Meow"
    rba$cleaned[i] =as.POSIXct(time$download[i])
  }else if(grepl(paste(dayLibrary,collapse="|"), time$article[i]))
  {
    rba$error[i] = "Day"
    rba$cleaned[i] =dayFunction(i)
  }else if(grepl(paste(yesterdayLibrary,collapse="|"), time$article[i]))
  {
    rba$error[i] = "Yesterday at __"
    rba$cleaned[i] =ymd_hm(yesterdayAtFunction(i))
  }else
  {
    rba$error[i] = "Too Old"
    rba$cleaned[i] = NA
  }
}
close(pb)
#----------->rbb Date Cleaning Function -------------------------

rbb$cleaned = as.POSIXct(rbb$Redit.BTC.Bay.timeNOWGMT)
downloadTime =  as.data.frame(rbb$Redit.BTC.Bay.timeNOWGMT)
articleTime =  as.data.frame(rbb$Redit.BTC.Bay.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl(paste(hourLibrary,collapse="|"), time$article[i]))
  {
    # print(hourFunction(1))
    rbb$error[i] = "Hours"
    rbb$cleaned[i] =  as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60*60)
    
    
  }else if(grepl(paste(minuteLibrary,collapse="|"), time$article[i]))
  {
    rbb$error[i] = "Minutes"
    rbb$cleaned[i] = as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60)
    
  }else if(grepl(paste(nowLibrary,collapse="|"), time$article[i]))
  {
    rbb$error[i] = "Time Meow"
    rbb$cleaned[i] =as.POSIXct(time$download[i])
  }else  if(grepl(paste(dayLibrary,collapse="|"), time$article[i]))
  {
    rbb$error[i] = "Day"
    rbb$cleaned[i] =dayFunction(i)
  }else if(grepl(paste(yesterdayLibrary,collapse="|"), time$article[i]))
  {
    rbb$error[i] = "Yesterday at __"
    rbb$cleaned[i] =ymd_hm(yesterdayAtFunction(i))
  }else
  {
    rbb$error[i] = "Too Old"
    rbb$cleaned[i] = time$download[i]
  }
}
close(pb)
#----------->rbm Date Cleaning Function -------------------------

rbm$cleaned = as.POSIXct(rbm$Redit.BTC.Markets.timeNOWGMT)
downloadTime =  as.data.frame(rbm$Redit.BTC.Markets.timeNOWGMT)
articleTime =  as.data.frame(rbm$Redit.BTC.Markets.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl(paste(hourLibrary,collapse="|"), time$article[i]))
  {
    # print(hourFunction(1))
    rbm$error[i] = "Hours"
    rbm$cleaned[i] =  as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60*60)
    
    
  }else if(grepl(paste(minuteLibrary,collapse="|"), time$article[i]))
  {
    rbm$error[i] = "Minutes"
    rbm$cleaned[i] = as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60)
    
  }else if(grepl(paste(nowLibrary,collapse="|"), time$article[i]))
  {
    rbm$error[i] = "Time Meow"
    rbm$cleaned[i] =as.POSIXct(time$download[i])
  }else if(grepl(paste(dayLibrary,collapse="|"), time$article[i]))
  {
    rbm$error[i] = "Day"
    rbm$cleaned[i] =dayFunction(i)
  }else  if(grepl(paste(yesterdayLibrary,collapse="|"), time$article[i]))
  {
    rbm$error[i] = "Yesterday at __"
    rbm$cleaned[i] =ymd_hm(yesterdayAtFunction(i))
  }else
  {
    rbm$error[i] = "Too Old"
    rbm$cleaned[i] = time$download[i]
  }
}
close(pb)
#----------->rbc Date Cleaning Function -------------------------

rbc$cleaned = as.POSIXct(rbc$Redit.Crypto.timeNOWGMT)
downloadTime =  as.data.frame(rbc$Redit.Crypto.timeNOWGMT)
articleTime =  as.data.frame(rbc$Redit.Crypto.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl(paste(hourLibrary,collapse="|"), time$article[i]))
  {
    # print(hourFunction(1))
    rbc$error[i] = "Hours"
    rbc$cleaned[i] =  as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60*60)
    
    
  }else if(grepl(paste(minuteLibrary,collapse="|"), time$article[i]))
  {
    rbc$error[i] = "Minutes"
    rbc$cleaned[i] = as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60)
    
  }else if(grepl(paste(nowLibrary,collapse="|"), time$article[i]))
  {
    rbc$error[i] = "Time Meow"
    rbc$cleaned[i] =as.POSIXct(time$download[i])
  }else  if(grepl(paste(dayLibrary,collapse="|"), time$article[i]))
  {
    rbc$error[i] = "Day"
    rbc$cleaned[i] =dayFunction(i)
  }else if(grepl(paste(yesterdayLibrary,collapse="|"), time$article[i]))
  {
    rbc$error[i] = "Yesterday at __"
    rbc$cleaned[i] =ymd_hm(yesterdayAtFunction(i))
  }else
  {
    rbc$error[i] = "Too Old"
    rbc$cleaned[i] = time$download[i]
  }
}
close(pb)
#----------->RBmine Date Cleaning Function -------------------------

rbmine$cleaned = as.POSIXct(rbmine$Redit.BTC.Mining.timeNOWGMT)
downloadTime =  as.data.frame(rbmine$Redit.BTC.Mining.timeNOWGMT)
articleTime =  as.data.frame(rbmine$Redit.BTC.Mining.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl(paste(hourLibrary,collapse="|"), time$article[i]))
  {
    # print(hourFunction(1))
    rbmine$error[i] = "Hours"
    rbmine$cleaned[i] =  as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60*60)
    
    
  }else if(grepl(paste(minuteLibrary,collapse="|"), time$article[i]))
  {
    rbmine$error[i] = "Minutes"
    rbmine$cleaned[i] = as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60)
    
  }else if(grepl(paste(nowLibrary,collapse="|"), time$article[i]))
  {
    rbmine$error[i] = "Time Meow"
    rbmine$cleaned[i] =as.POSIXct(time$download[i])
  }else  if(grepl(paste(dayLibrary,collapse="|"), time$article[i]))
  {
    rbmine$error[i] = "Day"
    rbmine$cleaned[i] =dayFunction(i)
  }else if(grepl(paste(yesterdayLibrary,collapse="|"), time$article[i]))
  {
    rbmine$error[i] = "Yesterday at __"
    rbmine$cleaned[i] =ymd_hm(yesterdayAtFunction(i))
  }else
  {
    rbmine$error[i] = "Too Old"
    rbmine$cleaned[i] = time$download[i]
  }
}
close(pb)
#----------->reu Date Cleaning Function -------------------------


reu$cleaned = as.POSIXct(reu$Reuters.timeNOWGMT)
downloadTime =  as.data.frame(reu$Reuters.timeNOWGMT)
articleTime =  as.data.frame(reu$Reuters.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl("\\d", time$article[i]))
  {    reu$error[i] = "dates"  
  reu$cleaned[i]=as_datetime(mdy_hm(time$article[i])+5*60*60)
  }
  else{reu$cleaned[i] = time$download[i]
  reu$error[i] = "default"
  }
}
close(pb)
#----------->blm Date Cleaning Function -------------------------


blm$cleaned = as.POSIXct(blm$Bloomberg.timeNOWGMT)
downloadTime =  as.data.frame(blm$Bloomberg.timeNOWGMT)
articleTime =  as.data.frame(blm$Bloomberg.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)
for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i) 
  blm$cleaned[i] = time$download[i]
  blm$error[i] = "Writing Download Time"
  
}
close(pb)
#----------->scmp Date Cleaning Function -------------------------




scmp$cleaned = as.POSIXct(scmp$South.China.Morning.Post.timeNOWGMT)
downloadTime =  as.data.frame(scmp$South.China.Morning.Post.timeNOWGMT)
articleTime =  as.data.frame(mdy_hm(gsub("by.*","",gsub("^.*?Posted","",scmp$South.China.Morning.Post.datez))))
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")
pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)
for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl("\\d", time$article[i]))
  {    scmp$error[i] = "dates"  
  scmp$cleaned[i]=as_datetime(ymd_hms(time$article[i])-8*60*60)
  }
  else{scmp$cleaned[i] = time$download[i]
  scmp$error[i] = "default"
  }
}
close(pb)

#----------->tw Date Cleaning Function -------------------------


tw$cleaned = as.POSIXct(tw$Twitter.timeNOWGMT)
downloadTime =  as.data.frame(tw$Twitter.timeNOWGMT)
articleTime =  as.data.frame(tw$Twitter.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl("\\d", time$article[i]))
  {    tw$error[i] = "dates"  
  tw$cleaned[i]=googlePlusFunction(i)
  }
  else{tw$cleaned[i] = time$download[i]
  tw$error[i] = "default"
  }
}
close(pb)
#----------->iet Date Cleaning Function -------------------------


iet$cleaned = as.POSIXct(iet$India.Economic.Times..timeNOWGMT)
downloadTime =  as.data.frame(iet$India.Economic.Times..timeNOWGMT)
articleTime =  as.data.frame(iet$India.Economic.Times..datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)


for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl("\\d", time$article[i]))
  {    iet$error[i] = "dates"  
  iet$cleaned[i]=dmy_hm(time$article[i])-5.5*60*60
  }
  else{iet$cleaned[i] = time$download[i]
  iet$error[i] = "default"
  }
}
close(pb)
#----------->yn Date Cleaning Function -------------------------


yn$cleaned = as.POSIXct(yn$Yahoo.News.timeNOWGMT)
downloadTime =  as.data.frame(yn$Yahoo.News.timeNOWGMT)
articleTime =  as.data.frame(yn$Yahoo.News.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")
pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl(paste(hourLibrary,collapse="|"), time$article[i]))
  {
    # print(hourFunction(1))
    yn$error[i] = "Hours"
    yn$cleaned[i] =  as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60*60)
    
    
  }else if(grepl(paste(minuteLibrary,collapse="|"), time$article[i]))
  {
    yn$error[i] = "Minutes"
    yn$cleaned[i] = as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60)
    
  }else if(grepl(paste(nowLibrary,collapse="|"), time$article[i]))
  {
    yn$error[i] = "Time Meow"
    yn$cleaned[i] =as.POSIXct(time$download[i])
  }else if(grepl(paste(dayLibrary,collapse="|"), time$article[i]))
  {
    yn$error[i] = "Days ago"
    yn$cleaned[i] =dayFunction(i)# as.character(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60*60*24)
  }else
  {
    yn$error[i] = "Default"
    yn$cleaned[i] = time$download[i]
  }
}
close(pb)
#----------->you Date Cleaning Function -------------------------

you$cleaned = as.POSIXct(you$Youtube.timeNOWGMT)
downloadTime =  as.data.frame(you$Youtube.timeNOWGMT)
articleTime =  as.data.frame(you$Youtube.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")
pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl(paste(hourLibrary,collapse="|"), time$article[i]))
  {
    # print(hourFunction(1))
    you$error[i] = "Hours"
    hours = as.numeric(stri_extract_first_regex(time$article[i], "[0-9]+"))*60*60
    you$cleaned[i] =  as_datetime(time$download[i] - hours)
    
    
  }else if(grepl(paste(minuteLibrary,collapse="|"), time$article[i]))
  {
    you$error[i] = "Minutes"
    minutes = as.numeric(stri_extract_first_regex(time$article[i], "[0-9]+"))*60
    you$cleaned[i] = as_datetime(time$download[i] - minutes)
    
  }else if(grepl(paste(nowLibrary,collapse="|"), time$article[i]))
  {
    you$error[i] = "Time Meow"
    you$cleaned[i] =as.POSIXct(time$download[i])
  }else if(grepl(paste(dayLibrary,collapse="|"), time$article[i]))
  {
    you$error[i] = "Days ago"
    you$cleaned[i] =dayFunction(i)
  }else
  {
    you$error[i] = "Default No Time.but worth keeping"
    you$cleaned[i] = time$download[i]
  }
}
close(pb)
#----------->you Date Cleaning Function -------------------------

zh$cleaned = as.POSIXct(zh$Zero.Hedge.timeNOWGMT)
downloadTime =  as.data.frame(zh$Zero.Hedge.timeNOWGMT)
articleTime =  as.data.frame(zh$Zero.Hedge.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")
pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

test = time$article
test = gsub("^.*?-","",test)
test =gsub("^.*?-","",test)
dateTrimmed = gsub("-.*","",test)
test =gsub("^.*?-","",test)
timeTrimmed = gsub("-.*","",test)

time$article = paste(dateTrimmed,timeTrimmed)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl("\\d", time$article[i]))
  {    zh$error[i] = "dates"  
  zh$cleaned[i]= mdy_hm(time$article[i])
  }
  else{zh$cleaned[i] = time$download[i]
  zh$error[i] = "default"
  }
}
close(pb)
#----------->you Date Cleaning Function -------------------------
wsj$cleaned = as.POSIXct(wsj$Wall.Street.Journal.timeNOWGMT)
downloadTime =  as.data.frame(wsj$Wall.Street.Journal.timeNOWGMT)
articleTime =  as.data.frame(wsj$Wall.Street.Journal.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl(paste(hourLibrary,collapse="|"), time$article[i]))
  {
    # print(hourFunction(1))
    wsj$error[i] = "Hours"
    wsj$cleaned[i] =  as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60*60)
    
    
  }else if(grepl(paste(minuteLibrary,collapse="|"), time$article[i]))
  {
    wsj$error[i] = "Minutes"
    wsj$cleaned[i] = as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60)
    
  }else if(grepl(paste(dayLibrary,collapse="|"), time$article[i]))
  {
    wsj$error[i] = "Days ago"
    wsj$cleaned[i] =dayFunction(i)# as.character(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60*60*24)
  }else if(grepl("\\d", time$article[i]))
  {
    wsj$error[i] = "Date"
    wsj$cleaned[i] = mdy_hm(time$article[i])+5*60*60
  }else
  {
    wsj$error[i] = "Default"
    wsj$cleaned[i] = time$download[i]
  }
}
close(pb)

#----------->ccn Date Cleaning Function -------------------------

ccn$cleaned = as.POSIXct(ccn$Crypto.Coin.News.timeNOWGMT )
downloadTime =  as.data.frame(ccn$Crypto.Coin.News.timeNOWGMT)
articleTime =  as.data.frame(ccn$Crypto.Coin.News.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)


for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl("\\d", time$article[i]))
  {    ccn$error[i] = "dates"  
  ccn$cleaned[i]=ymd_hms(paste(dmy(time$article[i]),strftime(as.character(time$download[i]), '%H:%M:%S')))#-- adds download time to just a date
  }
  else{ccn$cleaned[i] = time$download[i]
  ccn$error[i] = "default"
  }
}
close(pb)

#----------->rbx Date Cleaning Function -------------------------

rbx$cleaned = as.POSIXct(rbx$Redit.BTC.XT.timeNOWGMT)
downloadTime =  as.data.frame(rbx$Redit.BTC.XT.timeNOWGMT)
articleTime =  as.data.frame(rbx$Redit.BTC.XT.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")
pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)

for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl(paste(hourLibrary,collapse="|"), time$article[i]))
  {
    # print(hourFunction(1))
    rbx$error[i] = "Hours"
    rbx$cleaned[i] =  as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60*60)
    
    
  }else if(grepl(paste(minuteLibrary,collapse="|"), time$article[i]))
  {
    rbx$error[i] = "Minutes"
    rbx$cleaned[i] = as_datetime(time$download[i] - as.numeric(regmatches(time$article[i],gregexpr('[0-9]+',time$article[i])))*60)
    
  }else if(grepl(paste(nowLibrary,collapse="|"), time$article[i]))
  {
    rbx$error[i] = "Time Meow"
    rbx$cleaned[i] =as.POSIXct(time$download[i])
  }else if(grepl(paste(dayLibrary,collapse="|"), time$article[i]))
  {
    rbx$error[i] = "Day"
    rbx$cleaned[i] =dayFunction(i)
  }else if(grepl(paste(yesterdayLibrary,collapse="|"), time$article[i]))
  {
    rbx$error[i] = "Yesterday at __"
    rbx$cleaned[i] =ymd_hm(yesterdayAtFunction(i))
  }else
  {
    rbx$error[i] = "Too Old"
    rbx$cleaned[i] = time$download[i]
  }
}
close(pb)

#----------->frt Date Cleaning Function -------------------------
frt$cleaned = as.POSIXct(frt$Fortune.timeNOWGMT)
downloadTime =  as.data.frame(frt$Fortune.timeNOWGMT)
articleTime =  as.data.frame(frt$Fortune.datez)
time = data.frame(downloadTime,articleTime)
colnames(time)= c("download", "article")

pb <- txtProgressBar(min = 0, max = length(time$article), style = 3)
for (i in 1:length(time$article))
{
  setTxtProgressBar(pb, i)
  if(grepl("\\d", time$article[i]))
  {    frt$error[i] = "dates"  
  frt$cleaned[i]=ymd_hms(paste(mdy(gsub("\\..*","",time$article[i])),strftime(as.character(time$download[i]), '%H:%M:%S')))#-- adds download time to just a date
  }
  else{frt$cleaned[i] = time$download[i]
  frt$error[i] = "default"
  }
}
close(pb)


#--------------------------BEGIN CONCATENATION-----------------------------------------
colnames(blm)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(bcn)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(cd)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(cnbc)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(cndk)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(fbbtc)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(fbsrch)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(fr)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(gf)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(rba)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(rbmine)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(reu)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(scmp)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(tw)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(yn)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(you)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(zh)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(wsj)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(rbb)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(rbm)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(rbc)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(bbc)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(gplus)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(iet)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(ccn)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(rbx)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
colnames(frt)= c("timeNOWGMT", "name","articleTime","title", "paragraph", "source", "price_gfbtc", "price_gf_delta_btc", "api_last_btc", "api_vol_btc", "api_bid_btc" , "combination", "text", "datez", "cleaned", "DateNotes")  
 
 

final = rbind(
  blm,
  bcn ,
  cd ,
   cnbc,
   cndk,
   fbbtc,
   fbsrch,
   fr,
   gf,
   rba,
   rbmine, 
   reu,
   scmp,
   tw,
   yn,
   you,
   zh,
   wsj,
   rbb ,
   rbm,
   rbc , 
   bbc ,
   gplus,
   iet,
   ccn ,
   rbx,
   frt 
 )

rm(
  blm,bcn , cd ,  cnbc,  cndk,  fbbtc,  fbsrch,  fr,  gf,  rba,  rbmine,   reu,  scmp,  tw,  yn,  you,  zh,
  wsj,  rbb ,  rbm,  rbc ,   bbc ,  gplus,  iet,  ccn ,  rbx,  frt )

#remove columns
remove.columns <- c( "X", "X1", "text.1", "articleTime", "title", "paragraph" ,
                    "source" , "price_gfbtc" , "api_bid_btc", "price_gf_delta_btc" ,"api_last_btc" ,
                    "api_vol_btc" , "api_vol_btc")

cleaned = final[,!(names(final) %in% remove.columns)]

final = subset(cleaned, cleaned > "2017-11-29 00:00:00")


#write.csv(final, "final5.csv")

z <- gzfile("cleaned_with_dates.csv.gz")
write.csv(final, z)

############################--------> END DATE CLEANING FUNCTION <--------###########################
#####################################################################################################
############################--------> BEGIN PRICE LOOKUP FUNCTION <--------###########################


final = read.csv(gzfile("cleaned_with_dates.csv.gz"))
final$cleaned = as_datetime(final$cleaned)


##############################-----KRAKKEN PRICE LOOKUP-----------########################################
#---------------------Access Kraken Historical Price Info

#---------> Download file 
# temp <- tempfile()
# download.file("http://api.bitcoincharts.com/v1/csv/krakenUSD.csv.gz",temp)
# historicPriceKrakkenDL <- read.csv(gzfile(temp, ".krakenUSD.csv"))
# rm(temp)

#---------> Or Read file 

historicPriceKrakkenDL <- read.csv(".krakenUSD.csv", skip =4580000 )
colnames(historicPriceKrakkenDL) = c("krakkenDate", "KrakkenPrice")
historicPriceKrakkenDL = historicPriceKrakkenDL[1:2]
#---------> End File Input 

final$cleaned = as_datetime(final$cleaned)
historicPriceKrakkenDL$krakkenDate = as_datetime(historicPriceKrakkenDL$krakkenDate)


final = as.data.table(final)
historicPriceKrakkenDL= as.data.table(historicPriceKrakkenDL)

final[, join_time:=cleaned]
historicPriceKrakkenDL[, join_time:=krakkenDate]


setkey(as.data.table(final), cleaned, join_time)
setkey(as.data.table(historicPriceKrakkenDL), krakkenDate, join_time)

testKrakken = unique(historicPriceKrakkenDL[final, roll = T, on = "join_time"])

testedKrakken = testKrakken[!duplicated(testKrakken$combination),]


colnames(testedKrakken)[1] = "KrakkenDate" #--> For Quality Control Purposes
colnames(testedKrakken)[2] = "KrakkenPrice"

testedKrakken = testedKrakken[,-c(3,4)]

final = testedKrakken

#############################--->Coinbase<------####################################
#---------------------Access Kraken Historical Price Info

#------------> Download Coinbase File-------------
# temp <- tempfile()
# download.file("http://api.bitcoincharts.com/v1/csv/coinbaseUSD.csv.gz",temp)
# historicPriceCoinbaseDL <- read.csv(gzfile(temp, ".coinbaseUSD"))
# rm(temp)

#------------> Open Coinbase File-------------


historicPriceCoinbaseDL <- read.csv(".coinbaseUSD.csv", skip =24000000)
colnames(historicPriceCoinbaseDL) = c("coinbaseDate", "coinbasePrice")
historicPriceCoinbaseDL = historicPriceCoinbaseDL[1:2]
#---------> End File Input 

final$cleaned = as_datetime(final$cleaned)
historicPriceCoinbaseDL$coinbaseDate = as_datetime(historicPriceCoinbaseDL$coinbaseDate)


final = as.data.table(final)
historicPriceCoinbaseDL= as.data.table(historicPriceCoinbaseDL)

final[, join_time:=cleaned]
historicPriceCoinbaseDL[, join_time:=coinbaseDate]


setkey(as.data.table(final), cleaned, join_time)
setkey(as.data.table(historicPriceCoinbaseDL), coinbaseDate, join_time)

testCoin = unique(historicPriceCoinbaseDL[final, roll = T, on = "join_time"])

testedCoin = testCoin[!duplicated(testCoin$combination),]

colnames(testedCoin)[1] = "CoinbaseDate" #--> For Quality Control Purposes
colnames(testedCoin)[2] = "CoinbasePrice"

testedCoin = testedCoin[,-c(3)]

final = testedCoin
#############################--->CEX<------####################################
#---------------------Access Kraken Historical Price Info

#------------> Download Coinbase File-------------
# temp <- tempfile()
# download.file("http://api.bitcoincharts.com/v1/csv/cexUSD.csv.gz",temp)
# historicPriceCEXDL <- read.csv(gzfile(temp, ".coinbaseUSD"))
# rm(temp)

#------------> Open Coinbase File-------------



historicPriceCEXDL <- read.csv(".cexUSD.csv")
colnames(historicPriceCEXDL) = c("cexDate", "cexPrice")
historicPriceCEXDL = historicPriceCEXDL[1:2]
#---------> End File Input 

final$cleaned = as_datetime(final$cleaned)
historicPriceCEXDL$cexDate = as_datetime(historicPriceCEXDL$cexDate)


final = as.data.table(final)
historicPriceCEXDL= as.data.table(historicPriceCEXDL)

final[, join_time:=cleaned]
historicPriceCEXDL[, join_time:=cexDate]


setkey(as.data.table(final), cleaned, join_time)
setkey(as.data.table(historicPriceCEXDL), cexDate, join_time)

testCex = unique(historicPriceCEXDL[final, roll = T, on = "join_time"])

testedCex = testCex[!duplicated(testCex$combination),]

colnames(testedCex)[1] = "cexDate" #--> For Quality Control Purposes
colnames(testedCex)[2] = "cexPrice"

testedCex = testedCex[,-c(3)]

final = testedCex
z <- gzfile("cleaned_w_Krakken_Coinbase_Cex.csv.gz")
write.csv(final, z)

####################################################################################
####################################################################################
# # historicPriceKrakken = historicPriceKrakkenDL[1:nrow(historicPriceKrakkenDL),]
# # rm(historicPriceKrakkenDL)
# # colnames(historicPriceKrakken)= c("date_Krakken", "price_USD_BTC_Krakken", "vol_Krakken")
# # historicPriceKrakken$date = as_datetime(as.numeric(historicPriceKrakken$date))
# # 
# # #------------------- Isolate Key Dates and dump Historical Price
# # 
# # final = na.omit(final)  #---->set to the final output of the date cleaner
# # final = final#[1:10,]  #-----> restricts digestion amount
# # #----Selecting the first and last date from the final output of the date cleaner
# # firstDate = final$cleaned[which.min(final$cleaned)]
# # lastDate = final$cleaned[which.max(final$cleaned)]
# # 
# # 
# # y = as.POSIXct(final$cleaned)
# # x = structure(list(date = historicPriceKrakken$date, Value = historicPriceKrakken$`price USD`, class = "data.frame"))
# # 
# # minValue = sapply(firstDate, function(z) which.min(abs(x$date - z)))
# # maxValue = sapply(lastDate, function(z) which.min(abs(x$date - z)))
# # isolatedHistoricPriceKrakken = historicPriceKrakken[minValue:maxValue,]
# # rm(historicPriceKrakken)
# # rm(x)
# # #------------------- Look up price/vol using cleaned date and writing to final dataframe----------------
# # pb <- txtProgressBar(min = 0, max = length(final$cleaned), style = 3)
# # for (i in 1:length(final$cleaned))
# # {
# #   setTxtProgressBar(pb, i)
# #   final$price_USD_BTC_Krakken[i] = isolatedHistoricPriceKrakken$price_USD_BTC_Krakken[sapply(final$cleaned[i], function(z) which.min(abs(as_datetime(isolatedHistoricPriceKrakken$date_Krakken) - z)))]
# #   #final$vol_Krakken[i] = isolatedHistoricPriceKrakken$vol_Krakken[sapply(final$cleaned[i], function(z) which.min(abs(as_datetime(isolatedHistoricPriceKrakken$date_Krakken) - z)))]
# #   final$date_Krakken[i] = isolatedHistoricPriceKrakken$date[sapply(final$cleaned[i], function(z) which.min(abs(as_datetime(isolatedHistoricPriceKrakken$date_Krakken) - z)))]
# #   
# # }
# # close(pb)
# # rm(isolatedHistoricPriceKrakken)
# # final$date_Krakken = as_datetime(final$date_Krakken)
# # #write.csv(final, "final_w_Krakken.csv")
# # 
# # z <- gzfile("cleaned_w_Krakken.csv.gz")
# # write.csv(final, z)
# 
# ##############################-----Coinbase PRICE LOOKUP-----------########################################
# 
# #------------> Download Coinbase File-------------
# # temp <- tempfile()
# # download.file("http://api.bitcoincharts.com/v1/csv/coinbaseUSD.csv.gz",temp)
# # historicPriceCoinbaseDL <- read.csv(gzfile(temp, ".coinbaseUSD"))
# # rm(temp)
# 
# #------------> Open Coinbase File-------------
# 
# historicPriceCoinbaseDL <- read.csv(".coinbaseUSD.csv")
# 
# historicPriceCoinbaseDL = historicPriceCoinbaseDL[!duplicated(strftime(as_datetime(historicPriceCoinbaseDL$X1417412036), '%Y-%m-%d %H:%M')),]
# 
# #------------> Begin Analysis ------------------
# 
# historicPriceCoinbase = historicPriceCoinbaseDL[1:nrow(historicPriceCoinbaseDL),]
# rm(historicPriceCoinbaseDL)
# colnames(historicPriceCoinbase)= c("date_Coinbase", "price_USD_BTC_Coinbase", "vol_Coinbase")
# #write.csv(historicPriceCoinbase, "historicPriceCoinbase.csv")
# #historicPriceCoinbase = read.csv("historicPriceCoinbase.csv")
# historicPriceCoinbase$date = as_datetime(as.numeric(historicPriceCoinbase$date))
# 
# 
# 
# #------------------- Isolate Key Dates and dump Historical Price
# 
# final = na.omit(final)  #---->set to the final output of the date cleaner
# 
# #----Selecting the first and last date from the final output of the date cleaner
# firstDate = final$cleaned[which.min(final$cleaned)]
# lastDate = final$cleaned[which.max(final$cleaned)]
# 
# 
# y = as.POSIXct(final$cleaned)
# x = structure(list(date = historicPriceCoinbase$date, Value = historicPriceCoinbase$`price USD`, class = "data.frame"))
# 
# minValue = sapply(firstDate, function(z) which.min(abs(x$date - z)))
# maxValue = sapply(lastDate, function(z) which.min(abs(x$date - z)))
# isolatedHistoricPriceCoinbase = historicPriceCoinbase[minValue:maxValue,]
# rm(historicPriceCoinbase)
# rm(x)
# 
# #------------------- Look up price/vol using cleaned date and writing to final dataframe----------------
# pb <- txtProgressBar(min = 0, max = length(final$cleaned), style = 3)
# for (i in 1:length(final$cleaned))
# {
#   setTxtProgressBar(pb, i)
#   final$price_USD_BTC_Coinbase[i] = isolatedHistoricPriceCoinbase$price_USD_BTC_Coinbase[sapply(final$cleaned[i], function(z) which.min(abs(as_datetime(isolatedHistoricPriceCoinbase$date_Coinbase) - z)))]
#   final$date_Coinbase[i] = isolatedHistoricPriceCoinbase$date_Coinbase[sapply(final$cleaned[i], function(z) which.min(abs(as_datetime(isolatedHistoricPriceCoinbase$date_Coinbase) - z)))]
# }
# close(pb)
# rm(isolatedHistoricPriceCoinbase)
# final$date_Coinbase = as_datetime(final$date_Coinbase)
# 
# ##############################-----Coinbase PRICE LOOKUP-----------########################################
# 
# #------------> Download Coinbase File-------------
# # temp <- tempfile()
# # download.file("http://api.bitcoincharts.com/v1/csv/coinbaseUSD.csv.gz",temp)
# # historicPriceCoinbaseDL <- read.csv(gzfile(temp, ".coinbaseUSD"))
# # rm(temp)
# 
# #------------> Open Coinbase File-------------
# 
# historicPriceCoinbaseDL <- read.csv(".coinbaseUSD.csv")
# 
# historicPriceCoinbaseDL = historicPriceCoinbaseDL[!duplicated(strftime(as_datetime(historicPriceCoinbaseDL$X1417412036), '%Y-%m-%d %H:%M')),]
# 
# #------------> Begin Analysis ------------------
# 
# historicPriceCoinbase = historicPriceCoinbaseDL[1:nrow(historicPriceCoinbaseDL),]
# rm(historicPriceCoinbaseDL)
# colnames(historicPriceCoinbase)= c("date_Coinbase", "price_USD_BTC_Coinbase", "vol_Coinbase")
# #write.csv(historicPriceCoinbase, "historicPriceCoinbase.csv")
# #historicPriceCoinbase = read.csv("historicPriceCoinbase.csv")
# historicPriceCoinbase$date = as_datetime(as.numeric(historicPriceCoinbase$date))







#---------------------> Archive -----------------------------------------------------------------------------
#----These are the articles this file can parse----------------
# blm 
# bcn 
# cd 
# cnbc
# cndk
# fbbtc
# fbsrch
# fr
# gf
# rba
# rbmine 
# reu
# scmp
# tw
# yn
# you
# zh
# wsj
# rbb 
# rbm
# rbc  
# bbc 
# gplus
# iet
# ccn 
# rbx
# frt 
