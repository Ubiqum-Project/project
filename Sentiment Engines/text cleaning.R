library(readr)
library(tm)
library(tidyr)
library(gtools)


cleaned <- read.csv("~/Desktop/final5.csv")
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

#write csv
# remove.columns <- c("INSERT YOUR OWN HERE", "X", "X1", "text.1", "articleTime", "title", "paragraph" ,
#                     "source" , "price_gfbtc" , "api_bid_btc", "price_gf_delta_btc" ,"api_last_btc" ,
#                     "api_vol_btc" , "api_vol_btc", "DateNotes", "datez")
# cleaned = cleaned[,!(names(cleaned) %in% remove.columns)]
# write_csv(cleaned, "cleanedx.csv")
