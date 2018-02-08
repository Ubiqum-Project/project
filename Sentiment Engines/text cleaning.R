library(readr)
library(tm)
library(tidyr)

cleaned <- read_csv("~/Desktop/bitcoinPull 2018-01-25")
cleaned <- cleaned[ grep("year", cleaned$article_time, invert = TRUE) , ]
cleaned <- cleaned[ grep("years", cleaned$article_time, invert = TRUE) , ] 
cleaned <- cleaned[ grep("months", cleaned$article_time, invert = TRUE) , ]

cleaned$combination <- paste(cleaned$title, cleaned$paragraph)
docs <- Corpus(VectorSource(cleaned$combination))

# cleaned.x <- cleaned[1:100000,]
# cleaned.x$combination <- paste(cleaned.x$title, cleaned.x$paragraph)
#docs.x <- Corpus(VectorSource(cleaned.x$combination))

#convert various characters to spaces, to split up the words in the text
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x, fixed=TRUE))
docs <- tm_map(docs, toSpace, "/r/btc")
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

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers (not required?)
docs <- tm_map(docs, removeNumbers)
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("commentsharesavehidereport", "commentssharesavehidereportloading", "commentsharesavehidereportloading","commentssharesavehidereport","submitted", "reddit", "redditcom", "hour ago", "hours ago", "pictwittercom", "just", "now", "minutes", "ago", "bitcoinallbot1","wsj", "pm","city","pic.twitter.com","http","https","wrap","live", "cointelegraph", "am","bloomberg","selfcryptocurrency","ireddit","selfbitcoinmining","selfbitcointmarkets","discussiondaily","selfbitcoinbayarea","bitcointallbotcommentsharesavehidereport","dailycommentsharesavehidereport", "usethebitcoincom","andix3commentsharesavehidereport", "selfbitcoinmarkets","bitcoinallbotcommentsharesavehidereport", "newsbtc","read","cnbc","video","cryptodailycouk","coindoocom","andixcommentsharesavehidereport","selfbitcoinxt","lhicocommentsharesavehidereport","bitcoinallbot","iimgurcom", "minute", "months", "month","day","days")) 
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)

#put text back into cleaned df
text.df <- data.frame(text = sapply(docs, paste, collapse = " "), stringsAsFactors = FALSE)
cleaned <- as.data.frame(cbind(cleaned,text.df))

#removes duplicates
dupes <- which(duplicated(cleaned$text))
cleaned <- cleaned[-dupes, ]

# cleaned.puerto <- cleaned[ grep("threat global", cleaned$text, invert = FALSE) , ]
# write_csv(cleaned.puerto, "puerto.csv")


####### further cleanup ####### 
#input values for missings
cleaned$price_gf_delta_btc <- na.replace(cleaned$price_gf_delta_btc, 0)
cleaned$title <- na.replace(cleaned$title, "x")
cleaned$paragraph <- na.replace(cleaned$paragraph, "x")
cleaned$article_time <- na.replace(cleaned$article_time, "x")
cleaned$time_downloaded_gmt[is.na(cleaned$time_downloaded_gmt)] <- cleaned$time_now_gmt[is.na(cleaned$time_downloaded_gmt)]

#sapply(df.s.t, function(x) sum(is.na(x)))

write.csv(cleaned, "cleaned.csv")
