library(readr)
library(tm)
library(tidyr)

cleaned <- read_csv("~/Desktop/bitcoinPull 2018-01-25")
# cleaned.x <- cleaned[1:100000,]
# cleaned.x$combination <- paste(cleaned.x$title, cleaned.x$paragraph)
cleaned$combination <- paste(cleaned$title, cleaned$paragraph)
#docs <- Corpus(VectorSource(cleaned.x$title))
docs <- Corpus(VectorSource(cleaned$combination))
#docs.x <- Corpus(VectorSource(cleaned.x$combination))
inspect(docs)

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
# docs <- tm_map(docs, removeNumbers)
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("commentsharesavehidereport", "commentssharesavehidereportloading", "commentsharesavehidereportloading","commentssharesavehidereport","submitted", "reddit", "redditcom", "hour ago", "hours ago", "pictwittercom", "just", "now", "minutes", "ago", "bitcoinallbot1","wsj", "pm","city","pic.twitter.com","http","https","wrap","live", "cointelegraph", "am","bloomberg","selfcryptocurrency","ireddit","selfbitcoinmining","selfbitcointmarkets","discussiondaily","selfbitcoinbayarea","bitcointallbotcommentsharesavehidereport","dailycommentsharesavehidereport", "usethebitcoincom","andix3commentsharesavehidereport", "selfbitcoinmarkets","bitcoinallbotcommentsharesavehidereport", "newsbtc","read","cnbc","video","cryptodailyuk","coindoocom")) 
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)

inspect(docs)

text.df <- data.frame(text = sapply(docs, paste, collapse = " "), stringsAsFactors = FALSE)
cleaned <- as.data.frame(cbind(cleaned,text.df))

#write.csv(cleaned, "cleaned.csv")
