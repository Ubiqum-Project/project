library(readr)
library(tm)

cleaned <- read_csv("~/Desktop/bitcoinPull 2018-01-25")
cleaned.x <- cleaned[1:45000,]
docs <- Corpus(VectorSource(cleaned$title[1:45000]))
# docs.x <- Corpus(VectorSource(cleaned$title))
inspect(docs)

#convert various characters to spaces, to split up the words in the text
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x, fixed=TRUE))
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
docs <- tm_map(docs, removeWords, c("commentsharesavehidereport", "commentssharesavehidereportloading", "commentsharesavehidereportloading","commentssharesavehidereport","submitted", "reddit", "redditcom", "hour ago", "hours ago", "pictwittercom", "just", "now", "minutes", "ago")) 
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)

title.df <- data.frame(text = sapply(docs, paste, collapse = " "), stringsAsFactors = FALSE)
cleaned.x <- as.data.frame(cbind(cleaned.x,title.df))

#write.csv(cleaned.x, "cleanedX.csv")



##### text analysis ##### 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

findFreqTerms(dtm, lowfreq = 200)
findAssocs(dtm, terms = "game", corlimit = 0.3)
head(d, 10)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
