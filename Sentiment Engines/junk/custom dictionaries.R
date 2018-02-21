library(readr)
library(SentimentAnalysis)
library(tm)
library(sentimentr)

slangsd <- read_delim("Desktop/SlangSD.csv", 
                      ";", escape_double = FALSE, col_names = FALSE, 
                      trim_ws = TRUE)


slangsd.x1 <- as.matrix(slangsd$X1)
slangsd.x2 <- as.matrix(slangsd$X2)


slangsd.x1.c <- Corpus(VectorSource(slangsd.x1))
slangsd.x2.c <- Corpus(VectorSource(slangsd.x2))

# Generate dictionary
dict <- generateDictionary(slangsd.x1.c, slangsd.x2.c)
dict

summary(dict)

slangsd.x1.c <- c(slangsd$X1)
slangsd.x2.c <- c(slangsd$X2)

documents <- c("This is a good thing!",
               "This is a very good thing!",
               "This is okay.",
               "This is a bad thing.",
               "This is a very bad thing.")
response <- c(1, 0.5, 0, -0.5, -1)

dict2 <- generateDictionary(documents, response)

dict2

set.seed(10)
key <- data.frame(
        words = sample(slangsd$X1),
        polarity = rnorm(96461),
        stringsAsFactors = FALSE
)

mykey <- as_key(key)
is_key(key)

slangsd-sentiment <- sentiment_by(cleaned$text, polarity_dt = mykey)


