# Gemeni Trading Script

library(httr)
library(readr)
library(base64enc)
library(openssl)
library(digest)
library(magrittr)
library(jsonlite)
library(RCurl)

gemeni = read_csv("auth.csv")[3,] #---> This reads the Gemeni key and secret from my local auth.csv.  

key = as.character(gemeni[3]) #---> This reads the Gemeni key from my local auth.csv.  
secret = as.character(gemeni [4])#---> This reads the Gemeni secret from my local auth.csv.  

request = "/v1/balances"
nonce= round(as.numeric(Sys.time()) * 1000, 0)
# 'order_id'= 18834

to_encode = paste("request:",request, "nonce:",nonce)
to_encode
b64 = base64_encode(charToRaw(to_encode))

rawToChar(base64decode("ewogICAgInJlcXVlc3QiOiAiL3YxL29yZGVyL3N
    0YXR1cyIsCiAgICAibm9uY2UiOiAxMjM0NTYsCgogICAgIm9yZGV
                       yX2lkIjogMTg4MzQKfQo="))
signature = sha384(b64, key = secret)
signature
headers = c(
  'Content-Type'= "text/plain",
  'Content-Length'= "0",
 'X-GEMINI-API-KEY' = key,
  'X-GEMINI-PAYLOAD'= b64,
  'X-GEMINI-SIGNATURE'= signature,
  'Cache-Control'= "no-cache"
)


headers = c(
  paste('Content-Type:',"text/plain"),
  paste('Content-Length:',"0"),
  paste('X-GEMINI-API-KEY:',key),
  paste('X-GEMINI-PAYLOAD:',b64),
  paste('X-GEMINI-SIGNATURE:',signature),
  paste( 'Cache-Control:',"no-cache")
)

url = "https://api.gemini.com/v1/balances"

r <- POST(url, add_headers(headers = headers)) 
r
r$date

#--------------------------------------------------------------------------------
#   Set variable for the gemini api URL
geminiHost <- "https://api.gemini.com"
#   Set variable for the gemini endpoint
geminiEndpoint <- "/v1/heartbeat"
#   Create nonce parameter
currentTimeNonce <- round(as.numeric(Sys.time()) * 1000, 0)
#   Create JSON payload
payload <-
  toJSON(data.frame(request = geminiEndpoint, nonce = currentTimeNonce), pretty = TRUE) %>%
  gsub("\\[|\\]", "", .)
payload
#   Convert payload to base64
payloadBase64Enc <- base64_encode(payload)
payloadBase64Enc
#   Create signature
signatureString <- sha384(payloadBase64Enc, key = secret)
#   Construct the complete URL
completeURL <- paste0(geminiHost, geminiEndpoint)
#   Create header
hdr = c(
  "Content-Type" = "text/plain",
  "Content-Length" = 0,
  "Cache-Control" = "no-cache",
  "X-GEMINI-APIKEY" = key,
  "X-GEMINI-PAYLOAD" = payloadBase64Enc,
  "X-GEMINI-SIGNATURE" = signatureString
)
#   Request API using the complete URL and the required headers
mytradesAPIResult <- fromJSON(httpPOST(completeURL,
                                       httpheader = hdr,
                                       verbose = TRUE))


#--------------------------------------------------------------------------------

#   Set variable for the gemini api URL
geminiHost <- "https://api.gemini.com"
#   Set variable for the gemini endpoint
geminiEndpoint <- "/v1/balances"
#   Create the symbol parameter
symbol <- 'btcusd'
#   Create nonce parameter
currentTimeNonce <- round(as.numeric(Sys.time()) * 1000, 0)
#   Create JSON payload
payload <-
  toJSON(data.frame(
    request = geminiEndpoint,
    nonce = currentTimeNonce
   # symbol = symbol
  )) %>% gsub("\\[|\\]", "", .)
#   Convert payload to base64
payloadBase64Enc <- base64_enc(payload)
payloadBase64Enc
#   Create signature
signatureString <- sha384(payloadBase64Enc, key = secret)
#   Construct the complete URL
completeURL <- paste0(geminiHost, geminiEndpoint)
#   Create header
hdr = c(
  "Content-Type" = "text/plain",
  "Content-Length" = "0",
  "Cache-Control" = "no-cache",
  "X-GEMINI-APIKEY" = key,
  "X-GEMINI-PAYLOAD" = payloadBase64Enc,
  "X-GEMINI-SIGNATURE" = signatureString
)
#   Request API using the complete URL and the required headers
mytradesAPIResult <- fromJSON(httpPOST(completeURL,
                                       httpheader = hdr,
                                       verbose = TRUE))

#------------------> Public Get <---------------------------
library(lubridate)
library(jsonlite)
gemeniPublicGetQuote = "https://api.gemini.com/v1/pubticker/btcusd"
latestTickerBTC = fromJSON(gemeniPublicGetQuote)
gemeniBid = latestTickerBTC$bid
gemeniAsk = latestTickerBTC$ask
gemeniVolBTC = latestTickerBTC$volume$BTC
gemeniVolUSD = latestTickerBTC$volume$USD
gemeniTime = as_datetime(latestTickerBTC$volume$timestamp/1000)
gemeniLast = latestTickerBTC$last


