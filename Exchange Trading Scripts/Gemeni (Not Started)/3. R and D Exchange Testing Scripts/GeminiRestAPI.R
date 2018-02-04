# Gemeni Trading Script

library(httr)
library(readr)
library(base64enc)
library(openssl)

gemeni = read_csv("auth.csv")[3,] #---> This reads the Gemeni key and secret from my local auth.csv.  

key = as.character(gemeni[3]) #---> This reads the Gemeni key from my local auth.csv.  
secret = as.character(gemeni [4])#---> This reads the Gemeni secret from my local auth.csv.  

request = "/v1/balances"
nonce= Sys.time()

to_encode = paste(request, nonce)

b64 = base64encode(charToRaw(to_encode))




signature = sha384(b64, key = secret)

headers = c(
  'Content-Type'= "text/plain",
  'Content-Length'= "0",
 'X-GEMINI-API-KEY' = key,
  'X-GEMINI-PAYLOAD'= b64,
  'X-GEMINI-SIGNATURE'= signature,
  'Cache-Control'= "no-cache"
)

url = "https://api.gemini.com/v1/balances"

r <- POST(url, add_headers(headers = headers)) 

