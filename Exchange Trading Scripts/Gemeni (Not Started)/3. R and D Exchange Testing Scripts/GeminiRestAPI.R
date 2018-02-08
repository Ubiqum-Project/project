# Gemeni Trading Script

library(httr)
library(readr)
library(base64enc)
library(openssl)
library(digest)

gemeni = read_csv("auth.csv")[3,] #---> This reads the Gemeni key and secret from my local auth.csv.  

key = as.character(gemeni[3]) #---> This reads the Gemeni key from my local auth.csv.  
secret = as.character(gemeni [4])#---> This reads the Gemeni secret from my local auth.csv.  

'request' = "/v1/order/status"
'nonce'= 123456
'order_id'= 18834

to_encode = paste("request:",request, "nonce:",nonce, "order_id:", order_id)
to_encode
b64 = base64encode(charToRaw(to_encode))

rawToChar(base64decode("ewogICAgInJlcXVlc3QiOiAiL3YxL29yZGVyL3N
    0YXR1cyIsCiAgICAibm9uY2UiOiAxMjM0NTYsCgogICAgIm9yZGV
                       yX2lkIjogMTg4MzQKfQo="))
signature = sha384(b64, key = "1234abcd")
signature
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

