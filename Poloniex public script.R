library(jsonlite)
library(httr)
library(digest)
library(xts)
library(dygraphs)
library(magrittr)
library("data.table")
library(quantmod)

api.poloniex2 <- function() {
  
  month_ago <- seq(Sys.Date(), length = 2, by = "-1 months")[2]
  start_time <- as.numeric(as.POSIXct(month_ago))
  
  api_url <- paste("https://poloniex.com/public?command=returnChartData&currencyPair=BTC_XRP&start=",
                   start_time,
                   "&end=9999999999&period=14400",
                   sep='')
  
  ret <- GET(api_url)
  stop_for_status(ret)
  content(ret)
}

data  <- api.poloniex2()

newlist <- rbindlist(data, fill=TRUE)

zzz <- xts(apply(newlist[,2:6], 2, as.numeric), as.POSIXct(newlist$date,origin="1970-01-01"))
yyy <- as.quantmod.OHLC(zzz,col.names = c("High","Low","Open","Close","Volume"))

chartSeries(yyy, theme="white")

addSMA()
addMACD()