# Pull Market Data

library(quantmod)
library(data.table)

pullDateStart ="2017-11-28"

SP500 <- setDT(as.data.frame(getSymbols("^GSPC",auto.assign = FALSE, from = pullDateStart, to= Sys.Date())), keep.rownames = TRUE)[]
colnames(SP500)[1] ="date"

BTCUSDX <- setDT(as.data.frame(getSymbols("BTCUSD=X",auto.assign = FALSE, from = pullDateStart, to= Sys.Date())), keep.rownames = TRUE)[]
colnames(BTCUSDX)[1] ="date"

VIX <- setDT(as.data.frame(getSymbols("^VIX",auto.assign = FALSE, from = pullDateStart, to= Sys.Date())), keep.rownames = TRUE)[]
colnames(VIX)[1] ="date"

GOLD <- setDT(as.data.frame(getSymbols("GCJ18.CMX",auto.assign = FALSE, from = pullDateStart, to= Sys.Date())), keep.rownames = TRUE)[]
colnames(GOLD)[1] ="date"

BTC_ETH <- setDT(as.data.frame(getSymbols("BTC-ETH",auto.assign = FALSE, from = pullDateStart, to= Sys.Date())), keep.rownames = TRUE)[]
colnames(BTC_ETH)[1] ="date"

BTC_LTC <- setDT(as.data.frame(getSymbols("LTC-BTC",auto.assign = FALSE, from = pullDateStart, to= Sys.Date())), keep.rownames = TRUE)[]
colnames(BTC_LTC)[1] ="date"




out = ( SP500[c(VIX), on = c("date")])
out = ( out[c(GOLD), on = c("date")])
out = ( out[c(BTCUSDX), on = c("date")])
out = ( out[c(BTC_LTC), on = c("date")])
out = ( out[c(BTC_ETH), on = c("date")])

out = na.locf(out)

