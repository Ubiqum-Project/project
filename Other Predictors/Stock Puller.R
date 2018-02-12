# Pull Market Data

library(quadmod)
SP500 <- getSymbols("^GSPC",auto.assign = FALSE, from = "2017-06-01")
BTCUSDX <- getSymbols("BTCUSD=X",auto.assign = FALSE, from = "2017-06-01")

btcUSD <- getSymbols("BTCUSD = X")

