library(PoloniexR)

poloniex.public <- PoloniexPublicAPI()
ticker.info     <- ReturnTicker(poloniex.public)
head(ticker.info)

pol = read.csv("auth.csv")

key    = as.character(pol$key[1])
secret = as.character(pol$secret[1])
poloniex.trading <- PoloniexTradingAPI(key    = key,
                                       secret = secret)
balances <- ReturnBalances(poloniex.trading)
head(balances)


