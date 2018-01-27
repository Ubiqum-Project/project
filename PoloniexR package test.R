library(PoloniexR)

poloniex.public <- PoloniexPublicAPI()
ticker.info     <- ReturnTicker(poloniex.public)
head(ticker.info)


key    = ""
secret = ""
poloniex.trading <- PoloniexTradingAPI(key    = key,
                                       secret = secret)
balances <- ReturnBalances(poloniex.trading)
head(balances)