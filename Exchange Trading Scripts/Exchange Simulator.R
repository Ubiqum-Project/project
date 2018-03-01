
library(rgdax)


current = public_ticker(product_id = "BTC-USD")
bid = current$bid
ask = current$ask
price = current$price
vol = current$volume

date =as.character(current$time)
technical= 1
sentiment = 1

riskBTC = .1        #User defined value (basically percentage of wallet available for transaction)
riskUSD = .1        #User defined value (basically percentage of wallet available for transaction)

sentVtech = 5       #User defined value (the amount of weight each algo will receive: 1 = full sentiment, 9 = full technical, 5 = equal mix)



btcXaction = +.1
usdXaction = -1000

walletBTC = 5    +btcXaction   #amount of BTC in the wallet
walletUSD = 3000 + usdXaction    #amount of USD in the wallet

walletValue = price*walletBTC+walletUSD
delta = 0

transaction= data.frame(date, bid, ask, price, vol, technical, sentiment, sentVtech, riskBTC, riskUSD, btcXaction, usdXaction,walletBTC,walletUSD, walletValue, delta)

write.csv(transaction, file = "transactions.csv")
#----------------------------------------------------------

transactions = read.csv("transactions.csv")[,-1]
transactions$date = as.character(transactions$date)
transactions$walletValue= as.numeric(transactions$walletValue)


current = public_ticker(product_id = "BTC-USD")
bid = current$bid
ask = current$ask
price = current$price
vol = current$volume

date =as.character(current$time)

technical= .9  # Range of 1,.5, 0, -.5, -1  These Feed in from Algorithms
sentiment = .5 # Range of 1,.5, 0, -.5, -1   These Feed in from Algorithms

riskBTC = .01        #User defined value (basically percentage of wallet available for transaction) 
riskUSD = .03        #User defined value (basically percentage of wallet available for transaction)

sentVtech = 3       #User defined value (the amount of weight each algo will receive: 1 = full sentiment, 9 = full technical, 5 = equal mix)

#---------> Combines the user defined weights to identify a multiplier <------------------
sentVal = sentiment*((10- sentVtech)/10)
techVal = technical*(sentVtech/10)
combiVal = techVal+sentVal
combiVal

#---------> Calculates amount of wallet available to use <------------------
walletUSD =as.numeric(transactions$walletUSD[nrow(transactions)])
walletBTC =as.numeric(transactions$walletBTC[nrow(transactions)])

playUSD=walletUSD *riskUSD
playBTC=walletBTC*riskBTC


canBuy = playUSD/price 
canSell = playBTC*price
#----------> Calculates the appropriate move <---------------

if (combiVal == 0)
{
  print(paste("doing nothing",combiVal))
  walletBTC = walletBTC
  walletUSD = walletUSD
}else if(combiVal > 0)
{
 
  walletBTC = walletBTC+canBuy
  walletUSD = walletUSD-playUSD
  print(paste("buy that shit!  You bought",canBuy, "bitcoin for", playUSD, ".  Your bitcoin wallet is now:", walletBTC, "and your USD wallet is now", walletUSD))
  btcXaction = +canBuy
  usdXaction = -playUSD
  }else if(combiVal < 0)
{
 
  walletBTC = walletBTC-playBTC
  walletUSD =walletUSD +canSell
  btcXaction = -playBTC
  usdXaction = +canSell
  print(paste("sell that shit!  You sold",playBTC, "bitcoin for", canSell, ".  Your bitcoin wallet is now:", walletBTC, "and your USD wallet is now", walletUSD))
}

btcXaction
usdXaction



walletValue = (price*walletBTC)+walletUSD

delta = walletValue-as.numeric(transactions$walletValue[nrow(transactions)-1]) #change in wallet value from previous

transactions[nrow(transactions) + 1,] = c(date,  bid, ask, price, vol, technical, sentiment, sentVtech, riskBTC, riskUSD, btcXaction, usdXaction,walletBTC,walletUSD, walletValue, delta)

write.csv(transactions, file = "transactions.csv")
