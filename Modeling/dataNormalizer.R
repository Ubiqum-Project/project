dirtyFrame = read.csv(gzfile("secondary_Predictor_Pull.csv.gz"))


dirtyFrame[,c("cleaned","KrakkenPrice")]

 dirtyFrame[,c("cleaned","KrakkenPrice",
"CoinbasePrice",
"gtrendLiteCrash",
"gtrendEtherCrash",
"gtrendEtherBubble",
"gtrendEther",
"gtrendBitcoinExchangeHack",
"gtrendBitcoinBounce",
"gtrendBitcoinBubble",
"gtrendBitcoin",
"GSPC.Adjusted",
"GSPC.Volume",
"GSPC.Close",
"GSPC.Low",
"GSPC.High",
"GSPC.Open",
"VIX.Adjusted",
"VIX.Close",
"VIX.Low",
"VIX.High",
"VIX.Open",
"GCJ18.CMX.Adjusted",
"GCJ18.CMX.Volume",
"GCJ18.CMX.Close",
"GCJ18.CMX.Low",
"GCJ18.CMX.High",
"GCJ18.CMX.Open",
"BTCUSD=X.Adjusted",
"BTCUSD=X.Close",
"BTCUSD=X.Low",
"BTCUSD=X.High",
"BTCUSD=X.Open")]
# "LTC-BTC.Adjusted",
# "LTC-BTC.Volume",
# "LTC-BTC.Close",
# "LTC-BTC.Low",
# "LTC-BTC.High",
# "LTC-BTC.Open",
# "BTC-ETH.Adjusted",
# "BTC-ETH.Volume",
# "BTC-ETH.Close",
# "BTC-ETH.Low",
# "BTC-ETH.High",
#"BTC-ETH.Open")]







