#

#########################################---Beginning of Secondary Predictor Puller-------################################## 

library(quantmod)
library(lubridate)
library(data.table)
#install.packages("curl")
library(curl)
#devtools::install_github('diplodata/gtrendsR')
library(gtrendsR)



#####################################################################################################
############################--------> BEGIN PRICE LOOKUP FUNCTION <--------###########################


final.test = read_csv(gzfile("FINAL.csv.gz"))
final$cleaned = as_datetime(final$cleaned)


##############################-----KRAKKEN PRICE LOOKUP-----------########################################
#---------------------Access Kraken Historical Price Info

#---------> Download file 
# temp <- tempfile()
# download.file("http://api.bitcoincharts.com/v1/csv/krakenUSD.csv.gz",temp)
# historicPriceKrakkenDL <- read.csv(gzfile(temp, ".krakenUSD.csv"))
# # rm(temp)

#---------> Or Read file 

historicPriceKrakkenDL <- read.csv(".krakenUSD.csv", skip =4580000 )
historicPriceKrakkenDL = historicPriceKrakkenDL[1:2]
colnames(historicPriceKrakkenDL) = c("krakkenDate", "KrakkenPrice")

#---------> End File Input 

final$cleaned = as_datetime(final$cleaned)
historicPriceKrakkenDL$krakkenDate = as_datetime(historicPriceKrakkenDL$krakkenDate)


final = as.data.table(final)
historicPriceKrakkenDL= as.data.table(historicPriceKrakkenDL)

final[, join_time:=cleaned]
historicPriceKrakkenDL[, join_time:=krakkenDate]


setkey(as.data.table(final), cleaned, join_time)
setkey(as.data.table(historicPriceKrakkenDL), krakkenDate, join_time)

testKrakken = unique(historicPriceKrakkenDL[final, roll = T, on = "join_time"])

testedKrakken = testKrakken[!duplicated(testKrakken$text),]

colnames(testedKrakken)[1] = "KrakkenDate" #--> For Quality Control Purposes
colnames(testedKrakken)[2] = "KrakkenPrice"

testedKrakken = testedKrakken[,-c(3,4)]

final = testedKrakken


#############################--->Coinbase<------####################################
#---------------------Access Coinbase Historical Price Info

#------------> Download Coinbase File-------------
# temp <- tempfile()
# download.file("http://api.bitcoincharts.com/v1/csv/coinbaseUSD.csv.gz",temp)
# historicPriceCoinbaseDL <- read.csv(gzfile(temp, ".coinbaseUSD"))
# rm(temp)

#------------> Open Coinbase File-------------


historicPriceCoinbaseDL <- read.csv(".coinbaseUSD.csv", skip =24000000)
colnames(historicPriceCoinbaseDL) = c("coinbaseDate", "coinbasePrice")
historicPriceCoinbaseDL = historicPriceCoinbaseDL[1:2]
#---------> End File Input 

final$cleaned = as_datetime(final$cleaned)
historicPriceCoinbaseDL$coinbaseDate = as_datetime(historicPriceCoinbaseDL$coinbaseDate)


final = as.data.table(final)
historicPriceCoinbaseDL= as.data.table(historicPriceCoinbaseDL)

final[, join_time:=cleaned]
historicPriceCoinbaseDL[, join_time:=coinbaseDate]


setkey(as.data.table(final), cleaned, join_time)
setkey(as.data.table(historicPriceCoinbaseDL), coinbaseDate, join_time)

testCoin = unique(historicPriceCoinbaseDL[final, roll = T, on = "join_time"])

testedCoin = testCoin[!duplicated(testCoin$text),]

colnames(testedCoin)[1] = "CoinbaseDate" #--> For Quality Control Purposes
colnames(testedCoin)[2] = "CoinbasePrice"

testedCoin = testedCoin[,-c(3)]

final = testedCoin
#############################--->CEX<------####################################
#---------------------Access CEX Historical Price Info

#------------> Download CEX File-------------
# temp <- tempfile()
# download.file("http://api.bitcoincharts.com/v1/csv/cexUSD.csv.gz",temp)
# historicPriceCEXDL <- read.csv(gzfile(temp, ".coinbaseUSD"))
# rm(temp)

# #------------> Open CEX File-------------
# 
# historicPriceCEXDL <- read.csv(".cexUSD.csv")
# colnames(historicPriceCEXDL) = c("cexDate", "cexPrice")
# historicPriceCEXDL = historicPriceCEXDL[1:2]
# #---------> End File Input 
# 
# final$cleaned = as_datetime(final$cleaned)
# historicPriceCEXDL$cexDate = as_datetime(historicPriceCEXDL$cexDate)
# 
# 
# final = as.data.table(final)
# historicPriceCEXDL= as.data.table(historicPriceCEXDL)
# 
# final[, join_time:=cleaned]
# historicPriceCEXDL[, join_time:=cexDate]
# 
# 
# setkey(as.data.table(final), cleaned, join_time)
# setkey(as.data.table(historicPriceCEXDL), cexDate, join_time)
# 
# testCex = unique(historicPriceCEXDL[final, roll = T, on = "join_time"])
# 
# testedCex = testCex[!duplicated(testCex$combination),]
# 
# colnames(testedCex)[1] = "cexDate" #--> For Quality Control Purposes
# colnames(testedCex)[2] = "cexPrice"
# 
# testedCex = testedCex[,-c(3)]
# 
# final = testedCex
# z <- gzfile("cleaned_w_Krakken_Coinbase_Cex.csv.gz")
# write.csv(final, z)

####################################################################################

# Import the first and last date from the cleaned dates on our cleaned data frame
cleanedData = final #read.csv(gzfile("cleaned_w_Krakken_Coinbase_Cex.csv.gz"))
pullDateStart = strftime(as.character(min(as_datetime(cleanedData$cleaned))), '%Y-%m-%d')
pullDateEnd = strftime(as.character((max(as_datetime(cleanedData$cleaned)))), '%Y-%m-%d')


# Begin Secondary Predictor Collection
SP500 <- setDT(as.data.frame(getSymbols("^GSPC",auto.assign = FALSE, from = pullDateStart, to= pullDateEnd)), keep.rownames = TRUE)[]
colnames(SP500)[1] ="date"
colnames(SP500)[2] ="GSPC_Open"
colnames(SP500)[3] ="GSPC_High"
colnames(SP500)[4] ="GSPC_Low"
colnames(SP500)[5] ="GSPC_Close"
colnames(SP500)[6] ="GSPC_Volume"
colnames(SP500)[7] ="GSPC_Adjusted"

BTCUSDX <- setDT(as.data.frame(getSymbols("BTCUSD=X",auto.assign = FALSE, from = pullDateStart, to= pullDateEnd)), keep.rownames = TRUE)[]
colnames(BTCUSDX)[1] ="date"
colnames(BTCUSDX)[2] ="BTCUSDX_Open"
colnames(BTCUSDX)[3] ="BTCUSDX_High"
colnames(BTCUSDX)[4] ="BTCUSDX_Low"
colnames(BTCUSDX)[5] ="BTCUSDX_Close"
colnames(BTCUSDX)[6] ="BTCUSDX_Volume"
colnames(BTCUSDX)[7] ="BTCUSDX_Adjusted"

VIX <- setDT(as.data.frame(getSymbols("^VIX",auto.assign = FALSE, from = pullDateStart, to= pullDateEnd)), keep.rownames = TRUE)[]

colnames(VIX)[1] ="date"
colnames(VIX)[2] ="VIX_Open"
colnames(VIX)[3] ="VIX_High"
colnames(VIX)[4] ="VIX_Low"
colnames(VIX)[5] ="VIX_Close"
colnames(VIX)[6] ="VIX_Volume"
colnames(VIX)[7] ="VIX_Adjusted"

GOLD <- setDT(as.data.frame(getSymbols("GCJ18.CMX",auto.assign = FALSE, from = pullDateStart, to= pullDateEnd)), keep.rownames = TRUE)[]

colnames(GOLD)[1] ="date"
colnames(GOLD)[2] ="GOLD_Open"
colnames(GOLD)[3] ="GOLD_High"
colnames(GOLD)[4] ="GOLD_Low"
colnames(GOLD)[5] ="GOLD_Close"
colnames(GOLD)[6] ="GOLD_Volume"
colnames(GOLD)[7] ="GOLD_Adjusted"

BTC_ETH <- setDT(as.data.frame(getSymbols("BTC-ETH",auto.assign = FALSE, from = pullDateStart, to= pullDateEnd)), keep.rownames = TRUE)[]
colnames(BTC_ETH)[1] ="date"

colnames(BTC_ETH)[1] ="date"
colnames(BTC_ETH)[2] ="BTC_ETH_Open"
colnames(BTC_ETH)[3] ="BTC_ETH_High"
colnames(BTC_ETH)[4] ="BTC_ETH_Low"
colnames(BTC_ETH)[5] ="BTC_ETH_Close"
colnames(BTC_ETH)[6] ="BTC_ETH_Volume"
colnames(BTC_ETH)[7] ="BTC_ETH_Adjusted"

BTC_LTC <- setDT(as.data.frame(getSymbols("LTC-BTC",auto.assign = FALSE, from = pullDateStart, to= pullDateEnd)), keep.rownames = TRUE)[]
colnames(BTC_LTC)[1] ="date"
colnames(BTC_LTC)[2] ="BTC_LTC_Open"
colnames(BTC_LTC)[3] ="BTC_LTC_High"
colnames(BTC_LTC)[4] ="BTC_LTC_Low"
colnames(BTC_LTC)[5] ="BTC_LTC_Close"
colnames(BTC_LTC)[6] ="BTC_LTC_Volume"
colnames(BTC_LTC)[7] ="BTC_LTC_Adjusted"

#--> Collect Google Trends

location = c("")  #---> See Below for country codes for Google Trends...Code only accepts one for now

gtrendBitcoin = gtrends("bitcoin", geo = location, time = paste0(pullDateStart," ",pullDateEnd), gprop = c("web", "news",
                                                         "images", "froogle", "youtube"), category = 0, hl = "en-US")
gtrendBitcoin = as.data.frame(gtrendBitcoin$interest_over_time[,c(1,2)])

gtrendBitcoin$date = as.character(gtrendBitcoin$date)
colnames(gtrendBitcoin)[2] ="gtrendBitcoin"
gtrendBitcoin= setDT(gtrendBitcoin)

Sys.sleep(sample(1:3))

gtrendBitcoinBubble = gtrends("bitcoin bubble", geo = location, time = paste0(pullDateStart," ",pullDateEnd), gprop = c("web", "news",
                                                                                                           "images", "froogle", "youtube"), category = 0, hl = "en-US")
gtrendBitcoinBubble =as.data.frame( gtrendBitcoinBubble$interest_over_time[,c(1,2)])
gtrendBitcoinBubble$date = as.character(gtrendBitcoinBubble$date)
colnames(gtrendBitcoinBubble)[2] ="gtrendBitcoinBubble"
gtrendBitcoinBubble= setDT(gtrendBitcoinBubble)
Sys.sleep(sample(1:3))

gtrendBitcoinCrash = gtrends("bitcoin crash", geo = location, time = paste0(pullDateStart," ",pullDateEnd), gprop = c("web", "news",
                                                                                                                        "images", "froogle", "youtube"), category = 0, hl = "en-US")
gtrendBitcoinCrash = as.data.frame(gtrendBitcoinCrash$interest_over_time[,c(1,2)])
gtrendBitcoinCrash$date = as.character(gtrendBitcoinCrash$date)
colnames(gtrendBitcoinCrash)[2] ="gtrendBitcoinCrash"
gtrendBitcoinCrash= setDT(gtrendBitcoinCrash)
Sys.sleep(sample(1:3))

gtrendBitcoinPrice = gtrends("bitcoin price", geo = location, time = paste0(pullDateStart," ",pullDateEnd), gprop = c("web", "news",
                                                                                                                      "images", "froogle", "youtube"), category = 0, hl = "en-US")
gtrendBitcoinPrice = as.data.frame(gtrendBitcoinPrice$interest_over_time[,c(1,2)])
gtrendBitcoinPrice$date = as.character(gtrendBitcoinPrice$date)
colnames(gtrendBitcoinPrice)[2] ="gtrendBitcoinPrice"
gtrendBitcoinPrice= setDT(gtrendBitcoinPrice)
Sys.sleep(sample(1:3))


gtrendBitcoinBan = gtrends("bitcoin ban", geo = location, time = paste0(pullDateStart," ",pullDateEnd), gprop = c("web", "news",
                                                                                                                      "images", "froogle", "youtube"), category = 0, hl = "en-US")
gtrendBitcoinBan = as.data.frame(gtrendBitcoinBan$interest_over_time[,c(1,2)])
gtrendBitcoinBan$date = as.character(gtrendBitcoinBan$date)
colnames(gtrendBitcoinBan)[2] ="gtrendBitcoinBan"
gtrendBitcoinBan= setDT(gtrendBitcoinBan)
Sys.sleep(sample(1:3))


gtrendBitcoinTether = gtrends("bitcoin tether", geo = location, time = paste0(pullDateStart," ",pullDateEnd), gprop = c("web", "news",
                                                                                                                                   "images", "froogle", "youtube"), category = 0, hl = "en-US")
gtrendBitcoinTether = as.data.frame(gtrendBitcoinTether$interest_over_time[,c(1,2)])
gtrendBitcoinTether$date = as.character(gtrendBitcoinTether$date)
colnames(gtrendBitcoinTether)[2] ="gtrendBitcoinTether"
gtrendBitcoinTether= setDT(gtrendBitcoinTether)
Sys.sleep(sample(1:3))

gtrendEther = gtrends("ethereum", geo = location, time = paste0(pullDateStart," ",pullDateEnd), gprop = c("web", "news",
                                                                                                                                     "images", "froogle", "youtube"), category = 0, hl = "en-US")
gtrendEther = as.data.frame(gtrendEther$interest_over_time[,c(1,2)])
gtrendEther$date = as.character(gtrendEther$date)
colnames(gtrendEther)[2] ="gtrendEther"
gtrendEther= setDT(gtrendEther)
Sys.sleep(sample(1:3))

gtrendEtherCrash = gtrends("ethereum crash", geo = location, time = paste0(pullDateStart," ",pullDateEnd), gprop = c("web", "news",
                                                                                                          "images", "froogle", "youtube"), category = 0, hl = "en-US")
gtrendEtherCrash = as.data.frame(gtrendEtherCrash$interest_over_time[,c(1,2)])
gtrendEtherCrash$date = as.character(gtrendEtherCrash$date)
colnames(gtrendEtherCrash)[2] ="gtrendEtherCrash"
gtrendEtherCrash= setDT(gtrendEtherCrash)
Sys.sleep(sample(1:3))

gtrendEtherBubble = gtrends("ethereum bubble", geo = location, time = paste0(pullDateStart," ",pullDateEnd), gprop = c("web", "news",
                                                                                                                       "images", "froogle", "youtube"), category = 0, hl = "en-US")
gtrendEtherBubble = as.data.frame(gtrendEtherBubble$interest_over_time[,c(1,2)])
gtrendEtherBubble$date = as.character(gtrendEtherBubble$date)
colnames(gtrendEtherBubble)[2] ="gtrendEtherBubble"
gtrendEtherBubble= setDT(gtrendEtherBubble)
Sys.sleep(sample(1:3))

gtrendLite = gtrends("litecoin", geo = location, time = paste0(pullDateStart," ",pullDateEnd), gprop = c("web", "news",
                                                                                                          "images", "froogle", "youtube"), category = 0, hl = "en-US")
gtrendLite = as.data.frame(gtrendLite$interest_over_time[,c(1,2)])
gtrendLite$date = as.character(gtrendLite$date)
colnames(gtrendLite)[2] ="gtrendLite"
gtrendLite= setDT(gtrendLite)
Sys.sleep(sample(1:3))

gtrendLiteCrash = gtrends("litecoin crash", geo = location, time = paste0(pullDateStart," ",pullDateEnd), gprop = c("web", "news",
                                                                                                                     "images", "froogle", "youtube"), category = 0, hl = "en-US")
gtrendLiteCrash = as.data.frame(gtrendLiteCrash$interest_over_time[,c(1,2)])
gtrendLiteCrash$date = as.character(gtrendLiteCrash$date)
colnames(gtrendLiteCrash)[2] ="gtrendLiteCrash"
gtrendLiteCrash= setDT(gtrendLiteCrash)

# Begin merging gTrends into a single dataframe

gOut =  gtrendBitcoin[gtrendBitcoinBubble, on = "date"]
gOut = gOut[gtrendBitcoinPrice, on = "date"]
gOut = gOut[gtrendBitcoinTether, on = "date"]
gOut = gOut[gtrendEther, on = "date"]
gOut = gOut[gtrendEtherBubble, on = "date"]
gOut = gOut[gtrendEtherCrash, on = "date"]
gOut = gOut[gtrendLite, on = "date"]
gOut = gOut[gtrendLiteCrash, on = "date"]
gOut = gOut[gtrendBitcoinBan, on = "date"]

gOut$date <- as.character(as.Date(gOut$date))

# Begin merging gTrends and Stocks into a single dataframe

out = gOut[FALSE,]
out = ( SP500[c(gOut), on = c("date")])
out =  ( VIX[c(out), on = c("date")])
out = ( GOLD[c(out), on = c("date")])
out = ( BTCUSDX[c(out), on = c("date")])
out = ( BTC_LTC[c(out), on = c("date")])
out = ( BTC_ETH[c(out), on = c("date")])

# Last Value Carry Forward (assign friday values to saturday and sunday)
concatenated_Secondary_Predictors = na.locf(out)

# Clean the workspace
#rm(out, SP500, GOLD, BTCUSDX, BTC_ETH, BTC_LTC, VIX, concatenated_Secondary_Predictors, z, pullDateStart, pullDateEnd)

cleanedWithExchangePrice = cleanedData

#bkup = cleanedWithExchangePrice

#cleanedWithExchangePrice = bkup
cleanedWithExchangePrice$cleaned = as_datetime(cleanedWithExchangePrice$cleaned)
concatenated_Secondary_Predictors$date = as_datetime(concatenated_Secondary_Predictors$date)


cleanedWithExchangePrice = as.data.table(cleanedWithExchangePrice)
concatenated_Secondary_Predictors= as.data.table(concatenated_Secondary_Predictors)

cleanedWithExchangePrice[, join_time:=cleaned]
concatenated_Secondary_Predictors[, join_time:=date]


setkey(as.data.table(cleanedWithExchangePrice), cleaned, join_time)
setkey(as.data.table(concatenated_Secondary_Predictors), date, join_time)

test = concatenated_Secondary_Predictors[cleanedWithExchangePrice, roll = T, on = "join_time"]
tester = test[!duplicated(test$text),]

# tester = final =  tester[,c("cleaned",
#                                 "name",
#                                 "combination",
#                                 "text",
#                                 "KrakkenPrice",
#                                 "CoinbasePrice",
#                                 "gtrendLiteCrash",
#                                 "gtrendEtherCrash",
#                                 "gtrendEtherBubble",
#                                 "gtrendEther",
#                                 "gtrendBitcoinPrice",
#                                 "gtrendBitcoinTether",
#                                 "gtrendBitcoinBubble",
#                                 "gtrendBitcoin",
#                                 "GSPC_Adjusted",
#                                 "GSPC_Volume",
#                                 "GSPC_Close",
#                                 "GSPC_Low",
#                                 "GSPC_High",
#                                 "GSPC_Open",
#                                 "VIX_Adjusted",
#                                 "VIX_Close",
#                                 "VIX_Low",
#                                 "VIX_High",
#                                 "VIX_Open",
#                                 "GOLD_Adjusted",
#                                 "GOLD_Volume",
#                                 "GOLD_Close",
#                                 "GOLD_Low",
#                                 "GOLD_High",
#                                 "GOLD_Open",
#                                 "BTCUSDX_Adjusted",
#                                 "BTCUSDX_Volume", 
#                                 "BTCUSDX_Close",
#                                 "BTCUSDX_Low",
#                                 "BTCUSDX_High",
#                                 "BTCUSDX_Open",
#                                 "BTC_LTC_Adjusted",
#                                 "BTC_LTC_Volume",
#                                 "BTC_LTC_Close",
#                                 "BTC_LTC_Low",
#                                 "BTC_LTC_High",
#                                 "BTC_LTC_Open",
#                                 "BTC_ETH_Adjusted",
#                                 "BTC_ETH_Volume",
#                                 "BTC_ETH_Close",
#                                 "BTC_ETH_Low",
#                                 "BTC_ETH_High",
#                                 "BTC_ETH_Open")]

# Write to GZ file
z <- gzfile("secondary_Predictor_Pull.csv.gz")
write.csv(tester, z)

################################## End of Secondary Predictor Puller ######################################################

# G-trends Country Codes

# Afghanistan	AF	AFG	004
# ALA	Aland Islands	AX	ALA	248
# Albania	AL	ALB	008
# Algeria	DZ	DZA	012
# American Samoa	AS	ASM	016
# Andorra	AD	AND	020
# Angola	AO	AGO	024
# Anguilla	AI	AIA	660
# Antarctica	AQ	ATA	010
# Antigua and Barbuda	AG	ATG	028
# Argentina	AR	ARG	032
# Armenia	AM	ARM	051
# Aruba	AW	ABW	533
# Australia	AU	AUS	036
# Austria	AT	AUT	040
# Azerbaijan	AZ	AZE	031
# Bahamas	BS	BHS	044
# Bahrain	BH	BHR	048
# Bangladesh	BD	BGD	050
# Barbados	BB	BRB	052
# Belarus	BY	BLR	112
# Belgium	BE	BEL	056
# Belize	BZ	BLZ	084
# Benin	BJ	BEN	204
# Bermuda	BM	BMU	060
# Bhutan	BT	BTN	064
# Bolivia	BO	BOL	068
# Bosnia and Herzegovina	BA	BIH	070
# Botswana	BW	BWA	072
# Bouvet Island	BV	BVT	074
# Brazil	BR	BRA	076
# British Virgin Islands	VG	VGB	092
# British Indian Ocean Territory	IO	IOT	086
# Brunei Darussalam	BN	BRN	096
# Bulgaria	BG	BGR	100
# Burkina Faso	BF	BFA	854
# Burundi	BI	BDI	108
# Cambodia	KH	KHM	116
# Cameroon	CM	CMR	120
# Canada	CA	CAN	124
# Cape Verde	CV	CPV	132
# Cayman Islands	KY	CYM	136
# Central African Republic	CF	CAF	140
# Chad	TD	TCD	148
# Chile	CL	CHL	152
# China	CN	CHN	156
# Hong Kong, SAR China	HK	HKG	344
# Macao, SAR China	MO	MAC	446
# Christmas Island	CX	CXR	162
# Cocos (Keeling) Islands	CC	CCK	166
# Colombia	CO	COL	170
# Comoros	KM	COM	174
# Congo (Brazzaville)	CG	COG	178
# Congo, (Kinshasa)	CD	COD	180
# Cook Islands	CK	COK	184
# Costa Rica	CR	CRI	188
# Côte d'Ivoire	CI	CIV	384
# Croatia	HR	HRV	191
# Cuba	CU	CUB	192
# Cyprus	CY	CYP	196
# Czech Republic	CZ	CZE	203
# Denmark	DK	DNK	208
# Djibouti	DJ	DJI	262
# Dominica	DM	DMA	212
# Dominican Republic	DO	DOM	214
# Ecuador	EC	ECU	218
# Egypt	EG	EGY	818
# El Salvador	SV	SLV	222
# Equatorial Guinea	GQ	GNQ	226
# Eritrea	ER	ERI	232
# Estonia	EE	EST	233
# Ethiopia	ET	ETH	231
# Falkland Islands (Malvinas)	FK	FLK	238
# Faroe Islands	FO	FRO	234
# Fiji	FJ	FJI	242
# Finland	FI	FIN	246
# France	FR	FRA	250
# French Guiana	GF	GUF	254
# French Polynesia	PF	PYF	258
# French Southern Territories	TF	ATF	260
# Gabon	GA	GAB	266
# Gambia	GM	GMB	270
# Georgia	GE	GEO	268
# Germany	DE	DEU	276
# Ghana	GH	GHA	288
# Gibraltar	GI	GIB	292
# Greece	GR	GRC	300
# Greenland	GL	GRL	304
# Grenada	GD	GRD	308
# Guadeloupe	GP	GLP	312
# Guam	GU	GUM	316
# Guatemala	GT	GTM	320
# Guernsey	GG	GGY	831
# Guinea	GN	GIN	324
# Guinea-Bissau	GW	GNB	624
# Guyana	GY	GUY	328
# Haiti	HT	HTI	332
# Heard and Mcdonald Islands	HM	HMD	334
# Holy See (Vatican City State)	VA	VAT	336
# Honduras	HN	HND	340
# Hungary	HU	HUN	348
# Iceland	IS	ISL	352
# India	IN	IND	356
# Indonesia	ID	IDN	360
# Iran, Islamic Republic of	IR	IRN	364
# Iraq	IQ	IRQ	368
# Ireland	IE	IRL	372
# Isle of Man	IM	IMN	833
# Israel	IL	ISR	376
# Italy	IT	ITA	380
# Jamaica	JM	JAM	388
# Japan	JP	JPN	392
# Jersey	JE	JEY	832
# Jordan	JO	JOR	400
# Kazakhstan	KZ	KAZ	398
# Kenya	KE	KEN	404
# Kiribati	KI	KIR	296
# Korea (North)	KP	PRK	408
# Korea (South)	KR	KOR	410
# Kuwait	KW	KWT	414
# Kyrgyzstan	KG	KGZ	417
# Lao PDR	LA	LAO	418
# Latvia	LV	LVA	428
# Lebanon	LB	LBN	422
# Lesotho	LS	LSO	426
# Liberia	LR	LBR	430
# Libya	LY	LBY	434
# Liechtenstein	LI	LIE	438
# Lithuania	LT	LTU	440
# Luxembourg	LU	LUX	442
# Macedonia, Republic of	MK	MKD	807
# Madagascar	MG	MDG	450
# Malawi	MW	MWI	454
# Malaysia	MY	MYS	458
# Maldives	MV	MDV	462
# Mali	ML	MLI	466
# Malta	MT	MLT	470
# Marshall Islands	MH	MHL	584
# Martinique	MQ	MTQ	474
# Mauritania	MR	MRT	478
# Mauritius	MU	MUS	480
# Mayotte	YT	MYT	175
# Mexico	MX	MEX	484
# Micronesia, Federated States of	FM	FSM	583
# Moldova	MD	MDA	498
# Monaco	MC	MCO	492
# Mongolia	MN	MNG	496
# Montenegro	ME	MNE	499
# Montserrat	MS	MSR	500
# Morocco	MA	MAR	504
# Mozambique	MZ	MOZ	508
# Myanmar	MM	MMR	104
# Namibia	NA	NAM	516
# Nauru	NR	NRU	520
# Nepal	NP	NPL	524
# Netherlands	NL	NLD	528
# Netherlands Antilles	AN	ANT	530
# New Caledonia	NC	NCL	540
# New Zealand	NZ	NZL	554
# Nicaragua	NI	NIC	558
# Niger	NE	NER	562
# Nigeria	NG	NGA	566
# Niue	NU	NIU	570
# Norfolk Island	NF	NFK	574
# Northern Mariana Islands	MP	MNP	580
# Norway	NO	NOR	578
# Oman	OM	OMN	512
# Pakistan	PK	PAK	586
# Palau	PW	PLW	585
# Palestinian Territory	PS	PSE	275
# Panama	PA	PAN	591
# Papua New Guinea	PG	PNG	598
# Paraguay	PY	PRY	600
# Peru	PE	PER	604
# Philippines	PH	PHL	608
# Pitcairn	PN	PCN	612
# Poland	PL	POL	616
# Portugal	PT	PRT	620
# Puerto Rico	PR	PRI	630
# Qatar	QA	QAT	634
# Réunion	RE	REU	638
# Romania	RO	ROU	642
# Russian Federation	RU	RUS	643
# Rwanda	RW	RWA	646
# Saint-Barthélemy	BL	BLM	652
# Saint Helena	SH	SHN	654
# Saint Kitts and Nevis	KN	KNA	659
# Saint Lucia	LC	LCA	662
# Saint-Martin (French part)	MF	MAF	663
# Saint Pierre and Miquelon	PM	SPM	666
# Saint Vincent and Grenadines	VC	VCT	670
# Samoa	WS	WSM	882
# San Marino	SM	SMR	674
# Sao Tome and Principe	ST	STP	678
# Saudi Arabia	SA	SAU	682
# Senegal	SN	SEN	686
# Serbia	RS	SRB	688
# Seychelles	SC	SYC	690
# Sierra Leone	SL	SLE	694
# Singapore	SG	SGP	702
# Slovakia	SK	SVK	703
# Slovenia	SI	SVN	705
# Solomon Islands	SB	SLB	090
# Somalia	SO	SOM	706
# South Africa	ZA	ZAF	710
# South Georgia and the South Sandwich Islands	GS	SGS	239
# South Sudan	SS	SSD	728
# Spain	ES	ESP	724
# Sri Lanka	LK	LKA	144
# Sudan	SD	SDN	736
# Suriname	SR	SUR	740
# Svalbard and Jan Mayen Islands	SJ	SJM	744
# Swaziland	SZ	SWZ	748
# Sweden	SE	SWE	752
# Switzerland	CH	CHE	756
# Syrian Arab Republic (Syria)	SY	SYR	760
# Taiwan, Republic of China	TW	TWN	158
# Tajikistan	TJ	TJK	762
# Tanzania, United Republic of	TZ	TZA	834
# Thailand	TH	THA	764
# Timor-Leste	TL	TLS	626
# Togo	TG	TGO	768
# Tokelau	TK	TKL	772
# Tonga	TO	TON	776
# Trinidad and Tobago	TT	TTO	780
# Tunisia	TN	TUN	788
# Turkey	TR	TUR	792
# Turkmenistan	TM	TKM	795
# Turks and Caicos Islands	TC	TCA	796
# Tuvalu	TV	TUV	798
# Uganda	UG	UGA	800
# Ukraine	UA	UKR	804
# United Arab Emirates	AE	ARE	784
# United Kingdom	GB	GBR	826
# United States of America	US	USA	840
# US Minor Outlying Islands	UM	UMI	581
# Uruguay	UY	URY	858
# Uzbekistan	UZ	UZB	860
# Vanuatu	VU	VUT	548
# Venezuela (Bolivarian Republic)	VE	VEN	862
# Viet Nam	VN	VNM	704
# Virgin Islands, US	VI	VIR	850
# Wallis and Futuna Islands	WF	WLF	876
# Western Sahara	EH	ESH	732
# Yemen	YE	YEM	887
# Zambia	ZM	ZMB	894
# Zimbabwe	ZW	ZWE	716