#Financial data on Bitcoin
#https://www.quandl.com/tools/r
#https://www.quandl.com/collections/markets/bitcoin-data
library(Quandl)
Quandl.api_key("2zs3zXe_rhC6E9CW1QPb")



st.dt<-"2017-11-29"
end.dt<-"2018-01-25"

Price<-Quandl("BCHARTS/BITSTAMPUSD", start_date=st.dt, end_date=end.dt)
nb.Bitcoin<-Quandl("BCHAIN/TOTBC", start_date=st.dt, end_date=end.dt)

Price<-Price[order(as.Date(Price$Date)),]

#Add price features
Price<-DeltaCol(Price,Price$Close)

#GRAPH
ggplot()+geom_line(data=Price,aes(x = Date,y=Close))+scale_x_date(breaks = date_breaks("2 days"),labels=date_format("%d-%m"))


