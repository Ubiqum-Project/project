#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages('shiny')
# install.packages('coindeskr')
# install.packages('rgdax')
# install.packages('shinydashboard')
# install.packages('shinyjs')
# install.packages('plotly')
# install.packages('quantmod')

library(shiny) #To build the shiny App
library(coindeskr) #R-Package connecting to Coindesk API 
library(rgdax)
library(shinydashboard)
library(ggplot2)
#library(shinyjs)
library(plotly)
library(quantmod)

#jscode <- "shinyjs.refresh = function() { history.go(0); }"
#----------------------------------------------------------


gg.gauge <- function(pos,breaks=c(0,30,50,70,100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="orange")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="lightgreen")+
    geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="forestgreen")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(c("SELL!!", "sell","No Action", "buy", "BUY!!"),"")))+
    annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}

#-----------------------------------------------------------

getSymbols("BTC-USD",src='yahoo')
getSymbols("ETH-USD",src='yahoo')
getSymbols("LTC-USD",src='yahoo')
getSymbols("XRP-USD",src='yahoo')
getSymbols("^GSPC",src='yahoo')
getSymbols("^VIX",src='yahoo')
ETH <- data.frame(Date=index(`ETH-USD`),coredata(`ETH-USD`))
LTC <- data.frame(Date=index(`LTC-USD`),coredata(`LTC-USD`))
ETH = subset(ETH,Date >= "2017-10-14")
ETH = data.frame(ETH$Date ,((ETH$ETH.USD.Close+ETH$ETH.USD.Open)/2))
colnames(ETH) = c("date","dailyAvgETH")

LTC = subset(LTC,Date >= "2017-10-14")
LTC = data.frame(LTC$Date ,((LTC$LTC.USD.Close+LTC$LTC.USD.Open)/2))
colnames(LTC) = c("date","dailyAvgLTC")

#-----------------------> Bitcoin Chart <----------------------------
df <- data.frame(Date=index(`BTC-USD`),coredata(`BTC-USD`))
# create Bollinger Bands
bbands <- BBands(`BTC-USD`[,c("BTC-USD.High","BTC-USD.Low","BTC-USD.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2017-10-14")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$BTC.USD.Close[i] >= df$BTC.USD.Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart
p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~BTC.USD.Open, close = ~BTC.USD.Close,
          high = ~BTC.USD.High, low = ~BTC.USD.Low, name = "BTC.USD",
          increasing = i, decreasing = d) %>%
  add_lines(x = ~Date, y = ~up , name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) %>%
  add_lines(x = ~Date, y = ~dn, name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F)%>%
  layout(yaxis = list(title = "Price"))

# plot volume bar chart
pp <- df %>%
  plot_ly(x=~Date, y=~BTC.USD.Volume, type='bar', name = "BTC.USD Volume",
          color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
  layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label=' WK',
                  step='week',
                  stepmode='backward')
           ))

# subplot with shared x axis
bitcoinChart <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
             shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste("Bitcoin to USD: 2017-10-14 -",Sys.Date()),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))
bitcoinChart
#-----------------------------------------------------------
#-----------------------> Ether Chart <----------------------------
df <- data.frame(Date=index(`ETH-USD`),coredata(`ETH-USD`))
# create Bollinger Bands
bbands <- BBands(`ETH-USD`[,c("ETH-USD.High","ETH-USD.Low","ETH-USD.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2017-10-14")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$ETH.USD.Close[i] >= df$ETH.USD.Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart
p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~ETH.USD.Open, close = ~ETH.USD.Close,
          high = ~ETH.USD.High, low = ~ETH.USD.Low, name = "ETH.USD",
          increasing = i, decreasing = d) %>%
  add_lines(x = ~Date, y = ~up , name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) %>%
  add_lines(x = ~Date, y = ~dn, name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F)%>%
  layout(yaxis = list(title = "Price"))

# plot volume bar chart
pp <- df %>%
  plot_ly(x=~Date, y=~ETH.USD.Volume, type='bar', name = "ETH.USD Volume",
          color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
  layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label=' WK',
                  step='week',
                  stepmode='backward')
           ))

# subplot with shared x axis
etherChart <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
             shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste("Ethereum to USD: 2017-10-14 -",Sys.Date()),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))


etherChart
#-----------------------------------------------------------
#-----------------------> Litecoin Chart <----------------------------
df <- data.frame(Date=index(`LTC-USD`),coredata(`LTC-USD`))
# create Bollinger Bands
bbands <- BBands(`LTC-USD`[,c("LTC-USD.High","LTC-USD.Low","LTC-USD.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2017-10-14")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$LTC.USD.Close[i] >= df$LTC.USD.Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart
p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~LTC.USD.Open, close = ~LTC.USD.Close,
          high = ~LTC.USD.High, low = ~LTC.USD.Low, name = "LTC.USD",
          increasing = i, decreasing = d) %>%
  add_lines(x = ~Date, y = ~up , name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) %>%
  add_lines(x = ~Date, y = ~dn, name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F)%>%
  layout(yaxis = list(title = "Price"))

# plot volume bar chart
pp <- df %>%
  plot_ly(x=~Date, y=~LTC.USD.Volume, type='bar', name = "LTC.USD Volume",
          color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
  layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label=' WK',
                  step='week',
                  stepmode='backward')
           ))

# subplot with shared x axis
liteChart <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
             shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste("Litecoin to USD: 2017-10-14 -",Sys.Date()),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))


liteChart
#-----------------------------------------------------------
#-----------------------> Ripple Chart <----------------------------
df <- data.frame(Date=index(`XRP-USD`),coredata(`XRP-USD`))
# create Bollinger Bands
bbands <- BBands(`XRP-USD`[,c("XRP-USD.High","XRP-USD.Low","XRP-USD.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2017-10-14")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$XRP.USD.Close[i] >= df$XRP.USD.Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart
p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~XRP.USD.Open, close = ~XRP.USD.Close,
          high = ~XRP.USD.High, low = ~XRP.USD.Low, name = "XRP.USD",
          increasing = i, decreasing = d) %>%
  add_lines(x = ~Date, y = ~up , name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) %>%
  add_lines(x = ~Date, y = ~dn, name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F)%>%
  layout(yaxis = list(title = "Price"))

# plot volume bar chart
pp <- df %>%
  plot_ly(x=~Date, y=~XRP.USD.Volume, type='bar', name = "XRP.USD Volume",
          color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
  layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label=' WK',
                  step='week',
                  stepmode='backward')
           ))

# subplot with shared x axis
rippleChart <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
             shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste("Ripple to USD: 2017-10-14 -",Sys.Date()),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))


rippleChart
#-----------------------------------------------------------
#-----------------------> VIX Chart <----------------------------
df <- data.frame(Date=index(`VIX`),coredata(`VIX`))
# create Bollinger Bands
bbands <- BBands(`VIX`[,c("VIX.High","VIX.Low","VIX.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2017-10-14")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$VIX.Close[i] >= df$VIX.Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart
p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~VIX.Open, close = ~VIX.Close,
          high = ~VIX.High, low = ~VIX.Low, name = "VIX",
          increasing = i, decreasing = d) %>%
  add_lines(x = ~Date, y = ~up , name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) %>%
  add_lines(x = ~Date, y = ~dn, name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F)%>%
  layout(yaxis = list(title = "Price"))

# plot volume bar chart
pp <- df %>%
  plot_ly(x=~Date, y=~VIX.Volume, type='bar', name = "VIX Volume",
          color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
  layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label=' WK',
                  step='week',
                  stepmode='backward')
           ))

# subplot with shared x axis
vixChart <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
                       shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste("VIX: 2017-10-14 -",Sys.Date()),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))


vixChart
#-----------------------------------------------------------
#-----------------------> SP500 Chart <----------------------------
df <- data.frame(Date=index(`GSPC`),coredata(`GSPC`))
# create Bollinger Bands
bbands <- BBands(`GSPC`[,c("GSPC.High","GSPC.Low","GSPC.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2017-10-14")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$GSPC.Close[i] >= df$GSPC.Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart
p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~GSPC.Open, close = ~GSPC.Close,
          high = ~GSPC.High, low = ~GSPC.Low, name = "GSPC",
          increasing = i, decreasing = d) %>%
  add_lines(x = ~Date, y = ~up , name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) %>%
  add_lines(x = ~Date, y = ~dn, name = "B Bands",
            line = list(color = 'black', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F)%>%
  layout(yaxis = list(title = "Price"))

# plot volume bar chart
pp <- df %>%
  plot_ly(x=~Date, y=~GSPC.Volume, type='bar', name = "GSPC Volume",
          color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
  layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label=' WK',
                  step='week',
                  stepmode='backward')
           ))

# subplot with shared x axis
sp500Chart <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
                    shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste("S&P 500: 2017-10-14 -",Sys.Date()),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))


sp500Chart
#-----------------------------------------------------------
transactions = read.csv("transactions.csv")[,-1]


transactions$date = as.character(transactions$date)
transactions$walletValue= as.numeric(transactions$walletValue)


current = public_ticker(product_id = "BTC-USD")


#-----------------------------------------------------------


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Crypto-Prophet"),
  dashboardSidebar(sidebarMenu(
    a(h4("Data Home"), href = paste0("http://www.joe-data.com/")),
    menuItem("Welcome!!", tabName = "home", icon = icon("home")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer")),
    menuItem("Chart Corner", tabName = "chart", icon = icon("bar-chart")),
    menuItem("Wallet Ledger", tabName = "ledger", icon = icon("money"))
    
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              
              h1(icon("android"), align="center" ),
              h2("Welcome to Crypto-Prophet: Your Cryptocurrency Robot Trading Advisor", align="center"),
              hr(),
              p(" "),
              
              hr(),
              p(" "),
              
              hr(),
             
              h1(icon("comments"), align="center" ),
              hr(),
              p(" "),
              hr(),
              h1(icon("line-chart"), align="center" ),
              hr(),
              p(" "),
              hr(),
              p(" "),
              hr(),
              h1(icon("money"), align="center" ),
              hr(),
              p(" "),
              hr(),
              
              h1(icon("bitcoin"), align="center" )
              ),
      tabItem(tabName = "dashboard",
             
              
              fluidRow(
                titlePanel('Robot Advisor Settings'),
                
                mainPanel(
                  
                  box(
                    title = "Controls",
                    sliderInput("sentiment",  label = div(style='width:250px;', 
                                                          "This is the Output of the Sentiment Model",
                                                          br(),
                                                          div(style='float:left;', 'Decrease'), 
                                                          div(style='float:right;', 'Increase')),  -1, 1, 0, step = .5),
                    plotOutput('sentGauge'),
                    sliderInput("technical",  label = div(style='width:250px;', 
                                                          "This is the Output of the Technical Model",
                                                          br(),
                                                          div(style='float:left;', 'Decrease'), 
                                                          div(style='float:right;', 'Increase')),  -1, 1, 0, step = .5),
                    plotOutput('techGauge'),
                    sliderInput("riskBTC",  label = div(style='width:250px;', 
                                                        "What % of BTC Wallet Available to Trade?",
                                                        br(),
                                                        div(style='float:left;', '0%'), 
                                                        div(style='float:right;', '100%')),  0, 100, 10),
                    sliderInput("riskUSD", label = div(style='width:250px;', 
                                                       "What % of USD Wallet Available to Trade?",
                                                       br(),
                                                       div(style='float:left;', '0%'), 
                                                       div(style='float:right;', '100%')), 1, 100, 10),
                    sliderInput("sentVtech", label = div(style='width:250px;', 
                                                         "What Analysis Algorithm Would You Like to Favor (5.5 means equal weight)",
                                                         br(),
                                                         div(style='float:left;', 'Sentiment'), 
                                                         div(style='float:right;', 'Technical')), 1, 10, 5.5, step = .5),
                    plotOutput('combiGauge'),
                    textOutput('recommendation'),
                    actionButton("submitButton","Submit Order!")
                    
                  )
                )
              )
              
              
              ),
      tabItem(tabName = "chart",
              fluidRow(
                
                box( plotlyOutput("btcprice")),
                box( plotlyOutput("ethprice")),
                box( plotlyOutput("ltcprice")),
                box( plotlyOutput("xrpprice")),
                box( plotlyOutput("vixprice")),
                box( plotlyOutput("sp500price"))
                
              )),
      tabItem(tabName = "ledger",
              
              tableOutput("values")))
    # useShinyjs(),
    # extendShinyjs(text = jscode),
   ))

# Define server logic required to draw a histogram
server <- function(input,output){
  
  sliderValues <- reactive({
 
    bid = current$bid
    ask = current$ask
    price = current$price
    vol = current$volume
    
    date =as.character(current$time)
    
    btcAlert = "No BTC Wallet Constraints!"
    usdAlert = "No USD Wallet Constraints!"
    technical= input$technical  # Range of 1,.5, 0, -.5, -1  These Feed in from Algorithms
    sentiment = input$sentiment # Range of 1,.5, 0, -.5, -1   These Feed in from Algorithms
    print(paste("technical:",technical))
    print(paste("sentiment:",sentiment))
    riskBTC = input$riskBTC/100        #User defined value (basically percentage of wallet available for transaction) 
    riskUSD = input$riskUSD/100        #User defined value (basically percentage of wallet available for transaction)
    print(paste("riskUSD:",riskUSD))
    print(paste("riskBTC:",riskBTC))
    sentVtech = input$sentVtech       #User defined value (the amount of weight each algo will receive: 1 = full sentiment, 9 = full technical, 5 = equal mix)
    print(paste("sentVtech:",sentVtech))
    #---------> Combines the user defined weights to identify a multiplier <------------------
    sentVal = sentiment*((10- sentVtech)/10)
    techVal = technical*(sentVtech/10)
    combiVal = techVal+sentVal
    print(paste("sentVal:",sentVal))
    print(paste("techVal:",techVal))
    print(paste("combiVal:",combiVal))
    
    #---------> Calculates amount of wallet available to use <------------------
    walletUSD =as.numeric(transactions$walletUSD[nrow(transactions)])
    print(paste("walletUSD:",walletUSD))
    walletBTC =as.numeric(transactions$walletBTC[nrow(transactions)])
    print(paste("walletBTC:",walletBTC))
    
    playUSD=walletUSD *riskUSD
    playBTC=walletBTC*riskBTC
    print(paste("playUSD:",playUSD))
    print(paste("playBTC:",playBTC))
    
    playUSDAvailable = playUSD
    playBTCAvailable = playBTC
    
    
    
    if((playUSD/price) < .001)
    {
      if(walletUSD > .001*price)
      {
        usdAlert = paste("Oops, you selected a USD wallet risk percentage that GDAX does not support:",round(playUSD/price, digits =4),"BTC.  I am going to default you to .001 BTC, the minimum for a trade")
        playUSDAvailable = .001*price
      } else { playUSDAvailable = 0
      usdAlert = paste("Oops, your wallet has insufficient funds to meet the .001 BTC trade minimum.  ")
      }
    }
    
    if((playBTC) < .001)
    {
      if(walletBTC > .001)
      {
        playBTCAvailable = .001
      } else { playBTCAvailable = 0
      usdAlert = paste("Oops, your wallet has insufficient funds to meet the .001 BTC trade minimum.  ")
      }
    }
    
    print(paste("playUSDAvailable:",playUSDAvailable))
    print(paste("playBTCAvailable:",playBTCAvailable))
    btcXaction = 0
    usdXaction = 0
    
    canBuy = playUSDAvailable/price 
    canSell = playBTCAvailable*price
    
    print(paste("canBuy:",canBuy))
    print(paste("canSell:",canSell))
    recommendation =0
    #----------> Calculates the appropriate move <---------------
    
    if (combiVal == 0)
    {
      recommendation =paste("Based upon your selections and the sentiment/technical indicators from the last week, it appears there will be very little change in Bitcoin price over the next 24 hours to justify any position change.  Therefore, I recommend no transactions. If you wish to log a transaction of zero, go ahead anc click the submit order button below. ")
      walletBTC = walletBTC
      walletUSD = walletUSD
    }else if(combiVal > 0 && canBuy>0)
    {
      
      walletBTC = walletBTC+canBuy
      walletUSD = walletUSD-playUSD
      recommendation =paste("I think you should buy some Bitcoin.  Based upon sentiment and technical indicators from the past week, it appears that Bitcoin price will increase over the next 24 hour period.  I recommend you buy",round(canBuy, digits = 4), "bitcoin for $", round(playUSD, digits = 2), ".  That would put your bitcoin wallet at:", round(walletBTC, digits = 4), "BTC and your USD wallet at: $", round(walletUSD, digits = 2),".  If you agree with this transaction (Buy Low), click the submit order button below.")
      btcXaction = +canBuy
      usdXaction = -playUSD
    }else if(combiVal < 0 && canSell>0)
    {
      
      walletBTC = walletBTC-playBTC
      walletUSD =walletUSD +canSell
      btcXaction = -playBTC
      usdXaction = +canSell
      recommendation = paste("I think you should sell some Bitcoin.  Based upon sentiment and technical indicators from the past week, it appears that Bitcoin price will decrease over the next 24 hour period.  I recommend you sell",round(playBTC, digits = 4), "bitcoin for $", round(canSell, digits = 2), ".  That would put your bitcoin wallet at:", round(walletBTC, digits = 4), "BTC and your USD wallet at: $", round(walletUSD, digits = 2),".  If you agree with this transaction (Sell High), click the submit order button below.")
    } else { recommendation = paste("Trade denied to to insuficient funds:", round(canBuy, digits = 4),"USD and ", round(canSell, digits = 4),"BTC")}
    
    print(paste("walletBTC:",walletBTC))
    print(paste("walletUSD:",walletUSD))
    print(paste("btcXaction:",btcXaction))
    print(paste("usdXaction:",usdXaction))
    walletValue = (price*walletBTC)+walletUSD
    print(paste("walletValue:",walletValue))
    
    delta = walletValue-as.numeric(transactions$walletValue[nrow(transactions)-1]) #change in wallet value from previous
   value = data.frame(date,  bid, ask, price, vol, technical, sentiment, sentVtech, riskBTC, riskUSD, btcXaction, usdXaction,walletBTC,walletUSD, walletValue, delta, recommendation)
    # transactions[nrow(transactions) + 1,] = c(date,  bid, ask, price, vol, technical, sentiment, sentVtech, riskBTC, riskUSD, btcXaction, usdXaction,walletBTC,walletUSD, walletValue, delta)
    # 
    # write.csv(transactions, file = "transactions.csv")
    #print(recommendation)
    print(paste("recommendation: ",value$recommendation))
    return(value)
    
   
    
  })
  output$values <- renderTable({
   data =  transactions
   data = data[,c(1,11,12,13,14,15,16)]
   data
  })
  
  output$recommendation <- renderText({
    data=sliderValues()
    response = as.character(data[1,ncol(data)])
    return(response)
  })
  output$sentGauge = renderPlot({
    data = sliderValues()
    sentVal = data$sentiment
    sentNorm = ((sentVal+1)/2)*100
   sentGauge= gg.gauge(sentNorm,breaks=c(0,30,50,70,100))
   sentGauge
  })
  
  output$techGauge = renderPlot({
    data = sliderValues()
    sentVal = data$technical
    sentNorm = ((sentVal+1)/2)*100
    techGauge= gg.gauge(sentNorm,breaks=c(0,30,50,70,100))
    techGauge
  })
  
  output$combiGauge = renderPlot({
    data = sliderValues()
    technical = data$technical
    sentiment = data$sentiment
    sentVtech = input$sentVtech       #User defined value (the amount of weight each algo will receive: 1 = full sentiment, 9 = full technical, 5 = equal mix)
      sentVal = sentiment*((10- sentVtech)/10)
    techVal = technical*(sentVtech/10)
    combiVal = techVal+sentVal
    sentNorm = ((combiVal+1)/2)*100
    combiGauge= gg.gauge(sentNorm,breaks=c(0,30,50,70,100))
    combiGauge
  })
  #------------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  output$btcprice <- renderPlotly(
   
    bitcoinChart
  )
  output$ethprice <- renderPlotly(
    etherChart
  )
  output$ltcprice <- renderPlotly(
    liteChart
  )
  output$xrpprice <- renderPlotly(
    rippleChart
  )
  output$vixprice <- renderPlotly(
    vixChart
  )
  
  output$sp500price <- renderPlotly(
   sp500Chart
  )
  

  
  writeCSV <- observe({
    if(input$submitButton == 0) return()
    data = sliderValues()
    data = data[,1:ncol(data)-1]
    data[,1] = as.character(data[,1])
    print(data)
    print(transactions)
     transactions[nrow(transactions) + 1,] = data[1,]
     print(transactions)
     write.csv(transactions, file = "transactions.csv")
     print("writing Complete")
   #  js$refresh();
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




