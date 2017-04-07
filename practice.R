folder <- "C:/Users/yekim/Desktop/data"

setwd(folder)

install.packages("quantmod")
install.packages("lattice")
install.packages("timeSeries")
install.packages("rugarch")
install.packages("zoo")
install.packages("xts")
install.packages("TTR")

#library("quantmod")
library(tseries)
library(zoo)
library(xts); library(TTR)
library(quantmod)
library(timeSeries)
library(rugarch)

# Read data 
fundamentals <- read.csv("fundamentals.csv")
prices <- read.csv("prices.csv")
prices_adj <- read.csv("prices-split-adjusted.csv")
securities <- read.csv("securities.csv")

#Change the colnames for convenience (to use sql queries below)
colnames(fundamentals) <- gsub("[^[:alnum:]]", "_", colnames(fundamentals))
colnames(securities) <- gsub("[^[:alnum:]]", "_", colnames(securities))

#Change the date formats of tables 
fundamentals$Period_Ending <- as.Date(fundamentals$Period_Ending)
prices$date <- as.Date(prices$date)
prices_adj$date <- as.Date(prices_adj$date)
#securities$Date_first_added <- as.Date(securities$Date_first_added)

# SQL queries to explore data
library(proto); library(gsubfn); library(RSQLite); library(DBI)
library(sqldf); #library(tcltk)

sqldf("SELECT COUNT(DISTINCT(Ticker_Symbol)) FROM fundamentals")
sqldf("SELECT COUNT(DISTINCT(symbol)) FROM prices_adj")
sqldf("SELECT COUNT(DISTINCT(Ticker_symbol)) FROM securities")

sqldf("SELECT DISTINCT(Ticker_symbol) FROM securities WHERE GICS_Sub_Industry = 'Airlines'")
combined_prices <- sqldf("WITH temp AS (SELECT Ticker_Symbol, Security, GICS_Sub_Industry FROM securities)
                         SELECT * FROM prices_adj LEFT JOIN temp ON prices_adj.symbol = temp.Ticker_symbol
                         ORDER BY 10, 2, 1")

combined_prices <- combined_prices[,c(10,1,2,9,3,4,5,6,7)]

# Look into Yahoo's stock prices

stock_ALK <- subset(prices_adj, symbol == "ALK")
stock_AAL <- subset(prices_adj, symbol == "AAL")
stock_DAL <- subset(prices_adj, symbol == "DAL")
stock_LUV <- subset(prices_adj, symbol == "LUV")
stock_UAL <- subset(prices_adj, symbol == "UAL")

#Create time series using zoo
library(zoo)
timeline <- stock_ALK[,1]
close_ALK <- zoo(stock_ALK[,4], timeline)
close_AAL <- zoo(stock_AAL[,4], timeline)
close_DAL <- zoo(stock_DAL[,4], timeline)
close_LUV <- zoo(stock_LUV[,4], timeline)
close_UAL <- zoo(stock_UAL[,4], timeline)

#Plot the adjusted closing price for each airline
par(mfrow = c(2,3))
plot(close_ALK, ylab = "Adjusted closing price", main = "Alaska Air")
plot(close_AAL, ylab = "Adjusted closing price", main = "American Airlines")
plot(close_DAL, ylab = "Adjusted closing price", main = "Delta Airlines")
plot(close_LUV, ylab = "Adjusted closing price", main = "Southwest Airlines")
plot(close_UAL, ylab = "Adjusted closing price", main = "United Continental Holdings")

library(leaps); library(locfit); library(mgcv); library(nlme); library(quadprog); library(tseries); library(TSA)

#Take the first difference from the Logarithmic transformation of closing prices
returns_ALK = diff(log(close_ALK))
returns_AAL = diff(log(close_AAL))
returns_DAL = diff(log(close_DAL))
returns_LUV = diff(log(close_LUV))
returns_UAL = diff(log(close_UAL))

#Plot the first difference of logarithmic transformations (increments)
par(mfrow = c(2,3))
plot(returns_ALK)
plot(returns_AAL)
plot(returns_DAL)
plot(returns_LUV)
plot(returns_UAL)

windowLength = 500
foreLength = 50
forecasts <- vector(mode = "character", length=foreLength)

for(d in 0:foreLength){
  return_offset = returns_ALK[(1+d):(windowLength + d)]
  final.aic <- Inf
  final.order <- c(0,0,0)
  for(p in 0:5) for(q in 0:5){
    if(p==0 && q==0){
      next
    }
    arimafit = tryCatch( arima(return_offset, order= c(p,0,q)),
                         error = function( err ) FALSE, 
                         warning = function( err ) FALSE)
    
    if(!is.logical(arimafit)) {
      current.aic <- AIC(arimafit)
      if(current.aic < final.aic){
        final.aic <-current.aic
        final.order <- c(p,0,q)
        final.arima <- arima(return_offset, order = final.order)
      }
    }else{
      next
    }
  } 
  
  #Specify and fit the Garch model
  spec = ugarchspec(variance.model = list(garchOrder = c(1,1)),
                    mean.model = list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
                    distribution.model = "sged")
  fit = tryCatch(ugarchfit(spec, return_offset, solver='hybrid'),
                 error=function(e) e, warning=function(w) w)
   
  
}



final.arima[is.na(final.arima)]
acf(resid(final.arima))

sum(is.na(return_offset))
is.na((final.arima))


par(mfrow=c(2,3))
acf(as.vector(returns_ALK), lag.max=36, ci.type='ma')
acf(as.vector(returns_AAL), lag.max=36, ci.type='ma')
acf(as.vector(returns_DAL), lag.max=36, ci.type='ma')
acf(as.vector(returns_LUV), lag.max=36, ci.type='ma')
acf(as.vector(returns_UAL), lag.max=36, ci.type='ma')

#acf(coredata(diff(close_ALK)))
par(mfrow=c(2,3))
pacf(as.vector(diff(close_ALK)), lag.max=36)
pacf(as.vector(diff(close_AAL)), lag.max=36)
pacf(as.vector(diff(close_DAL)), lag.max=36)
pacf(as.vector(diff(close_LUV)), lag.max=36)
pacf(as.vector(diff(close_UAL)), lag.max=36)



data = merge(diff(close_ALK), diff(close_AAL))
data = merge(data, diff(close_DAL))
data = merge(data, diff(close_LUV))
data = merge(data, diff(close_UAL))

cor(data)

comb <- lm(diff(close_UAL) ~ diff(close_DAL))
adf.test(comb$residuals)
