folder <- "./Desktop/Kaggle competition/stock data"
setwd(folder)

#library("quantmod")
library("tseries")

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

#Take Logarithmic transformation of closing prices
close_ALK = log(close_ALK)
close_AAL = log(close_AAL)
close_DAL = log(close_DAL)
close_LUV = log(close_LUV)
close_UAL = log(close_UAL)

#Plot the first difference of logarithmic transformations (increments)
par(mfrow = c(2,3))
plot(diff(close_ALK))
plot(diff(close_AAL))
plot(diff(close_DAL))
plot(diff(close_LUV))
plot(diff(close_UAL))

#par(mfrow = c(2,3))
#plot(diff(diff(close_ALK), lag=25))
#plot(diff(diff(close_AAL), lag=25))
#plot(diff(diff(close_DAL), lag=25))
#plot(diff(diff(close_LUV), lag=25))
#plot(diff(diff(close_UAL), lag=25))


par(mfrow=c(2,3))
acf(as.vector(diff(close_ALK)), lag.max=36, ci.type='ma')
acf(as.vector(diff(close_AAL)), lag.max=36, ci.type='ma')
acf(as.vector(diff(close_DAL)), lag.max=36, ci.type='ma')
acf(as.vector(diff(close_LUV)), lag.max=36, ci.type='ma')
acf(as.vector(diff(close_UAL)), lag.max=36, ci.type='ma')

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
