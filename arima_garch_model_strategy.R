# Set up the current working directory 
folder <- "./Desktop/stock-analysis"
setwd(folder)

# Libraries that are going to be used in analysis
library(zoo); library(xts)
library(TTR); library(quantmod)
library(tseries)
library(timeSeries)
library(rugarch)

# Read data 
prices_adj <- read.csv("prices-split-adjusted.csv")

# Change the date formats of tables
prices_adj$date <- as.Date(prices_adj$date)

# Select stock prices to look into
stock_YHOO <- subset(prices_adj, symbol == "YHOO")
#stock_YHOO <- stock_YHOO[stock_YHOO$date > "2014-01-01",]

# Create time series of adjusted closing prices using zoo
timeline <- stock_YHOO[,1]
close_YHOO <- zoo(stock_YHOO[,4], timeline)
plot(close_YHOO, ylab = "Adjusted closing price", main = "YAHOO")

#Take the first difference of the Logarithmic transformation of closing prices
returns_YHOO = diff(log(close_YHOO))
plot(returns_YHOO)

#Set up window length used for modelling and the length of periods to be forecasted
windowLength = 500
foreLength = 50
forecasts <- vector(mode = "character", length=foreLength)

for(d in 0:foreLength){
  return_offset = returns_YHOO[(1+d):(windowLength + d)]
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
  
  if(is(fit, "warning")){
    forecasts[d+1] = paste(index(return_offset[windowLength]), 1, sep=",")
    print(paste(index(return_offset[windowLength]), 1, sep=","))
  } else{
    fore = ugarchforecast(fit, n.ahead=1)
    ind = fore@forecast$seriesFor
    forecasts[d+1] = paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")
    print(paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")) 
  }
}

# Since the current forecast series contain a list of dates and a list of predictions for
# the next day, rearrange the forecast series by moving predicted values one day ahead
forecast_spl <- strsplit(forecasts, ",")
forecast_spl <- matrix(unlist(forecast_spl), ncol=2, byrow=TRUE)
timeline <- as.Date(forecast_spl[,1])
forecast_series <- lag(zoo(forecast_spl[,2], timeline))

# Backtesting & Plotting 
returnIntersect <- merge(forecast_series, returns_YHOO, all=F)
ArimaGarchReturns <- as.numeric(returnIntersect[,1]) * as.numeric(returnIntersect[,2])

ArimaGarchCurve <- log(cumprod(1 + ArimaGarchReturns))
BuyHoldCurve <- log(cumprod(1 + as.numeric(returnIntersect[,2])))

miny = min(ArimaGarchCurve, BuyHoldCurve)
maxy = max(ArimaGarchCurve, BuyHoldCurve)
plot(timeline[2:length(timeline)], ArimaGarchCurve, type="l", col="red", ylim=c(miny, maxy))
lines(timeline[2:length(timeline)], BuyHoldCurve, type="l", col="blue")



