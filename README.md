# stock-analysis

Data: "securities.csv", "prices.csv", "prices-split-adjusted.csv", "fundamentals.csv" 
 (downloaded from Kaggle competition) 
 
 "SP500.csv"
 (downloaded from SP500)
 
1. ARIMA + GARCH trading strategy 
 (Benchmarked and modelled based on Michael Halls-Moore's blog post: https://www.quantstart.com/articles/ARIMA-GARCH-Trading-Strategy-on-the-SP500-Stock-Market-Index-Using-R)
 
Strategy:
1. Choose window length = k and forecasting length = n
2. For each day, from k+1 to k+n, use the differenced logarithmic returns of a stock market index to fit an optimal ARIMA and GARCH model. 
3. Use the combiend model to predict the next day's returns
4. If the predicted value is positive, the stock is longed at the previous close. 
If the predicted value is negative, the stock is longed.


