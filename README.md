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

Questions:
-What are optimal windowlength and forelength?
-How would the quality of forecasting differ between ARIMA model and ARIMA + GARCH model?
-For which time periods do results of ARIMA+GARCH strategy and buy hold strategy differ little/a lot? 
 For which time periods does ARIMA+GARCH strategy outperform/underperform buy hold strategy? 
-What is an optimal position to take (short vs long) when the GARCH model does not converge? 
