#install.packages("forecast")
#install.packages("tseries")
library(forecast)
library(tseries)


setwd('/Users/wenyantuo/Desktop/TAwork/R-LAB-2022')
sales<-read.csv('sales.csv',header = TRUE)
head(sales)
sales_ts = ts(sales$Sales_k, start= c(1972), freq = 12)
autoplot(sales_ts)

# Check for stationarity
adf.test(sales_ts, k =12)

# use difference 
sales_ts_df1 = diff(sales_ts, differences = 1)
adf.test(sales_ts_df1, k = 12)

# if still not stationary, use second degree difference
sales_ts_df2 = diff(sales_ts, differences = 2)
adf.test(sales_ts_df2, k = 12)

#We find it stationary when d = 2, so in the ARIMA model, d =2
#we plot again with d = 2
autoplot(sales_ts_df2)


#Choose p (AR or Lag) term with PACF plot
Pacf(sales_ts_df2)
# we choose p =7 from this particular plot

#Choose q (MA or Moving Average ) term with ACF plot
Acf(sales_ts_df2)
# we choose q =6 from this particular plot

# Fitting an ARIMA model
# with (p, d, q) = (7,2,6)
tsMod = Arima(y = sales_ts, order = c(7,2,6))
print(tsMod)

#forecast
forecast(tsMod, h = 12)


#plot the forecasted
autoplot(forecast(tsMod, h = 12))
