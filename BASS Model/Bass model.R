setwd('/Users/vihari/Desktop/R-LAB-2022')
data = read.csv('iphone_sales.csv',header = TRUE)
head(data) 

# Create a time series variable - sales; started at the 3rd quarter of 2007, "freq = 4" specify it is quarterly data 
Sales = ts(data$Sales, start= c(2007,3), freq = 4)

# Plot the data
plot(Sales, type= 'l', lty= 2, col = 'red', ylab='', xlab='')
points(Sales,pch=20,col="blue")
title("Quarterly Iphone Sales(millions)")

# Plot the cumulative sales
Y = cumsum(Sales)
Y = ts(Y, start = c(2007,3), freq = 4)
plot(Y, type= 'l', lty= 2, col = 'red', ylab='', xlab='')
points(Y,pch=20,col="blue")
title("Cumulative Iphone Sales(millions)")

#Fit the bass model and compute p, q, m
Y = c(0, Y[1:length(Y)-1]) # we want Y_t-1 not Y_t, Y_0 = 0
Ysq = Y**2
res = lm(Sales ~ Y+Ysq)
summary(res)
a = res$coefficients[1]
b = res$coefficients[2]
c = res$coefficients[3]
mplus = (-b+sqrt(b**2-4*a*c))/(2*c)
mminus = (-b-sqrt(b**2-4*a*c))/(2*c)

m = mminus
p = 1/m
q = b+p

#Bass Model Function
bassModel = function(p,q,m, T =100)
{
  S = double(T)
  Y = double(T+1)
  Y[1] = 0
  for(t in 1:T)
  {
    S[t] = p*m + (q-p)*Y[t] - (q/m)*Y[t]**2
    Y[t+1] = Y[t]+S[t]
  }
  return(list(sales=S, cumSales=cumsum(S)))
}

#compute
Spred = bassModel(p,q,m,T = 23)$sales
Spred = ts(Spred, start = c(2007,3), frequency = 4)
ts.plot(Sales, Spred, col = c("blue", "red"))
legend("topleft", legend = c("Actual", "Bass Model"), fill = c("blue","red"))

# now do this for the cumulative sales
Spred = bassModel(p,q,m)$sales
cumSpred = ts(cumsum(Spred), start = c(2007,3), frequency = 4)
cumSales = ts(cumsum(Sales), start = c(2007,3), frequency = 4)
ts.plot(cumSales, cumSpred, col = c("blue", "red"))
legend("topleft", legend = c("Actual", "Bass Model"), fill = c("blue","red"))
title("Predicted Cumulative Iphone Sales")


