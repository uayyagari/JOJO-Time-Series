# Forecasting based on ARIMA (autoregressive integrated moving averages) models,
# commonly know as the Box–Jenkins approach, comprises following stages: 

# i.) Model identification 
# ii.) Parameter estimation 
# iii.) Diagnostic checking. 

# a) 

# An important step before fitting an ARIMA function is to make sure the timeseries
# is stationary. To do this, there are many ways to remove trend and seasonality.
# One easy approach is to use the function ts( ) in the stats package to remove
# seasonality, and use diff to remove a trend.  
#                                                                   
# A first step in analyzing (stationary) time series is to examine the autocorrelations
# (ACF) and partial autocorrelations (PACF). R provides the functions acf( ) and
# pacf( ) for computing and plotting of ACF and PACF. The order of “pure” AR and
# MA processes can be identified from the ACF and PACF plots. Example ACF
# plots suggesting AR(5) and MA(4) values, left and right respectively, are shown
# below.


# Making data stationary

dataset<-read.table(file.choose(),header=FALSE,sep=",")

#to remove trend

JoJo_value<-diff(dataset$V1)  

# quarterly data

tsData2 <- ts(JoJo_value, frequency = 4); 

# Decompose a time series into seasonal, trend and irregular components using moving averages.

x1=decompose(tsData2)

#to remove seasonality

x1remainder<-tsData2-x1$seasonal  

# "tseries" package in order to use the function "adf.test"(augmented Dickey-Fuller test) and "kpss.test"(Kwiatkowski–Phillips–Schmidt–Shin test).

install.packages("tseries")
library(tseries)

adf.test(x1remainder,alternative = "stationary")
kpss.test(x1remainder)


#As we can see from the Augmented Dickey-Fuller Test that p-value is nearly around
#Zero(too small,not significant from zero). In the same manner, by looking at kpss test we can say that p-value is 
#significant from Zero. So we can now say that time series is stationary


##Q-1B

acf(x1remainder)  
par(mfrow=c(2,1))
acf(x1remainder,21,xlim=c(1,5))   # set the x-axis limits to start at 1 then
pacf(x1remainder,21,ylim=c(-.5,1))

#As the lag length of the final ACF spike equals the MA order of the process we can say that MA order for x1remainder (Stationary Time series data) is 2.


#As ,the lag length of the final PACF spike equals the AR order of the process we can say that MA order for x1remainder (Stationary Time series data) is 2(taken ceiling of 1.5) .


#Q-1C

arima(x1remainder,order=c(1,0,1))    
#(p=1,q=1)

arima(x = x1remainder, order = c(1, 0, 2)) 
#(p=1,q=2)

arima(x = x1remainder, order = c(1, 0, 3)) 
#(p=1,q=3)


arima(x = x1remainder, order = c(2, 0, 1))  
#(p=2,q=1)


arima(x = x1remainder, order = c(2, 0, 2))  
#(p=2,q=2)

arima(x = x1remainder, order = c(2, 0, 3))  
#(p=2,q=3)

arima(x = x1remainder, order = c(2, 0, 4))
#(p=2,q=4)

arima(x = x1remainder, order = c(3, 0, 5))
#(p=3,q=5)

arima(x = x1remainder, order = c(4, 0, 7))  
#(p=4,q=7)


#As we can see that aic value of the arima model of the order (p,d,q)=(4,0,7) is the minimum among all the models analyzed . So we can take this model as appropriate to make further analysis.


#Q-1D

m1.jojo=arima(x1remainder,order=c(4,0,7))
tsdiag1<-tsdiag(m1.jojo,gof=15,omit.initial=FALSE)
Box.test(m1.jojo$residuals,lag=1)

#We can see from Box-pierce test that p-value is significant (>0.5) .So the residuals are not stationary.

#Q-1E
arima101<-arima(x1remainder,order=c(1,0,1))
mydatapred1<-predict(arima101,n.ahead=4)
mydatapred1

#we are satisfied with the fit of an ARIMA(1,0,1)–model 
#By using this model we can predict the quarterly earnings per share values of next quarters of the #year(2030 which is 22 nd year from 2009) which are mentioned above.


#Q-1F
arima101<-arima(x1remainder,order=c(1,0,1))
mydatapred1<-predict(arima101,n.ahead=4)
par(mfrow=c(1,1))
plot(x1remainder)
lines(mydatapred1$pred,col="blue")
lines(mydatapred1$pred+2*mydatapred1$se, col="red")
lines(mydatapred1$pred-2*mydatapred1$se, col="red")

#Confidence interval lies between the two red lines. Took (2*(+,-) standard deviation) from predicted value to define and plot confidence interval


#-------------------------------