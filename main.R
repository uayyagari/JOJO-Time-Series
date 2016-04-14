##Q-1A  Making data stationary
dataset<-read.table(file.choose(),header=FALSE,sep=",")
JoJo_value<-diff(dataset$V1)   #to remove trend
tsData2 <- ts(JoJo_value, frequency = 4); # quarterly data
x1=decompose(tsData2)
x1remainder<-tsData2-x1$seasonal   #to remove seasonality
adf.test(x1remainder,alternative = "stationary")
kpss.test(x1remainder)


#As we can see from the Augmented Dickey-Fuller Test that p-value is nearly around
#Zero(too small,not significant from zero).Same way by looking At kpss test we can say that p-value is #significant from Zero. So we can now say that time series is stationary


##Q-1B

acf(x1remainder)  
par(mfrow=c(2,1))
acf(x1remainder,21,xlim=c(1,5))   # set the x-axis limits to start at 1 then
pacf(x1remainder,21,ylim=c(-.5,1))

#As the lag length of the final ACF spike equals the MA order of the process we can say that MA order for x1remainder (Stationary Time series data) is 2.


#As ,the lag length of the final PACF spike equals the AR order of the process we can say that MA order for x1remainder (Stationary Time series data) is 2(taken ceil of 1.5) .


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

#Q-2A

#----Read Glassdata file
glassdata<-read.table(file.choose(),header=F,sep=",")        
glassdataframe<-data.frame(glassdata)
collection<-c(1,2,3,4)
#----V11 column is glass_type column
glassdataframe["bi"]<-ifelse(glassdataframe$V11 %in% collection,0,1)
header1<-c("id","RI","Na","Mg","Al","Si","K","Ca","Ba","Fe","Type","bi")
#----unlist the header
colnames(glassdataframe)<-unlist(header1)
View(glassdataframe)

#Created new DataFrame Column “bi” 
#1)set the value of column “bi” to 0 if the glass type is 1 through 4
#2)set the value of column “bi” to 1 if the glass type is 5 through 7


#Q-2B
#--feature matrix (fea)
attach(glassdataframe)
fea=matrix(c(RI,Na,Mg,Al,Si,K,Ca,Ba,Fe),nrow=214,ncol =9 ,byrow = F)
m2=lm(glassdataframe$bi~RI+Na+Mg+Al+Si+K+Ca+Ba+Fe)
#--Response vector (y)
y=m2$model$`glassdataframe$bi`

#Created “fea” matrix by using features (RI,Na,Mg,Al,Si,K,Ca,Ba,Fe)
#Created response vector “y” from the “bi” column


#Q-2C
#Normalize the data so that higher magnitude values don’t influence while predicting response values for testing data
normalize<-function(x) 
{
  return ((x-min(x))/(max(x)-min(x)))
}

glassdata_normalize<-as.data.frame(lapply(glassdataframe[,c(2,3,4,5,6,7,8,9,10)],normalize))
dataframecoll=data.frame(y) 
smp<-floor(0.60*nrow(glassdata_normalize))
set.seed(123)
train<-sample(seq_len(nrow(glassdata_normalize)),size=smp)
traindata<-glassdata_normalize[train,]
trainres<-(dataframecoll[train,])
testdata<-glassdata_normalize[-train,]
testres<-(dataframecoll[-train,])


#Randomly selected  60% rows from the dataframe  and assign them as training data. Selected training data randomly so that we can have  higher probability of choosing data rows for each of the glass type presented in original data file. By choosing training data that way we can predict values of the responses more efficiently for the testing data set.

#Selected 60% values (more than 50% value) for training data. Reason for doing this is that the more data we have to train the model,the more accuracy we can get for predicting the response values for testing data(40% values).

#Q-2D

#Fit  knn model by using
#train parameter as traindata(training data)
#test  parameter as testdata(testing data)
#cl  parameter as trainres (training result)
glassdata_normalize<-as.data.frame(lapply(glassdataframe[,c(2,3,4,5,6,7,8,9,10)],normalize))
dataframecoll=data.frame(y) 
smp<-floor(0.60*nrow(glassdata_normalize))
set.seed(123)
train<-sample(seq_len(nrow(glassdata_normalize)),size=smp)
traindata<-glassdata_normalize[train,]
trainres<-(dataframecoll[train,])
testdata<-glassdata_normalize[-train,]
testres<-(dataframecoll[-train,])
library(class)
knn_req<-knn(train=traindata,test=testdata,cl=trainres,k=5)

#Fit  knn model by using train parameter as traindata(training data), test  parameter as testdata(testing data) and         cl  parameter as trainres (training result) and K-=5;


#Q-2E

#Fit  knn model by using
#train parameter as traindata(training data)
#test  parameter as testdata(testing data)
#cl  parameter as trainres (training result)
glassdata_normalize<-as.data.frame(lapply(glassdataframe[,c(2,3,4,5,6,7,8,9,10)],normalize))
dataframecoll=data.frame(y) 
smp<-floor(0.60*nrow(glassdata_normalize))
set.seed(123)
train<-sample(seq_len(nrow(glassdata_normalize)),size=smp)
traindata<-glassdata_normalize[train,]
trainres<-(dataframecoll[train,])
testdata<-glassdata_normalize[-train,]
testres<-(dataframecoll[-train,])
knn_req<-knn(train=traindata,test=testdata,cl=trainres,k=5)	
install.packages('e1071', repo='http://nbcgib.uesc.br/mirrors/cran/')
library(e1071)
library(class)
library(caret)
confusionMatrix(testres,knn_req)


#As we can see by the Confusion matrix statistics that  we have accuracy 
#(58+21)/(58+4+3+21) =(79)/(86)=91.86%


#Q-2F

library(class)
library(caret)
seq<-c(3,5,7,9,11,13)
for(val in seq)
{
  knn_req<-knn(train=traindata,test=testdata,cl=trainres,k=val)
  print("confusion matrix for k=")
  print(val)
  print(confusionMatrix(testres,knn_req))
}


#Wrote the loop that computes the testing accuracy for odd k values ranging 3 to floor(sqrt(no of observations))
#I have taken  odd numbers ranging from 3 to floor(sqrt(no of observations) as reasonable value of k.Reason of taking only odd number is that there are no tie situations  in predicting response vector values.So there are no ambiguity while choosing value for prediction.


#Q-2G
seq<-c(3,5,7,9,11,13)
Accuracyvector<-integer()

for(val in seq){
  knn_req<-knn(train=traindata,test=testdata,cl=trainres,k=val)
  GetAccuracy<-confusionMatrix(testres,knn_req)
  Accuracyvector<-c(Accuracyvector,GetAccuracy$overall[1])
}
plot(seq,Accuracyvector,xlab="Values of K",ylab="Accuracy")


#From the graph we can see that we can get maximum accuracy of prediction for k value=3.
#Optimal value of k=3.

#Q-2H

glassdata_normalize<-as.data.frame(lapply(glassdataframe[,c(2,3,4,5,6,7,8,9,10)],normalize))
dataframecoll=data.frame(glassdataframe$Type) 
smp<-floor(0.60*nrow(glassdata_normalize))
set.seed(123)
train<-sample(seq_len(nrow(glassdata_normalize)),size=smp)
traindata<-glassdata_normalize[train,]
trainres<-(dataframecoll[train,])
testdata<-glassdata_normalize[-train,]
testres<-(dataframecoll[-train,])
library(class)
knn_req<-knn(train=traindata,test=testdata,cl=trainres,k=5)
confusionMatrix(testres,knn_req)

#Most frequent class is Glass Type(ranging from 1,2,3,4,5,6,7) in glassdataframe
#Predicting the value of the most frequent class(null accuracy) we got is 59.3%

#Q-2Bonus)

normalize<-function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}
glassdata_normalize<-as.data.frame(lapply(glassdataframe[,c(4,5,8)],normalize))
dataframecoll=data.frame(y)
smp<-floor(0.60*nrow(glassdata_normalize))
set.seed(123)
train<-sample(seq_len(nrow(glassdata_normalize)),size=smp)
traindata<-glassdata_normalize[train,]
trainres<-(dataframecoll[train,])
testdata<-glassdata_normalize[-train,]
testres<-(dataframecoll[-train,])
knn_req<-knn(train=traindata,test=testdata,cl=trainres,k=5)
table(testres,knn_req)
confusionMatrix(testres,knn_req)  #Accuracy 95.35%


#I took Mg,Al and Ca as a good predictor and by redoing the exercise , I got higher accuracy
#95.35%  than if I would choose vector collection (RI,Na,Mg,Al,Si,K,Ca,Ba,Fe,Type) as a predictor.

