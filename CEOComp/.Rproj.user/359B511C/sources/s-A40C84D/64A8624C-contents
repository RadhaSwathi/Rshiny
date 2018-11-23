getwd()
setwd('/Users/rsklanu/RBasics/Statistical Modeling using R Bengaluru/Case Study/stockprices')


stockreturn<- read.csv("stockpricedata.csv", header=TRUE)

str(stockreturn)
summary(stockreturn)
?par
##
hist(stockreturn$ongc)
hist(stockreturn$rtongc)
hist(stockreturn$rtongc,breaks =8,freq = FALSE, main="Histogram of ongc returns",xlab = "returns",ylab = "probabilty")
help(hist)
##induvidual probabilty
dpois(10,15)
dpois(20,15)
##cumulative probability
ppois(8,15)
##histogram for poisson distribution
plot(x,dpois(x,5), xlab="Number of spam",ylab="P(X=x)",type="h",main="spam email,poisson")
plot(x,dpois(x,10), xlab="Number of hits",ylab="P(X=x)",type="h",main="hits on website,poisson")
plot(x,dpois(x,15), xlab="Number of Customer",ylab="P(X=x)",type="h",main="Customers in bigbazar,poisson")
plot(x,dpois(x,20), xlab="Number of Customer",ylab="P(X=x)",type="h",main="Customers in  bank,poisson")
##multiple frame graphical parameter
rm(list=ls())
par(mfrow=c(2,2))
x<-0:40

##cummulative
par(mfcol=c(2,3))
plot(x,ppois(x,5), xlab="Number of spam",ylab="P(X=x)",type="s",main="spam email,poisson")
plot(x,ppois(x,10), xlab="Number of hits",ylab="P(X=x)",type="s",main="hits on website,poisson")
plot(x,ppois(x,15), xlab="Number of Customer",ylab="P(X=x)",type="s",main="Customers in bigbazar,poisson")
plot(x,ppois(x,20), xlab="Number of Customer",ylab="P(X=x)",type="s",main="Customers in  bank,poisson")

data1<-data.frame(dpois(x,20))
x1<-data.frame(0:40)
df<-x1*data1
sum(df)
mean(data1)

##Exercise1
fandt<-10:30
plot(fandt,ppois(fandt,20), xlab="Number of spam",ylab="P(X=x)",type="s",main="spam email,poisson")
c<-ppois(fandt,20)

data2<-data.frame(ppois(fandt,20))
df2<- fandt*data2
sum(df2)
df<-x1*data1
sum(df)
sum(c)
ppois(30,20)-ppois(10,20)

##binomial thoery
a<-0:3
reult<-dbinom(x=0:3,size=3,prob=0.9)
sum(reult)
sum(reult*a)
dbinom(x=3,size=3,prob=0.9)
##BINOMIAL PLOT
rm(list=ls())
par(mfrow=c(2,2))
Co<-0:30
plot(Co,dbinom(Co,30,0.95),type="h",main = "Binomial Distribution of success(0.95)")
plot(Co,dbinom(Co,30,0.5),type="h",main = "Binomial Distribution of success(0.95)")
plot(Co,dbinom(Co,30,0.05),type="h",main = "Binomial Distribution of success(0.95)")

##Cummulative binomial distribution
pbinom(2,3,0.7)