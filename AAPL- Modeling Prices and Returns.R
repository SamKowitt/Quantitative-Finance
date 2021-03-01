#===================================================================================#
#                                                                                   #
#     Apple: Modeling Prices and Returns (Arithmetic vs Geometric Returns)          #
#                                                                                   #
#===================================================================================#


# Download the time series for Apple (AAPL)
start <- as.Date("2018-01-01")
end <- as.Date("2018-10-01")
getSymbols("AAPL", src = "yahoo", from = start, to = end)

AAPL.returns <- allReturns(AAPL)

View(AAPL.returns)

AAPL.A.returns <- periodReturn(AAPL[, "AAPL.Adjusted"],period='daily',subset=NULL,type='arithmetic',leading=TRUE)
AAPL.G.returns <- periodReturn(AAPL[, "AAPL.Adjusted"],period='daily',subset=NULL,type='log',leading=TRUE)

View(AAPL.A.returns*100)
View(AAPL.G.returns*100)

min(AAPL.A.returns*100)
min(AAPL.G.returns*100)

# Histogram and Normal fit
par(mfrow=c(1,2))
aapl.hist1 = hist(AAPL.A.returns,plot=F,breaks=30)
hist(AAPL.A.returns,main="Arithmetic", col="cornflowerblue",
     xlab="returns",
     breaks=aapl.hist1$breaks)
xfit<-seq(min(AAPL.A.returns),max(AAPL.A.returns),length=40) 
yfit<-dnorm(xfit,mean=mean(AAPL.A.returns),sd=sd(AAPL.A.returns)) 
yfit <- yfit*diff(aapl.hist1$mids[1:2])*length(AAPL.A.returns) 
lines(xfit, yfit, col="red", lwd=2)

aapl.hist2 = hist(AAPL.G.returns,plot=F,breaks=30)
hist(AAPL.G.returns,main="Geometric", col="cornflowerblue",
     xlab="returns",
     breaks=aapl.hist2$breaks)
xfit<-seq(min(AAPL.G.returns),max(AAPL.G.returns),length=40) 
yfit<-dnorm(xfit,mean=mean(AAPL.G.returns),sd=sd(AAPL.G.returns)) 
yfit<-dnorm(xfit,mean=0,sd=0.01) 
yfit <- yfit*diff(aapl.hist2$mids[1:2])*length(AAPL.G.returns) 
lines(xfit, yfit, col="red", lwd=2)

# We can see here that geometric returns better fit the distribution line and therefore should be our first choice between these two