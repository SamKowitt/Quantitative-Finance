#==================================================#
#                                                  #
# AAPL vs SPX- Hedging                             #
#                                                  #
#==================================================#

#Install and load PortfolioAnalytics package
if (!require("PerformanceAnalytics")) {
  install.packages("PerformanceAnalytics")
  library(PerformanceAnalytics)
}

#==================================================#
#Task 1 - Hedging                                  #
#==================================================#

#Imagine to have some data stored in few CSV files.
AAPL <- Return.read("AAPL.csv", frequency="d")
SPX <- Return.read("SPX.csv", frequency="d")

par(mfrow=c(2,1))
par(mar = rep(2, 4))
plot(AAPL[,5])
plot(SPX[,5])
#use Return.calculate to calculate the geometric returns 
AAPL.returns <- Return.calculate(AAPL[,5],"method"="log")
SPX.returns <- Return.calculate(SPX[,5],"method"="log")
AAPL.returns <- na.omit(AAPL.returns)
SPX.returns <- na.omit(SPX.returns)

par(mfrow=c(2,1))
par(mar = rep(2, 4))
plot(AAPL.returns)
plot(SPX.returns)


#Required statistics creation
corr=cor(AAPL.returns,SPX.returns)   # Correlation
AAPL.sd=sd(AAPL.returns)             # Standard Deviation of AAPL
SPX.sd=sd(SPX.returns)               # Standard Deviation of SPX
HedgeRatio <- corr*AAPL.sd/SPX.sd    # Hedge Ratio Calculation
HedgeRatio # The result is 1.23, This means that we need 1.23 shares of the SPX future



CAPM.beta(AAPL.returns, SPX.returns, Rf = 0) # Also 1.23 with risk free rate set at 0

spot <- as.numeric(AAPL[c(250:251),1])   # AAPL is the spot
futures <- as.numeric(SPX[c(250:251),1]) # SPX is the future

#Long-short portfolio
Initial.position <- spot[1] - HedgeRatio*futures[1]
as.numeric(Initial.position) #
spot.gain <- spot[2]-spot[1] #spot only - NEG
spot.gain
futures.gain <- -(HedgeRatio*futures[2] - HedgeRatio*futures[1])  
futures.gain #futures only - positive

New.position <- spot[2] - HedgeRatio*futures[2]
New.position

gain.fromg.hedge <- (New.position - Initial.position) 
gain.fromg.hedge

# The Spot was (-) and the Future was (+). Therefore our new position is the last obs of the spot minus the hedge ratio times the 
# last observation of the future. Using this we calculate our overall position This gives us a (+44.5) value
