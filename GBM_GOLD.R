library(quantmod)
library(data.table)
library(ggplot2)
library(dplyr)
library(Sim.DiffProc)


#load the dataset
dat<-getSymbols("GC=F",
          from = "2023-02-01", 
          auto.assign = FALSE)

View(dat)
#extract close price
GC <- dat$`GC=F.Close`




#----Forcasting Simulation using GBM----
#set the test and train data
test_dat=GC[216:269]
train_dat=GC[1:215]

#forcasting train set
train_dat.ts=ts(train_dat)# transform to time series object
plot(train_dat.ts,main="Train_Forecast", ylab='Return_prices',xlab='Days', col='blue')

#set the initial value
s0= test_dat[1]


#Create drift and volatility equations
#d= expression(theta[1]*x)# drift
#s=expression(theta[1]*x)# volatility



#----Parameters Estimation---- 
# Pseuduocode for drift mean and standard deviation
drift.f= function( S,lag=1){
  n<-length(S)#number of return in the sample
  #set condition for lag diff
  if (n<1+lag){
    stop("S must be greater than 2+ lag")
  }
  nxt.val= S[(1+lag):n]#define the value for the next day
  crt.val= S[1:(n-lag)]#define the value for the current day
  t=1# time horizon(daily)
  dt=t/n# change in time
  stk.R= (nxt.val-crt.val)/crt.val
  mu.hat=sum(stk.R)/(n*dt)
  mu.hat#display result
}
drift.f(train_dat.ts)

# Pseuduocode for drift volatility
volt.f= function(S,lag=1){
  n = length(S)#number of return in the sample
  #set condition for lag diff
  if (n<1+lag){
    stop("S must be greater than 2+ lag")
  }
  nxt.val= S[(1+lag):n]#define the value for the next day
  crt.val= S[1:(n-lag)]#define the value for the current day
  t=1# time horizon(daily)
  dt=t/n# change in time
  stk.R= (nxt.val-crt.val)/crt.val
  R.bar=mean(stk.R)
  sig.var=sum((stk.R-R.bar)^2)/((n-1)*dt)
  sig.hat= sqrt(sig.var)
  sig.hat#display result
}
volt.f(train_dat.ts)

#Assigned estimated values to defined objects
drift= drift.f(train_dat.ts)
diffusion= volt.f(train_dat.ts)

#Create drifts and diffusion eaquations from the estimated values
d=eval(substitute(expression(drift*x),list(drift=drift)))
s=eval(substitute(expression(diffusion*x),list(diffusion=diffusion)))

#Number of simulation
nsim=1000

#sum of the simulatated values
pred_x= rep(0,53)
all_x=data.frame()
for(i in 1:nsim){
  #create a new random seed for each simulation
  rand = as.integer(1000*runif(1))
  set.seed(rand)
  
  #Simulate the SDE using the Euler method for 54 days
  X= snssde1d(N=53, x0=s0, Dt=1/216, drift=d,diffusion=s, method="euler",M=1)
  pred_x= pred_x+ X$X
  all_x = rbind(all_x, as.numeric(X$X))
}

pred_x= pred_x/nsim#mean value
pred_x#dispaly predicted values

#standard deviation
sd_x= sapply(all_x,sd)
#Comparing actual vs predicted values
data.frame(test_dat,pred_x)
#Mean Absolute Percentage Error(MAPE)
mape.f= function(a.val,p.val){
  (1/length(a.val))*sum(abs((a.val-p.val)/a.val))*100
}
mape.f(test_dat,pred_x)


# Create upper lower confidence bounds(.99 or .96)
upper =pred_x +1.96*sd_x
lower= pred_x - 1.96*sd_x
cint= data.frame(lower, upper)
cint

#plot
c=ggplot()
c=c+geom_line(aes(x=1:215,y=train_dat, color='Price')) 
c=c+geom_line(aes(x=216:269,y=test_dat, color='Price test')) 
c=c+geom_line(aes(x=216:269,y=pred_x, color='variable predicted')) 
c=c+xlab('Days')
c=c+ylab('Prices')
c=c+ggtitle('GBM Forecast for Gold ')
c
