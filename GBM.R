library(quantmod)
library(data.table)
library(ggplot2)
library(dplyr)

#----Parameters Estimation---- 
# Pseuduocode for GBM simulation:
gbm.f = function(n,s0,mu,sigma){
  t= 1# time horizon
  t.s= seq(0,t,length= n+1)# time step
  dt=t/n #change in time
  Bt= sqrt(dt)*cumsum(rnorm((n+1),0,1))
  St= s0*exp((mu-sigma^2/2)*t.s+sigma*Bt)
  St
  }
rt=gbm.f()
plot(rt.type='l',col='purple', ylab='Simulated Returns', xlab='Time')


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


#----Monte Carlo simulation for drift and Volatility----
#Pseudocode for simulating gbm parameters
gbm.sim.est= function (nsim,n, mu,sigma,s0){
  #set matrix for drift and volatility
  est.vals=matrix(NA,nrow=nsim, ncol=2)
  #set looping
  for (i in 1:nsim){
    R= gbm.f(n,mu,sigma,s0)# returns
    est.vals[i,1]=drift.f(R)# store the drift coeff for each return
    est.vals[i,2]=volt.f(R)# store the volatility coeff for each return
    df= as.data.frame(est.vals)
  }
  colnames(df) = c ("Mu","Sigma")
  df# result
}


#sim.ret= gbm.sim.est(nsim=1000,n=500,mu =0.05, sigma=0.15,s0=10)
#head(sim.ret)
#apply(sim.ret,2,mean)
#hist(sim.ret[,1],nclass=30,col='blue', 
#     main='Sampling')





#----Forcasting Simulation using GBM----
set.seed(12206)
#simulated stock price
ssp =gbm.f(n=500,mu=0.05,sigma=0.15,s0=10)

#set train and test data set
train_dat=ssp[1:400]
test_dat=ssp[400:500]

# set the training set
train_dat.ts=ts(train_dat)# transform to time series object
plot(train_dat.ts,main="", ylab='Return', col='blue')


#set the initial value
s0= test_dat[1]

#Create drift and volatility equations
d= expression(theta[1]*x)# drift
s=expression(theta[1]*x)# volatility
