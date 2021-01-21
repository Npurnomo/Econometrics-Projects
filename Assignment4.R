# Econometrics 2 coded by Nico Purnomo, Natasya Lucky, and Abraham
# Assignment 4

# Clear workspace
rm(list=ls())
#Setting working directory
setwd("~/Documents/Uni/Subjects/2019 Sem 1/Econometrics 2/Ass/4")

# Load packages
library(MASS)
library(stargazer)
library(AER)
library(sandwich)
library(xtable)
library(texreg)
library(zoo)

# Section 1
# Question 2
#Generate sample of 2000 observations
set.seed(42)
Vt=mvrnorm(2000,mu=0,Sigma=1)
ni=c(50,100,500)

#Compute Moving Sums for each width
MS1=rollsum(Vt,50)
MS2=rollsum(Vt,100)
MS3=rollsum(Vt,500)

#Plot Vt, MS1, MS2, and MS3
plot(Vt,main="Vt")
plot(MS1,main="MS 50t")
plot(MS2,main="MS 100t")
plot(MS3,main="MS 500t")

#Change data into time series
Vtts=ts(Vt, start=1, end=2000)
Ms1ts=ts(MS1, start=1, end=1951)
Ms2ts=ts(MS2, start=1, end=1901)
Ms3ts=ts(MS3, start=1, end=1501)

#Plot ACF
acf(Vtts,lag.max=40,main="ACF Vt")
acf(Ms1ts, lag.max=40,main="ACF MS 50t")
acf(Ms2ts, lag.max=40,main="ACF MS 100t")
acf(Ms3ts, lag.max=40,main="ACF MS 500t")

# Question 3
set.seed(42)
Vt=mvrnorm(2000,mu=0,Sigma=1)
n=2000
a0=10
a1=5
Y=c()

for(i in (1:n)){
  if(n>1000){Dt=1} else {Dt=0}
  Y[i]=a0+a1*Dt+Vt[i]
}
eq1=lm(Y~Dt)

# Question 4



# Section 2
library(dynlm)
library(vars)
dt=read.csv("A4_Data.csv")

#Question 1
inc=ts(dt$inc, start=1946, end=2018)
cons=ts(dt$cons, start=1946, end=2018)
inc.2015=window(inc,end=2015)
cons.2015=window(cons,end=2015)
diffcons=diff(log(cons),lag=1)
diffcons.2015=window(diffcons,end=2015)

#AIC for lags p=1,2,3,4,5
maxlag=5
aic=matrix(nrow=maxlag,ncol=1)
for(j in 1:maxlag){
  aic[j]=AIC(dynlm(diffcons.2015~L(diffcons.2015,1:j)))
}
print(aic)

#ACF for p=1
ar1=dynlm(diffcons.2015~L(diffcons.2015,1))
acf(ar1$residuals,lag.max=10, main="Residual ACF of AR(1)")

#Using unadjusted R-squared
maxlag=5
rsq=matrix(nrow=maxlag,ncol=1)
for(j in 1:maxlag){
  req=dynlm(diffcons.2015~L(diffcons.2015,1:j))
  rsq[j]=summary(req)$r.squared
}
print(rsq)

#Question 2
#AR(2) Model
ar2=dynlm(diffcons.2015~L(diffcons.2015,1:2))
print(coeftest(ar2,vcov=vcovHC(ar2)))

#Specify coefficients for AR(2)
beta0.hat=ar2$coefficients[1]
beta1.hat=ar2$coefficients[2]
beta2.hat=ar2$coefficients[3]

#Forecast log difference for cons 2016,2017,2018
n=2015
cons2016=beta0.hat+beta1.hat*diffcons.2015[n]+beta2.hat*diffcons.2015[n-1]
cons2017=beta0.hat+beta1.hat*cons2016+beta2.hat*diffcons.2015[n]
cons2018=beta0.hat+beta1.hat*cons2017+beta2.hat*cons2016

#Question 3
#Create log difference for inc
diffinc=diff(log(inc),lag=1)
diffinc.2015=window(diffinc,end=2015)
diffconsinc=cbind(diffcons.2015,diffinc.2015)

#AIC for lags p=1,2,3,4,5
maxlag=5
aic3=matrix(nrow=maxlag,ncol=1)
for(j in 1:maxlag){
  aic3[j]=AIC(VAR(diffconsinc,p=j,type="const"))
}
print(aic3)

#ACF for p=1
VAR1=VAR(diffconsinc,p=1,type="const")
VAR.cons <- dynlm(diffcons.2015~L(diffcons.2015,1)+L(diffinc.2015,1))
VAR.inc <- dynlm(diffinc.2015~L(diffcons.2015,1)+L(diffinc.2015,1))
acf(VAR.cons$residuals,lag.max=10,main="Residual ACF of ARDL(1) 
    with Dependent Variable diffcons")
acf(VAR.inc$residuals,lag.max=10, main="Residual ACF of ARDL(1) 
    with Dependent Variable diffinc")

#Question 4
#Using p=2
VAR.cons2 <- dynlm(diffcons.2015~L(diffcons.2015,1:2)+L(diffinc.2015,1:2))
VAR.inc2 <- dynlm(diffinc.2015~L(diffcons.2015,1:2)+L(diffinc.2015,1:2))
mu.hat=VAR.cons2$coefficients[1]
phi1.hat=VAR.cons2$coefficients[2]
phi2.hat=VAR.cons2$coefficients[3]
lambda1.hat=VAR.cons2$coefficients[4]
lambda2.hat=VAR.cons2$coefficients[5]

al0.hat=VAR.inc2$coefficients[1]
al1.hat=VAR.inc2$coefficients[2]
al2.hat=VAR.inc2$coefficients[3]
bet1.hat=VAR.inc2$coefficients[4]
bet2.hat=VAR.inc2$coefficients[5]

cons22016=mu.hat+phi1.hat*diffcons.2015[n]+phi2.hat*diffcons.2015[n-1]+lambda1.hat*diffinc.2015[n]+lambda2.hat*diffinc.2015[n-1]
inc22016=al0.hat+al1.hat*diffcons.2015[n]+al2.hat*diffcons.2015[n-1]+bet1.hat*diffinc.2015[n]+bet2.hat*diffinc.2015[n-1]
cons22017=mu.hat+phi1.hat*cons22016+phi2.hat*diffcons.2015[n]+lambda1.hat*inc22016+lambda2.hat*diffinc.2015[n]
inc22017=al0.hat+al1.hat*cons22016+al2.hat*diffcons.2015[n]+bet1.hat*inc22016+bet2.hat*diffinc.2015[n]
cons22018=mu.hat+phi1.hat*cons22017+phi2.hat*cons22016+lambda1.hat*inc22017+lambda2.hat*inc22016
cons22016
cons22017
cons22018

#Question 5

#https://heuristically.wordpress.com/2013/07/12/calculate-rmse-and-mae-in-r-and-sas/
# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}  


#RMSE for AR(2)
RMSEAR=sqrt(((diffcons[n+1]-cons2016)^2+(diffcons[n+2]-cons2017)^2+(diffcons[n+3]-cons2018)^2)/3)

#RMSE for VAR(2)
RMSEVAR=sqrt(((diffcons[n+1]-cons22016)^2+(diffcons[n+2]-cons22017)^2+(diffcons[n+3]-cons22018)^2)/3)
