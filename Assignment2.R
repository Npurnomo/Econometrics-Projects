# Econometrics 2
# Assignment 2 coded by Nico Purnomo, Natasya Lucky, and Abraham

# Clear workspace
rm(list=ls())
#Setting working directory
setwd("~/Documents/Uni/Subjects/2019 Sem 1/Econometrics 2/Ass/2")

# Load packages
library(MASS)
library(stargazer)

# SECTION 1
# Question 3
set.seed(42)
n=50
U=rnorm(n,0,1)
V=rnorm(n,0,1)
X=c()
Y=c()
a=1
d=2
for(i in (1:n)){
  Y[i]=(a*V[i]+U[i])/(1-a*d)
  X[i]=(d*U[i]+V[i])/(1-a*d)
}
table<- data.frame(X,Y)
print(table)
eq1<-lm(Y~X)
print(summary(eq1))
stargazer(eq1,type="html",
          title="Simultaneous Causal Equation", out="simult.htm")

# SECTION 2
journals = read.csv("A2_Data.csv")

#Question 1
#Creating new variables
journals$p.cite=journals$libprice/journals$citestot
journals$age=2000-journals$date1
journals$char=(journals$pages*journals$charpp)/1000000

#Generate summary statistics
summary(journals$p.cite)
summary(journals$age)
summary(journals$char)

#Plot histograms for the three variables
hist(journals$p.cite,
     breaks=10,
     xlab="Price/Citation(dollars)",
     main="Histogram of Price/Citation")
hist(journals$age,
     breaks=15,
     xlab="Age of Journals (Years))",
     main="Age of Journals in the Year 2000")
hist(journals$char,
     breaks=10,
     xlab="Number of Characters per Journal (1,000,000)",
     main="Number of Characters per Journal",
     ylim=c(0,50))

#Question 2
#Create log variables
logoclc=log(journals$oclc)
logp.cite=log(journals$p.cite)
logage=log(journals$age)
logchar=log(journals$char)

#Create interaction variables
logagepcite=logage*logp.cite

#Model 1
model1=lm(logoclc~logp.cite, data=journals)
print(summary(model1))
#Model 2
model2=lm(logoclc~logp.cite+logage+logchar)
print(summary(model2))
#Model 3
model3=lm(logoclc~logp.cite+logage+logagepcite+logchar)
print(summary(model3))

stargazer(model1,model2,model3,type="html",
          title="OCLC on Price per Citation", out="allmodels.htm")

#Question 3
#P.cite on society
#Conditional expectations
#Price per citation when society = 1
society1<- subset(journals,society==1,select=p.cite)
print(summary(society1))
#Price per citation when society = 0
society0<- subset(journals,society==0,select=p.cite)
print(summary(society0))
#OLS of price per citation on society
modelsoc=lm(p.cite~society,data=journals)
print(summary(modelsoc))

#Price per citation on nonprofit
#Conditional expectations
#Price per citation when nonprofit = 1
nonprofit1<- subset(journals,nonprofit==1,select=p.cite)
print(summary(nonprofit1))
#Price per citation when nonprofit = 0
nonprofit0<- subset(journals,nonprofit==0,select=p.cite)
print(summary(nonprofit0))
#OLS of price per citation on nonprofit
modelnonp=lm(p.cite~nonprofit,data=journals)
print(summary(modelnonp))
stargazer(modelsoc,modelnonp,type="html",
          title="Price per Citation on Society and Nonprofit", out="modelsocnon.htm")

#Question 4
#Creating subsets of different ranked journals
astar <- subset(journals,rank=="Astar",select=age)
ranka <- subset(journals,rank=="A",select=age)
rankb <- subset(journals,rank=="B",select=age)
rankc <- subset(journals,rank=="C",select=age)

#Creating histograms
hist(astar$age,
     breaks=10,
     xlim=c(0,151),
     main="Histogram of Age of Astar Journals in the year 2000",
     xlab="Age of Journals (years)")
hist(ranka$age,
     breaks=10,
     xlim=c(0,151),
     main="Histogram of Age of A-ranked Journals in the year 2000",
     xlab="Age of Journals (years)")
hist(rankb$age,
     breaks=10,
     xlim=c(0,151),
     main="Histogram of Age of B-ranked Journals in the year 2000",
     xlab="Age of Journals (years)")
hist(rankc$age,
     breaks=10,
     main="Histogram of Age of C-ranked Journals in the year 2000",
     xlab="Age of Journals (years)")

#Creating dummy variables for journal ranks
dastar <- as.numeric(journals$rank=="Astar")
da <- as.numeric(journals$rank=="A")
db <- as.numeric(journals$rank=="B")
dc <- as.numeric(journals$rank=="C")
modelage=lm(age~dastar+da+db,data=journals)
print(summary(modelage))
stargazer(modelage,type="html",
          title="Journals Age on Rank", out="modelage.htm")

#Question 6
#Creating new variable p.cite2
journals$p.cite2=journals$libprice/journals$citestot2
logpcite2=log(journals$p.cite2)

#First stage regression of 2SLS
#Obtain fitted values of p.cite from p.cite2
stage1=lm(logp.cite~logpcite2+logage+logchar,data=journals)
print(summary(stage1))
stargazer(stage1,type="html",
          title="First Stage of 2SLS (p.cite on p.cite2)", out="stage1.htm")
#To obtain correct 2SLS standard errors, use AER
library("AER")
stage2=ivreg(logoclc~logp.cite+logage+logchar|logpcite2+logage+logchar,data=journals)
print(summary(stage2))
stargazer(stage2,type="html",
          title="Second Stage of 2SLS", out="stage2.htm")

#Comparison table of Model 2 from 2.2 and 2.6
stargazer(model2,stage2,type="html",
          title="Model 2 in 2.2 and 2.6", out="compare.htm")
