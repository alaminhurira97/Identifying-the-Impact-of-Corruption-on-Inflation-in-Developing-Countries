# Research Project
setwd("D:/OU/Econometrics/Research Project/Data/updated")
library(readxl) # adding the package readr 
Mydata0<-read_excel("WDI_Data.xlsx",na="..",range="A1:K1723")
ir<-is.na(Mydata0$CPIAT)
Mydata<-Mydata0[Mydata0$CountryName!="Korea, Dem. People's Rep.",]
Mydata<-as.data.frame(Mydata) 

## Descriptive statistics 
library(pastecs)
stat.desc(Mydata)

## checking what type of model I have to estimate
library(gplots)
plotmeans(Inflation~Time,data=Mydata)# inflation over the time
plotmeans(Inflation~CountryName,data=Mydata)# inflation across countries

# Model 1: Estimating LRM using twoways fixed effect model (baseline model)
library(plm)
m1<-plm(Inflation~CPIAT+GDP+Mt+OPENNESS+log(ER),data=Mydata,index=c("CountryName","Time"),effect="twoways")
summary(m1)

# Model 2: Estimating LRM using twoways fixed effect model with dropping OPENNESS regressor
m2<-plm(Inflation~CPIAT+GDP+Mt+log(ER),data=Mydata,index=c("CountryName","Time"),effect="twoways")
summary(m2)

# Model 3: Estimating LRM using only country effect
m3<-plm(Inflation~CPIAT+GDP+Mt+log(ER),data=Mydata,index=c("CountryName","Time"),model="within")
summary(m3)

##### CREATING A TABLE TO REPORT ESTIMATED MODELS USING THE OLS METHOD WITH ROBUST STANDAR ERRORS
### USING THE WHITE ESTIMATOR.

library(tidyverse)
### models to include in the table
## two ways fixed effect model (baseline model)
m1<-plm(Inflation~CPIAT+GDP+Mt+OPENNESS+log(ER),data=Mydata,index=c("CountryName","Time"),effect="twoways")
   
# two ways fixed effect model with dropping OPENNESS regressor
m2<-plm(Inflation~CPIAT+GDP+Mt+log(ER),data=Mydata,index=c("CountryName","Time"),effect="twoways")

# only country effect with dropping OPENNESS regressor
m3<-plm(Inflation~CPIAT+GDP+Mt+log(ER),data=Mydata,index=c("CountryName","Time"),model="within")

## Robust standard errors (RSE) to include in the table: 
## NEwey west HAC matrix is used :vcovNW 
library(sandwich) 
library(lmtest)

# STEP 1: I use function vcovNW to calculate the estimate of the variance-covariance matrix consistent with
# heteroscedasticity and autocorrelation of order 1
# (robust estimated variance) for EACH MODEL
# type=HC0 is the white matrix that we use to calculate a 'robust estimated variances and covariances
# of the beta.hat  

est.varcov.matrix.m1<-vcovNW(m1,maxlag=1)  
est.varcov.matrix.m2<-vcovNW(m2,maxlag=1)
est.varcov.matrix.m3<-vcovNW(m3,maxlag=1)


## STEP 2:  Get the estimates of the "robust" variances from the 'est.varcov.matrix' object
## contains the estimated variances for each beta.hat in the diagonal
### The diag() function extracts those estimates of the variance 

est.var.beta.hat.m1<-diag(est.varcov.matrix.m1)
est.var.beta.hat.m2<-diag(est.varcov.matrix.m2)
est.var.beta.hat.m3<-diag(est.varcov.matrix.m3)


##STEP 3: to calculate the RSE we take the square root of the 'est.var.beta.hat' object

RSEm1<-sqrt(est.var.beta.hat.m1) 
RSEm2<-sqrt(est.var.beta.hat.m2) 
RSEm3<-sqrt(est.var.beta.hat.m3)

###  STEP 4: create a  list with the RSEs calculated for each model. 
### The function list( ) can combine different type of objects (including graphs!)
### This list of RSEs will be used to replace the ordinary and incorrect SE in the table of results.

addRSE<-list(c(RSEm1,RSEm2,RSEm3)) 

## STEP 5. We also need to calculate the F test (joint statistical significance) with RSE for each model
### We use  the function waldtest to avoid typing the whole set of null hypothesis.
### The argument vcov is the estimated variance-covariance matrix that we use to calculate
### the F test.

rftestm1<-pwaldtest(m1,vcov.=vcovNW(m1, maxlag=1)) 
rftestm2<-pwaldtest(m2,vcov=vcovNW(m2, maxlag=1))
rftestm3<-pwaldtest(m3,vcov=vcovNW(m3, maxlag=1))


### STEP 6 : testing heteroscedasticity
ht.m1<-pcdtest(m1,test="lm")
ht.m2<-pcdtest(m2,test="lm")
ht.m3<-pcdtest(m3,test="lm")
ht.m3
## STEP 7 : testing Autocorrelation
at.m1<-pbgtest(m1, order = 1)
at.m2<-pbgtest(m2, order = 1)
at.m3<-pbgtest(m3, order = 1)

#### STEP 8 : We create a list with the calculated Wald test 
### We will add these Wald tests to the table of results. 
### The object rftestm2$F[2] is the f-statistics recorded in the rftestm2 object (similar for rftestm1$F[2]).
### I check the significance to decide the number of stars that I want to add.
### I checked the p-value. It was clsoe to zero, so I add three stars
### The function paste( ) allows to paste different type of data. In this case we paste the F test ( a number)
### with a character (the stars).

addWmore<-list(c("Wald Statistic", paste(round(rftestm1$stat,2),"***"),
		 paste(round(rftestm2$stat,2),"***"),
		 paste(round(rftestm3$stat,2),"***")),
		c("Country fixed effects and time effect?","YES","YES","NO"),
		c("Only country effect?","NO","NO","YES"))
addWmore<-list(c("Wald Statistic", paste(round(rftestm1$stat,2),"***"),
		 paste(round(rftestm2$stat,2),"***"),
		 paste(round(rftestm3$stat,2),"***")),
		c("Heteroscedasticity Test", paste(round(ht.m1$stat,2),"***"),
		 paste(round(ht.m2$stat,2),"***"),
		 paste(round(ht.m3$stat,2),"***")),
		c("Autocorrelation Test", paste(round(at.m1$stat,2),"***"),
		 paste(round(at.m2$stat,2),"***"),
		 paste(round(at.m3$stat,2),"***")),
		c("Country fixed effects and time effect?","YES","YES","NO"),
		c("Only country effect?","NO","NO","YES"))


## I have included a line that shows if the model was calculated with two-way effects or country effects

#### FINAL STEP : we create the table with two estimated models using the function stargazer
### we replace the ordinary SE with RSE using the argument se=addRSE
### We delete the F test calculated with ordinary SE: omit.stat="f"
### we add the correct Wald test with RSE using the argument add.lines

library(stargazer)
stargazer(m1,m2,m3, type = "html",out='mymodels.doc',se=addRSE, omit.stat="f",ci = TRUE,add.lines = addWmore,digits=2)

