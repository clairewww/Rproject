#==========================================================
## SET UP R 
#==========================================================

# You should generally clear the working space at the start of every R session
rm(list = ls())

# Set the directory
setwd("~/Downloads/Santa Clara University/OMIS 2392_Econometrics with R/Final project")

# install packages
#install.packages("ggeffects")
#install.packages("QuantPsyc")
#install.packages("VIF")
#install.packages("usdm")
#install.packages("lmtest")
#install.packages("multiwayvcov")
#install.packages("sandwich")
#install.packages("AER")
#install.packages("aod")
#install.packages("mfx")
#install.packages("readstata13")


# Load libraries everytime you start a session
library(stargazer)
library(gdata)
library(ggplot2)
library(psych) 
library(ggeffects)
library(QuantPsyc)
library(usdm)
library(lmtest)
library(multiwayvcov)
library(sandwich)
library(foreign)
library(AER)
library(aod)
library(Rcpp)
library(mfx)
library(nnet)
library(reshape2)
library(VIF)
library(MASS)
library(readstata13)

# turn off scientific notation except for big numbers. 
options(scipen = 9)

#==========================================================
## LOAD AND EXPLORE DATA
#==========================================================

bmsr=read.dta13("~/Downloads/Santa Clara University/OMIS 2392_Econometrics with R/Final project/BM store monthly sales-returns.dta")
bmp=read.dta13("~/Downloads/Santa Clara University/OMIS 2392_Econometrics with R/Final project/BM store monthly prod_cat sales-returns.dta")
olsr=read.dta13("~/Downloads/Santa Clara University/OMIS 2392_Econometrics with R/Final project/Online store daily sales-returns.dta")
olp=read.dta13("~/Downloads/Santa Clara University/OMIS 2392_Econometrics with R/Final project/Online store daily prod_cat sales-returns.dta")

# Summary statistics
stargazer(bmsr, type="text", median=TRUE, iqr=TRUE,digits=1, title="BM store monthly sales-returns")  # basic descriptive statistics
stargazer(bmp, type="text", median=TRUE, iqr=TRUE,digits=1, title="BM store monthly prod_cat sales-returns") 
stargazer(olsr, type="text", median=TRUE, iqr=TRUE,digits=1, title="Online store daily sales-returns") 
stargazer(olp, type="text", median=TRUE, iqr=TRUE,digits=1, title="Online store daily prod_cat sales-returns") 

ggplot(bmsr, aes(x=salesvalue)) + geom_histogram(colour="green")
ggplot(bmsr, aes(x=log(salesvalue))) + geom_histogram(colour="green")

bmsr$policy_dummy=ifelse(bmsr$policy=="60 days",0,1)
bmsr$time_dummy=ifelse(bmsr$month_index<51,0,1)
bmp$policy_dummy=ifelse(bmp$policy=="60 days",0,1)
bmp$time_dummy=ifelse(bmp$month_index<51,0,1)
olsr$policy_dummy=ifelse(olsr$policy=="60 days",0,1)
olsr$time_dummy=ifelse(olsr$year==2013 & olsr$month_dummy<10,0,1)
olp$policy_dummy=ifelse(olp$policy=="60 days",0,1)
olp$time_dummy=ifelse(olp$year==2013 & olp$month_dummy<10,0,1)


m_olsr = olsr
for(i in 1:ncol(olsr)){
  m_olsr[is.na(m_olsr[,i]), i] <- mean(m_olsr[,i], na.rm = TRUE)
}


#Q1

df = data.frame(m_olsr$policy_dummy,m_olsr$salesquantity,m_olsr$time_dummy,m_olsr$avg_female,m_olsr$avg_age,m_olsr$avg_income,m_olsr$avg_homeowner,m_olsr$avg_residency,m_olsr$avg_childowner)

cor(df) # Generates the correlation matrix
vifcor(df) #  VIF scores are less than 3, no indication of multicollinearity
#no multicollinearity


