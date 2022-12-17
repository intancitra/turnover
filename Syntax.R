# LOAD DATA
setwd("C:/Users/Lenovo/Downloads")
data=read.table("turnover.csv",header=TRUE,sep=",")
summary(turnover)

# LOAD LIBRARY
library("survival")
library("dplyr")

# Subsetting Data with several industries
turnover=subset(data, industry %in% c("manufacture","Retail","IT","Banks"))

# DEFINING SURVIVAL DATA
# Event = 1 (Non-censored)
Y=Surv(turnover$stag,turnover$event==1)

## Kurva Kaplan Meier untuk keseluruhan data ##
surv = survfit(Surv(turnover$stag,turnover$event==1)~1)
plot(surv, xlab="time in days", ylab="s(t)",
     main="KM curves for survival time")
# Mark the median
abline(a=.5, b=0)
# Get the median
surv

## Kurva Kaplan Meier by Gender (Female vs Male) ##
kmfit1=survfit(Y~turnover$gender)
kmfit1
plot(kmfit1, xlab="time in days",
     ylab="s(t)", main="KM curves by gender",
     lty=c("solid", "dashed"), col=c("red", "blue"))
legend("topright", fill=c("red", "blue"), inset=.1, legend=c("Female", "Male"))
abline(a=.5,b=0)

#Uji Log Rank KM
survdiff(Surv(stag,event)~gender, data=turnover)

## Kurva Kaplan Meier by Industry ##
kmfit2=survfit(Y~turnover$industry)
unique(turnover$industry)
kmfit2
plot(kmfit2,xlab="time in days",
     ylab="s(t)", main="KM curves by industry",
     col=c("red","blue","green","black"))
legend("topright", fill=c("red","blue","green","black"), inset=.1, 
       legend=c("Banks", "IT", "Manufacture", "Retail"))
abline(a=.5,b=0)

#Uji Log Rank KM
survdiff(Surv(stag,event)~industry, data=turnover)

## Model Cox PH ##
mod1=coxph(Y~industry + age + gender,data=turnover)
mod1

## Model Cox PH dengan Interaksi ##
mod2=coxph(Y~ industry + age + gender+gender*industry
           + gender*age,data=turnover, method='efron')
mod2

## Uji Asumsi PH ##

# Kurva Log-Log by Gender (Female vs Male)
plot(kmfit1, fun="cloglog",xlab="time in days using logarithmic scale",
     ylab="log-log survival", main="Log-log curves by gender",
     lty=c("solid", "dashed"), col=c("red", "blue"))
legend("bottomright", fill=c("red", "blue"), inset=.1, legend=c("Female", "Male"))

# Kurva Log-Log by Industry
plot(kmfit2, fun="cloglog",xlab="time in days using logarithmic scale",
     ylab="log-log survival", main="Log-log curves by industry",
     col=c("red","blue","green","black"))
legend("bottomright", fill=c("red","blue","green","black"), inset=.1, 
       legend=c("Banks", "IT", "Manufacture", "Retail"))

# Cek Asumsi PH dengan GOF Test
cox.zph(mod1, transform = rank)
# Cek Asumsi PH dengan plot GOF
plot(cox.zph(mod1,transform=rank),se=F,var="gender",main="PH Assumption by Gender")
plot(cox.zph(mod1,transform=rank),se=F,var="industry",main="PH Assumption by Industry")

## Pengujian Model Terbaik ##
lr=(-2)*(mod1$loglik[2]-mod2$loglik[2])
lr
Pvalue = 1 - pchisq(lr, 2)
Pvalue

## Uji Signifikansi Parameter ##
summary(mod1)

## Adjusted Survival ##
summary(turnover)
turnover$gender = factor(turnover$gender)
turnover$industry = factor(turnover$industry)

# We get the modes for industry is Retail, for gender is Female
# and the median for age is 30, hence the pattern is showed in pattern1

pattern1=data.frame(industry="Retail",age=30,gender="f")
summary(survfit(mod1, newdata=pattern1))
survfit(mod1,newdata=pattern1)

# Plotting the adjusted survival curves
plot(survfit(mod1,newdata=pattern1),conf.int=T,
     main="Adjusted survival for industry='Retail', age=30, gender='f'")
abline(a=.5,b=0)
