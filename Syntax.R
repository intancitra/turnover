#LOAD DATA
setwd("C:/Users/Lenovo/Downloads")
data=read.table("turnover.csv",header=TRUE,sep=",")
summary(turnover)

#LOAD LIBRARY SURVIVAL
library("survival")
library("dplyr")

#Subsetting Data
turnover=subset(data, industry %in% c("manufacture","Retail","IT","Banks"))

#DEFINING SURVIVAL DATA
#Event = 1 (Non-censored)
Y=Surv(turnover$stag,turnover$event==1)

#Kurva Kaplan Meier untuk keseluruhan data
plot(survfit(Surv(turnover$stag,turnover$event==1)~1),
     main="KM curves for survival time")

#Kurva Kaplan Meier by Gender (Female vs Male)
kmfit1=survfit(Y~turnover$gender)
plot(kmfit1, fun="cloglog",xlab="time in days using logarithmic scale",
     ylab="log-log survival", main="Log-log curves by gender",
     lty=c("solid", "dashed"), col=c("red", "blue"))
survdiff(Surv(stag,event)~gender, data=turnover)

#Kurva Kaplan Meier by Industry
kmfit2=survfit(Y~turnover$industry)
plot(kmfit2, fun="cloglog",xlab="time in days using logarithmic scale",
     ylab="log-log survival", main="Log-log curves by industry")
survdiff(Surv(stag,event)~industry, data=turnover)

#Model Cox PH
mod1=coxph(Y~industry + age + gender,data=turnover)
mod1

#Cek Asumsi PH dengan GOF Test
cox.zph(mod1, transform = rank)
plot(cox.zph(mod1,transform=rank),se=F,var="gender")

#Model Cox PH tanpa Interaksi
mod2=coxph(Y~ industry + age + gender+gender*industry
           + gender*age,data=turnover, method='efron')
mod2

#Pengujian Model Terbaik
lr=(-2)*(mod1$loglik[2]-mod2$loglik[2])
lr
Pvalue = 1 - pchisq(lr, 2)
Pvalue

#Uji Signifikansi Parameter
summary(mod1)

#Adjusted Survival for industry=Retail, age=30, gender=f
pattern1=data.frame(industry="Retail",age=30,gender="f")
summary(survfit(mod1, newdata=pattern1))

plot(survfit(mod1,newdata=pattern1),conf.int=F,
     main="Adjusted survival for industry='Retail', age=30, gender='f'")
