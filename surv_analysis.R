#read censored data 
ds = read.csv(file.choose(), header=TRUE)
cdf = as.data.frame(ds)


#install.packages("GGally")
#install.packages("survival")
library(GGally)
library(survival)


detach(mini)
#1000 samples randomly taken 
mini= cdf[sample(nrow(cdf),1000, replace=FALSE, prob = NULL),]
attach(mini)


churn = Churn_90
#derive response variable, referred as survival object
response = Surv(ActivePeriod, churn)

#Kaplain-Meier estimator with no covariates
km.surv = survfit(response ~ 1,type="kaplan-meier",data=mini)
summary(km.surv)
#plot(km.surv, mark.time = FALSE, xlab = "Days", ylab="Proportion Surviving")
ggsurv(km.surv)

#Kaplain-Meier estimator by gender
km.surv.gender = survfit(response ~ Sex,type="kaplan-meier",data=mini)
summary(km.surv.gender)
#plot(km.surv.gender, mark.time = FALSE,  xlab = "Days Active", ylab="Percent Surviving")
ggsurv(km.surv.gender)



#Cox Regression using coxph function for coefficients and hazard rates
cox.model = coxph(formula = response ~ NumOfVisits + VisitInterval,data = mini)
summary(cox.model)

#Nelson-aalen estimater of cumulative hazard rate
neal.surv = survfit(coxph(response~1), type="aalen")
summary(neal.surv)
ggsurv(neal.surv)


