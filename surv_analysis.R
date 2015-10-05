#read customers data 
customers = read.csv(file.choose(), header=TRUE)
#head(customers)
#create a data frame from the data set
cdf = as.data.frame(customers)


#install.packages("survival")
library(survival)


set.seed(1000)
detach(s1)
#1000 samples randomly taken 
s1= cdf[sample(nrow(cdf),1000, replace=FALSE, prob = NULL),]
attach(s1)


#derive response variable, referred as survival object
response = Surv(time=ceiling(ActivePeriod/30), event = Churn_90)

#Kaplain-Meier estimator with no covariates
km.surv = survfit(response ~ 1,type="kaplan-meier",data=s1)
summary(km.surv)
plot(km.surv, mark.time = FALSE, ylim=c(.7,1), xlab = "Months", ylab="Percent Surviving")
grid()

#Kaplain-Meier estimator by gender
km.surv.gender = survfit(response ~ Sex,type="kaplan-meier",data=s1)
summary(km.surv.gender)
plot(km.surv.gender, mark.time = FALSE, ylim=c(.75,1), xlab = "Days Active", ylab="Percent Surviving")
grid()



#Cox Regression using coxph function for coefficients and hazard rates
cox.response = coxph(formula = response ~ NumOfVisits + VisitInterval,data = s1)
summary(cox.response)
plot(survfit(cox.model), xscale=365.25,xlab = "Days Active", ylab = "Proportion Survived", main="Hazard curve")

#predict hazard ratio
cox.zph(cox.response)
plot(km.surv)
grid()

predRes <- predict(cox.response, type="risk")
head(predRes)


#Paramteric analysis
exp = survreg(response ~ NumOfVisits + VisitInterval, data = s1, dist = "exponential")
summary(exp)

weibull = survreg(response ~ NumOfVisits + VisitInterval, data=s1, dist = "weibull")
summary(weibull)

