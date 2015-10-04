#read customers data 
customers = read.csv(file.choose(), header=TRUE)
#head(customers)
#create a data frame from the data set
cdf = as.data.frame(customers)

###survival analysis for various churn levels

#install.packages("survival")
library(survival)

#1000 samples for test 1, churn 
set.seed(1000)
s1= cdf[sample(nrow(cdf),1000, replace=FALSE, prob = NULL),]
attach(s1)

#derive response variable, referred as survival object
response = Surv(ActivePeriod, event = Churn_450)

#Kaplain-Meier estimator with no covariates
km.surv = survfit(response ~ 1,type="kaplan-meier",data=s1)
summary(km.surv)
plot(km.surv, mark.time = FALSE, ylim=c(.75,1),xlab = "Days Active", ylab="Percent Surviving")

#Kaplain-Meier estimator by gender
km.surv.gender = survfit(response ~ Sex,type="kaplan-meier",data=s1)
summary(km.surv.gender)
plot(km.surv.gender, mark.time = FALSE, ylim=c(.75,1), xlab = "Days Active", ylab="Percent Surviving")


#Cox Regression using coxph function for coefficients and hazard rates
cox.response = coxph(formula = response ~ NumOfVisits + VisitInterval,data = s1)
summary(cox.response)
#plot(survfit(cox.model), xscale=365.25,xlab = "Days Active", ylab = "Proportion Survived", main="Hazard curve")
#predict hazard ratio
predRes <- predict(cox.response, type="risk")
head(predRes)


#Paramteric analysis
exp = survreg(response ~ NumOfVisits + VisitInterval, data = s1, dist = "exponential")
summary(exp)

weibull = survreg(response ~ NumOfVisits + VisitInterval, data=s1, dist = "weibull")
summary(weibull)
