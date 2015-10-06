#read customers data 
ds = read.csv(file.choose(), header=TRUE)
#head(customers)
#create a data frame from the data set
cdf = as.data.frame(ds)


detach(mini)
#1000 samples randomly taken 
mini= cdf[sample(nrow(cdf),1000, replace=FALSE, prob = NULL),]
attach(mini)

#install.packages("survival")
library(survival)

#derive response variable, referred as survival object
response = Surv(ActivePeriod, Churn_60)

#Kaplain-Meier estimator with no covariates
km.surv = survfit(response ~ 1,type="kaplan-meier",data=mini)
summary(km.surv)
plot(km.surv, mark.time = FALSE, xlab = "Days", ylab="Proportion Surviving")
grid()


#Kaplain-Meier estimator by gender
km.surv.gender = survfit(response ~ Sex,type="kaplan-meier",data=mini)
summary(km.surv.gender)
plot(km.surv.gender, mark.time = FALSE,  xlab = "Days Active", ylab="Percent Surviving")
grid()



#Nelson-aalen
km.surv.alen = survfit(coxph(Surv(cdf$days,cdf$churn)~1), type="aalen")
summary(km.surv.alen)

res = Surv(cdf$days,cdf$churn, type = "aalen")

#Cox Regression using coxph function for coefficients and hazard rates
cox.model = coxph(formula = response ~ NumOfVisits + VisitInterval,data = mini)
cox.model
#survival analysis for cox model
summary(survfit(cox.model))
plot(survfit(cox.model),xlab = "Month", ylab = "Proportion Survived", main="Hazard curve",mark.time = FALSE)

#predict hazard ratio
cox.zph(cox.model)

grid()

#predRes <- predict(cox.response, type="risk")
#head(predRes)


#Paramteric analysis
exp = survreg(response ~ NumOfVisits + VisitInterval, data = s1, dist = "exponential")
summary(exp)

weibull = survreg(response ~ NumOfVisits + VisitInterval, data=s1, dist = "weibull")
summary(weibull)

