#read censored data 
ds = read.csv(file.choose(), header=TRUE)
cdf = as.data.frame(ds)

#plot the distribution of the data set
days = cdf$ActivePeriod
d = density(days)
plot(d)
#Normal Q-Q plot
z= (days-mean(days))/sd(days) 
qqnorm(z)
abline(0,1)


#install.packages("GGally")
#install.packages("survival")
library(GGally)
library(survival)


detach(mini)
#1000 samples, simple random sample
mini= cdf[sample(nrow(cdf),1000, replace=FALSE, prob = NULL),]
attach(mini)


churn = Churn_90
time = ActivePeriod
#derive response variable, referred as survival object
response = Surv(time, churn)

#Kaplain-Meier estimator with no covariates
km.surv = survfit(response ~ 1,type="kaplan-meier",data=mini)
summary(km.surv)
ggsurv(km.surv)

#Kaplain-Meier estimator by gender
km.surv.gender = survfit(response ~ Sex,type="kaplan-meier",data=mini)
summary(km.surv.gender)
ggsurv(km.surv.gender)



#Cox Regression using coxph function for coefficients and hazard rates
cox.model = coxph(formula = response ~ NumOfVisits + VisitInterval,data = mini)
summary(cox.model)



#Nelson-aalen estimater of cumulative hazard rate
neal.surv = survfit(coxph(response~1), type="aalen")
summary(neal.surv)
ggsurv(neal.surv)

