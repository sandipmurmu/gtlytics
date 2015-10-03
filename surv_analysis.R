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

#derive response variable, referred as survival object
response = Surv(time=s1$First_Purchase, time2 = s1$Last_Purchase, event = s1$Churn_450)

#Kaplain-Meier estimator with no covariates
km.surv = survfit(response ~ 1,type="kaplan-meier",data=s1)
summary(km.surv)
plot(km.surv, lty = 1, mark.time = FALSE, xscale=365.25, ylim=c(.75,1), xlab = "Days Active", ylab="Percent Surviving")

#Kaplain-Meier estimator by gender
km.surv.gender = survfit(response ~ s1$Sex,type="kaplan-meier",data=s1)
summary(km.surv.gender)
plot(km.surv.gender, lty = 1, mark.time = FALSE, xscale = 365.25, ylim=c(.75,1), xlab = "Days Active", ylab="Percent Surviving")


#Cox Regression using coxph function for coefficients and hazard rates
cox.response = coxph(formula = response ~ s1$NumOfVisits + s1$VisitInterval,data = s1)
summary(cox.response)
#plot(survfit(cox.model), xscale=365.25,xlab = "Days Active", ylab = "Proportion Survived", main="Hazard curve")
#predict hazard ratio
predRes <- predict(cox.response, type="risk")
head(predRes)






            