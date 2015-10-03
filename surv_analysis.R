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
km.fit = survfit(response ~ 1,type="kaplan-meier", conf.type="log",data=s1)

plot(km.fit, lty = 1, mark.time = FALSE, ylim=c(.75,1), xlab = "Days Active", ylab="Percent Surviving")


summary(km.fit)
#summary(km.fit, times = c(60,90,120,130))

#install.packages("rms")
library(rms)
survplot(fit = km.fit)

#build a model
model = coxph(response ~ s1$NumOfVisits + s1$VisitInterval,data = s1)
summary(model)
#Plot the survival model
plot(survfit(model), xscale=365.25,xlab = "Days Active", ylab = "Proportion Survived", main="Hazard curve")

fit = survfit(response ~ s1$Sex, data=s1)
plot(fit, lty = 1:2, mark.time = FALSE, ylim=c(.75,1), xlab = "Days Active", ylab="Percent Surviving") 
legend(20, .8, c("M", "F"), lty = 1.2, bty = "n",ncol = 2)

survdiff(resp ~ s1$Sex, data = s1)

            