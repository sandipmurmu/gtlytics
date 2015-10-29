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

library(xlsx)
##customer Segmentation

#create Recency, Frequency and Monetory segmentation
cdf$RSeg = findInterval(cdf$RecentVisit, quantile(cdf$RecentVisit, c(0.0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE))
cdf$FSeg = findInterval(cdf$NumOfVisits, quantile(cdf$NumOfVisits, c(0.0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE))
cdf$MSeg = findInterval(cdf$AvgPurchase, quantile(cdf$AvgPurchase, c(0.0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE))

cdf = na.omit(cdf)
write.xlsx(cdf, file = 'D:/statistics/churn_rfm.xlsx', sheetName = "RFM", col.names = TRUE)
#create a subset data


RFM = cdf[c(2,17:19)]

#Segmentation using K-Means Cluster
km = kmeans(RFM[2:4], centers = 4)
aggregate(RFM[2:4], by=list(km$cluster), FUN=mean)
kluster = data.frame(RFM, km$cluster)
write.xlsx(kluster, file = 'D:/statistics/BigCluster.xlsx', sheetName = "Cluster", col.names = TRUE)




#install.packages("GGally")
#install.packages("survival")
library(GGally)
library(survival)


detach(mini)
#1000 samples, simple random sample
mini= cdf[sample(nrow(cdf),1000, replace=FALSE, prob = NULL),]
attach(mini)


churn = Churn_365
time = ActivePeriod
#derive response variable, referred as survival object
response = Surv(time, churn)

#Kaplain-Meier estimator with no covariates
km.surv = survfit(response ~ 1,type="kaplan-meier",data=mini)
summary(km.surv) 
ggsurv(km.surv, ylab = "Survival Probablity", main = "Survival Curve, Kaplan-Meier estimation", cens.shape = 3, plot.cens = T)

#Kaplain-Meier estimator by gender
km.surv.gender = survfit(response ~ Sex,type="kaplan-meier",data=mini)
summary(km.surv.gender)
ggsurv(km.surv.gender)



#Cox Regression using coxph function for coefficients and hazard rates
cox.model = coxph(formula = response ~ NumOfVisits + VisitInterval  ,data = mini)
summary(cox.model)

#plot the Hazard curve
ggsurv(survfit(cox.model), ylab = "Survival Probablity", main = "Survival Curve with cox regression", cens.shape = 3, plot.cens = T)

#Hazard ratio
cox.zph(cox.model)

pred = predict(cox.model, type="risk")
head(pred)
summary(pred)

#Nelson-aalen estimater of cumulative hazard rate
neal.surv = survfit(coxph(response~1), type="aalen")
summary(neal.surv)
ggsurv(neal.surv,ylab = "Survival Probablity", main = "Survival Curve, Nelson-Aalen Estimator", cens.shape = 3, plot.cens = T)

#Parametric analysis - Exponential Distribution
exp.surv = survreg(response ~ NumOfVisits + VisitInterval, data = mini, dist = "exponential")
summary(exp.surv)

#Parametric analysis - Weibull Distribution
weibull.surv = survreg(response ~ NumOfVisits + VisitInterval, data=mini, dist = "weibull")
summary(weibull.surv)

#churn period (90 days) analysis
cd = cdf[which(cdf$ActivePeriod < 90),]

#create Recency, Frequency and Monetory segmentation
cd$RSeg = findInterval(cd$RecentVisit, quantile(cd$RecentVisit, c(0.0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE))
cd$FSeg = findInterval(cd$NumOfVisits, quantile(cd$NumOfVisits, c(0.0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE))
cd$MSeg = findInterval(cd$AvgPurchase, quantile(cd$AvgPurchase, c(0.0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE))

cd = na.omit(cd)

#create a subset data
cd.rfm = cd[c(2,17:19)]

#Segmentation using K-Means Cluster
cd.km = kmeans(cd.rfm[2:4] , centers = 4)
aggregate(cd.rfm[2:4] , by=list(cd.km$cluster), FUN=mean)
cd.kluster = data.frame(cd.rfm , cd.km$cluster)

write.xlsx(cd.kluster, file = 'D:/statistics/kluster_90.xlsx', sheetName = "ChurnCluster", col.names = TRUE)



