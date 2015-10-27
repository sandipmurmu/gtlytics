#read censored data 
ds = read.csv(file.choose(), header=TRUE)
cdf = as.data.frame(ds)

#create a R, F variables 
cdf$RSeg = findInterval(cdf$RecentVisit, quantile(cdf$RecentVisit, c(0.0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE))
cdf$FSeg = findInterval(cdf$NumOfVisits, quantile(cdf$NumOfVisits, c(0.0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE))
cdf$MSeg = findInterval(cdf$AvgPurchase, quantile(cdf$AvgPurchase, c(0.0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE))
#cdf$F = cut(cdf$NumOfVisits, 5, labels = F)
#cdf$R = cut(cdf$RecentVisit, 5, labels =F)
cdf$rfm = paste(cdf$RSeg,cdf$FSeg,cdf$MSeg,sep = "")
library(xlsx)
write.xlsx(cdf, file = 'D:/statistics/churn_rf.xlsx', sheetName = "Churn", col.names = TRUE)




cdf.rfm = cdf[c(1,17:19)]
row.has.na <- apply(cdf.rfm, 1, function(x){any(is.na(x))})
sum(row.has.na)
final.rfm = cdf.rfm[!row.has.na,]
km = kmeans(final.rfm, centers = 5)
aggregate(final.rfm, by = list(km$cluster), FUN = mean)
final.rfm = data.frame(final.rfm, km$cluster)

#subset
#library(sqldf)
#cdf.sub = sqldf("select ClientID, Sex, First_Purchase, Last_Purchase, NumOfVisits, VisitInterval, TotalPurchase, AvgPurchase, RecentVisit from cdf where Churn_90=1")
km = kmeans(cdf.sub, centers = 3)


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

