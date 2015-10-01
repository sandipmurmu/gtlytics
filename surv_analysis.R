#read customers data 
customers = read.csv(file.choose(), header=TRUE)
head(customers)
#create a data frame from the data set
cdf = as.data.frame(customers)

#survival analysis for various churn levels
#install.packages("survival")
library(survival)

#1000 samples for test 1, churn 
set.seed(1000)
s1= cdf[sample(nrow(cdf),1000, replace=FALSE, prob = NULL),]

#derive response variable
resp = Surv(as.numeric(as.Date(s1$First_Purchase, origin='1900-01-01')), 
            as.numeric(as.Date(s1$Last_Purchase,  origin='1900-01-01')),
            s1$Churn_30)
#Fit a survival curve
fit = survfit(resp ~ 1, data=s1)
#plot the survival curve
surgraph = plot(fit, lty = 1, mark.time = FALSE, ylim=c(.75,1), xlab = "Days since subscribed", ylab="Percent Surviving")

            