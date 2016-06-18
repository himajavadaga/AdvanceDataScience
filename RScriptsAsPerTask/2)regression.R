#Start Regression
install.packages('forecast')
#lm.fit=lm(kWh~., data=sampleformat)

#singularities for account and year so remove them.
lm.fit=lm(kWh~. -Account -Date -year, data=sampleformat)
summary(lm.fit)

library(MASS)
library(ISLR)
smp_size <- floor(0.80*nrow(sampleformat))
set.seed(123)
train_ind <- sample(seq_len(nrow(sampleformat)),size=smp_size)
train <- sampleformat[train_ind, ]
test <- sampleformat[-train_ind, ]

lm.fit = lm(kWh~. -Account -Date -year, data = train)
summary(lm.fit)

library(forecast)
pred = predict(lm.fit, test)

#Exporting ReggressionOutputs and PerformanceMatrics

a = accuracy(pred,train$kWh)
b= lm.fit$coefficients

write.csv(b, "RegressionOutputs.csv")
write.csv(a, "PerformanceMatrics.csv")
