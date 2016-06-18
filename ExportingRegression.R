#Exporting ReggressionOutputs and PerformanceMatrics

lm.fit = lm(kWh~. -Account -year, data = train)
b= lm.fit$coefficients

pred = predict(lm.fit, test)
accuracy(pred,train$kWh)
a=accuracy(pred,train$kWh)

write.csv(b, "RegressionOutputs.csv")
write.csv(a, "PerformanceMatrics.csv")

