df = read.csv("monthly-milk-production-pounds-pp.csv")
View(df)


df.ts<- ts(df$milkprod, start=c(1962,1), end=c(1975,12), frequency=12)
View(df.ts)
jpeg(file="saving_plot1.jpeg",width=600, height=350)
plot(df.ts)
dev.off()

jpeg(file="saving_plot2.jpeg",width=600, height=350)
plot(decompose(df.ts))
dev.off()

df.train_set <- ts(df$milkprod,start=c(1962,1), end=c(1974,12), frequency=12 )
df.test_set <- ts(df$milkprod,start=c(1975,1), end=c(1975,12), frequency=12)
View(df.train_set)
View(df.test_set)


library(forecast)
df.arima = auto.arima(df.ts)
df.prd_arima = forecast(df.arima, h=12)
jpeg(file="saving_plot3.jpeg",width=600, height=350)
plot(forecast(df.arima, h=12), include=1)
dev.off()


df.HW = HoltWinters(df.ts)
df.HW_pred = predict(df.HW, n.ahead=12)
jpeg(file="saving_plot4.jpeg",width=600, height=350)
ts.plot(df.HW_pred)
dev.off()

jpeg(file="saving_plot5.jpeg",width=600, height=350)
par(mfrow=c(3,1))
plot(forecast(df.arima, h=12), include=1)
ts.plot(df.HW_pred)
plot(df.test_set)
dev.off()
