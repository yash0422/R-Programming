#24

mlk_prdis = read.csv("C:/Users/Asus/Desktop/Data Science Master Program/Edureka_Studies/R Certification/009 TIME SERIES/Milk_Prod_Edited.csv", header = TRUE)

mlk_prdis
View(mlk_prdis)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

## line plot ##
mlkprdis_ts = ts(mlk_prdis)
mlkprdis_ts
plot.ts(mlkprdis_ts[,2])

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### Data Partition###
mlk_prdis_train = mlk_prdis[1:156,]
mlk_prdis_test = mlk_prdis[157:nrow(mlk_prdis),]
View(mlk_prdis)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### rmse function#####

rmse = function(erroris){
  sqrt(mean(erroris^2))
}

MAPE = function(erroris,milk_prod){
  mean(abs(erroris/milk_prod))*100  
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

##linear trend model##

linearmdlis = lm(milk_prod~Month_series, mlk_prdis_train)
linearmdlis
Pre_validis = predict(linearmdlis, newdata = mlk_prdis_test[,-2])
erroris = mlk_prdis_test$milk_prod - Pre_validis
rmse(erroris)
MAPE(erroris, mlk_prdis_test$milk_prod)
plot(mlk_prdis$Month_series, mlk_prdis$milk_prod, type = "l")
# - - - -
par(new=TRUE)
pre_data = predict(linearmdlis, newdata = mlk_prdis[,-2])
plot(mlk_prdis$Month_series, pre_data, type = "l", col = "green", yaxt = "n", ylab="")
plot(mlk_prdis_train$Month_series, residuals(linearmdlis), type = "l", col = "black")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### linear trend with seasonality####

mlk1 = lm(milk_prod ~ Month_series + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11, mlk_prdis_train)
mlk1

Pre_validis = predict(mlk1, newdata = mlk_prdis_test[,-2])
erroris = mlk_prdis_test$milk_prod - Pre_validis

rmse(erroris)
MAPE(erroris, mlk_prdis_test$milk_prod)
plot(mlk_prdis$Month_series, mlk_prdis$milk_prod, type = "l")

par(new = TRUE)

pre_full_data = predict(mlk1, newdata = mlk_prdis[,-2])
plot(mlk_prdis$Month_series, pre_full_data, type = "l", col = "green", yaxt = "n", ylab ="")
plot(mlk_prdis_train$Month_series, residuals(mlk1), type = "l", col = "black")

### binomial trend with seasonality###

binomial_tr = lm(milk_prod ~ Month_series + Month_.2 + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11, mlk_prdis_train)
binomial_tr

Pre_valid_binomial = predict(binomial_tr, newdata = mlk_prdis_test[,-2])
erroris = mlk_prdis_test$milk_prod - Pre_valid_binomial
rmse(erroris)
MAPE(erroris, mlk_prdis_test$milk_prod)
plot(mlk_prdis$Month_series, mlk_prdis$milk_prod, type = "l" )

par(new = TRUE)

pred_full_data = predict(binomial_tr, newdata = mlk_prdis[,-2])
plot(mlk_prdis$Month_series, pred_full_data, type = "l", col = "black", yaxt = "n", ylab ="")
plot(mlk_prdis_train$Month_series, residuals(binomial_tr), type = "l", col = "black") 

#ARIMA model in R

install.packages("forecast")
library(forecast)

mlkprdis_ts_filter = mlkprdis_ts[1:156,c(2)]
mlkprdis_ts_filter
tsdatais = ts(mlkprdis_ts_filter, start = c(1962,1), frequency = 12)
tsdatais

#y = auto.arima(tsData)
yis = auto.arima(tsdatais)
yis
plot(forecast(yis, h=12))
predis = forecast(yis, h=12)
predis

#pred_1 <- data.frame(pred)
predis_1 = data.frame(predis)
#pred_1
View(predis_1)

#pred_1$original <- dssts[21:24,2]
predis_1$original = mlkprdis_ts[157:168,2]

predis_1$residual = predis_1$Point.Forecast - predis_1$original
rmse(predis_1$residual)
MAPE(erroris, predis_1$original)







