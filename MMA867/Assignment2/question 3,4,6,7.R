source('rollinghorizon/RH.R')
# First open the NetCDF file
nc <- nc_open('HadCRUT.4.6.0.0.median.nc')

#Import the variable as an array
temp <- ncvar_get(nc, "temperature_anomaly")
lon <- ncvar_get(nc, varid = "longitude")
lat <- ncvar_get(nc, varid = "latitude")
time <- ncvar_get(nc, varid = "time")


# Calculate the global median/median
# Iterate over the 3rd dimension, giving median/median for each time period
data.median <- apply(temp, MARGIN = 3, median, na.rm = TRUE)


##################################### Question 3 Kingston#################################################

lon_index <- which.min(abs(lon - (-76.5)))
lat_index <- which.min(abs(lat - 44.23))
tempks<-temp[lon_index, lat_index, ]

# Convert to time series
data_ts_ks <- ts(tempks, start = 1850, frequency = 12)



# Create a trigonometric box-cox autoregressive trend seasonality (TBATS) model

tbats_ks <- tbats(data_ts_ks)
tbats_pred_ks <-forecast(tbats_ks, h=1200, level=c(0.8, 0.95))
par(mfrow=c(1,1))
plot(tbats_pred_ks, xlab="Year", ylab="Predicted medium temperature")
library(tseries)
adf.test(data_ts_ks)# p-value < 0.05 indicates the TS is stationary
nsdiffs(data_ts_ks) 



# Tbats with multiple seasonality

data_msts_ks <- msts(tempks, seasonal.periods=c(12,60,84))
ks_tbats <- tbats(data_msts_ks)
plot(ks_tbats) #plot decomposition
ks_tbats_pred <- forecast(ks_tbats, h=1200, level=c(0.9))
summary(ks_tbats_pred$mean)
plot(ks_tbats_pred, xlab="Time", ylab="Temperature")


write.csv(ks_tbats_pred, "final_kingston_84.csv")

#lm+ARIMA on residuals

WFlm_msts_ks <- tslm(data_msts_ks ~ trend + season) # Build a linear model for trend and seasonality
summary(WFlm_msts_ks)
summary(WFlm_msts_ks$residuals)


# Build ARIMA on it's residuals
residarima1_ks <- auto.arima(WFlm_msts_ks$residuals) # Build ARIMA on it's residuals
residualsArimaForecast_ks <- forecast(residarima1_ks, h=1200) #forecast from ARIMA
residualsF_ks <- as.numeric(residualsArimaForecast_ks$mean)
plot(residualsArimaForecast_ks, xlab="Time", ylab="ARIMA on it's residuals")


regressionForecast_ks <- forecast(WFlm_msts_ks,h=1200) #forecast from lm
regressionF_ks <- as.numeric(regressionForecast_ks$mean)

forecastR_ks <- regressionF_ks+residualsF_ks # Total prediction


#compare with linear regression
plot(regressionForecast_ks, xlab="Time", title="ARIMA on it's residuals")
for (i in 1:1200){points((i+2004+1200)/(1200),forecastR_ks[i],col="red",pch=19, cex=0.5)}

#compare with TBATS
plot(ks_tbats_pred, xlab="Time", main="Compare with tbats")
for (i in 1:1200){points((i+2004+84)/(84),forecastR_ks[i],col="red",pch=19, cex=0.5)}

##### Rolling-horizon test
rh.tbats(data_msts_ks, 2, 120)
#periods=c(12,60,84)-Best model: tbats with seasonality 12,60,84
# RMSE for TBATS with 5 partitions of 120 periods:
# Mean: 1.752363
# Standard Deviation: 0.08353355
rh.arima(data_msts_ks, 2, 120)
# RMSE for lm + ARIMA with 5 partitions of 120 periods:
# Mean: 2.631334
# Standard Deviation: 0.1401782
rh(data_msts_ks, 2, 120)

##########################################Question 4###############################################################
# Generate forecast and 80% confidence interval
tbats_pred_ks <-forecast(tbats_ks, h=1200, level=c(0.8))
# Define standard error
prediction_standard_error<-(tbats_pred_ks$upper-tbats_pred_ks$lower)/2.56
#plot the standard error to see the trend
plot(prediction_standard_error, ylab="Standard Error")plot(prediction_standard_error, ylab="Standard Error")



############################################################Question 6#################################################

# Get data pre-2007 data
median_train <- data.median[1:1884]

# Get data from 2007-2016.12
median_test <- data.median[1885:2004]
#There are 120 records in median_test
length(median_test)

# Constant perdiction-use average median temperature of 2006 (most recent year prior to 2007)
median_2006 <- data.median[1873:1884]
median_2006

constant_pred <- rep(median_2006, 13)[1:120]

#TBATS with multiple sesonality

data_msts_2007 <- msts(median_train, seasonal.periods=c(12,60,84))
data_median_tbats_2007 <- tbats(data_msts_2007)
plot(data_median_tbats_2007) #plot decomposition
median_tbats_pred_2007 <- forecast(data_median_tbats_2007, h=120, level=c(0.9))
plot(median_tbats_pred_2007, xlab="Time", ylab="Temperature")

#Prediction using tbats

sp <- predict(data_median_tbats_2007, h=120)
print(accuracy(sp,median_test))
# 0.14198837 -Best model tbats with seasonality 12,60,84
plot(data_median_tbats_2007)

# Prediction using constant data
constant_pred
print(accuracy(constant_pred,median_test))
#0.1659124

#lm+ARIMA on residuals

WFlm_msts_2007 <- tslm(data_msts_2007 ~ trend + season) # Build a linear model for trend and seasonality
summary(WFlm_msts_2007)
summary(WFlm_msts_2007$residuals)


# Build ARIMA on it's residuals
residarima_2007 <- auto.arima(WFlm_msts_2007$residuals) # Build ARIMA on it's residuals
residualsArimaForecast_2007 <- forecast(residarima_2007, h=120) #forecast from ARIMA
residualsF_2007 <- as.numeric(residualsArimaForecast_2007$mean)

regressionForecast_2007 <- forecast(WFlm_msts_2007,h=120) #forecast from lm
regressionF_2007 <- as.numeric(regressionForecast_2007$mean)

forecastR_2007 <- regressionF_2007+residualsF_2007 # Total prediction

#compare with linear regression
plot(regressionForecast_2007, xlab="Time", title="ARIMA on it's residuals")
for (i in 1:120){points((i+1884+1200)/(1200),forecastR_2007[i],col="red",pch=19, cex=0.5)}


#compare with TBATS
plot(median_tbats_pred_2007, xlab="Time", title="Compare with tbats")
for (i in 1:120){points((i+1884+1200)/(1200),forecastR_2007[i],col="red",pch=19, cex=0.5)}


#Prediction using arima+lm

sp <- predict(forecastR_2007, h=120)
print(accuracy(sp,median_test))
# 0.16152526 





###############################################Question 7-use 1892-2007 data########################################

# Get 1892-2007 data
median_train_1892_2007 <- data.median[505:1884]

# Get data from 2007-2016.12
median_test_2007_2017 <- data.median[1885:2004]

#There are 120 records
length(median_test_2007_2017)


#TBATS with multiple sesonality

data_msts_1892_2007 <- msts(median_train_1892_2007, seasonal.periods=c(12,60,84))
data_median_tbats_1892_2007 <- tbats(data_msts_1892_2007)
plot(data_median_tbats_1892_2007) #plot decomposition
median_tbats_pred_1892_2007 <- forecast(data_median_tbats_1892_2007, h=120, level=c(0.9))
plot(median_tbats_pred_1892_2007, xlab="Time", ylab="Temperature")


#Prediction using tbats
sp_1892_2007 <- predict(data_median_tbats_1892_2007, h=120)
print(accuracy(sp_1892_2007,median_test_2007_2017))
# 0.13517766-Best model-tbats with seasonality 12,60,120
plot(data_median_tbats_1892_2007)

#Prediction using constant data
print(accuracy(constant_pred,median_test_2007_2017))
#0.1659124
# This is higher than RMSE of our model since the constant prediction model fails to capture the increase trend in temperature over years. 

#lm+ARIMA on residuals

WFlm_msts_1892_2007 <- tslm(data_msts_1892_2007 ~ trend + season) # Build a linear model for trend and seasonality
summary(WFlm_msts_1892_2007)
summary(WFlm_msts_1892_2007$residuals)


# Build ARIMA on it's residuals
residarima_1892_2007 <- auto.arima(WFlm_msts_1892_2007$residuals) # Build ARIMA on it's residuals
residualsArimaForecast_1892_2007 <- forecast(residarima_1892_2007, h=120) #forecast from ARIMA
residualsF_1892_2007 <- as.numeric(residualsArimaForecast_1892_2007$mean)

regressionForecast_1892_2007 <- forecast(WFlm_msts_1892_2007,h=120) #forecast from lm
regressionF_1892_2007 <- as.numeric(regressionForecast_1892_2007$mean)

forecastR_1892_2007 <- regressionF_1892_2007+residualsF_1892_2007 # Total prediction

#compare with linear regression
plot(regressionForecast_1892_2007, xlab="Time", title="ARIMA on it's residuals")
for (i in 1:120){points((i+1884+1200)/(1200),forecastR_1892_2007[i],col="red",pch=19, cex=0.5)}


#compare with TBATS
plot(median_tbats_pred_1892_2007, xlab="Time", title="Compare with tbats")
for (i in 1:120){points((i+1884+1200)/(1200),forecastR_1892_2007[i],col="red",pch=19, cex=0.5)}


#Prediction using arima+lm

sp_1892_2007 <- predict(forecastR_1892_2007, h=120)
print(accuracy(sp_1892_2007,median_test_2007_2017))
# 0.12020337
