# Libraries ---------------------------------------------------------------
# install.packages('ncdf4')
# install.packages('forecast')
install.packages('lubridate')
install.packages('xlsx')
library(ncdf4)
library(forecast)
library(ggplot2)
library(tidyr)
library(lubridate)
library(xlsx)
source('rollinghorizon/RH.R')

# Import Data -------------------------------------------------------------
# This will be different for each dataset, ask me if you have any questions
# NetCDF files are very complicated.

# First open the NetCDF file
nc <- nc_open('HadCRUT.4.6.0.0.median.nc')
# # View what variables we have in the dataset
# attributes(nc$var)
# Import the variable as an array
data <- ncvar_get(nc, "temperature_anomaly")

# Kingston data
kingston <- data[21, 27,]
print(kingston)
data[21, 27, 2030]

# # Miscellaneous code
dim(data)
# print(data)
data.201902 <- data[,,2030]
# data.201902[is.na(data.201902)] <- 0
# dim(data.201902)
# dim(data.201902.t)
data.201902.t <- t(data.201902)
data.201902.t <- as.data.frame(data.201902.t)
# colnames(data.201902.t) <- c(1:72)
# str(data.201902.t)
# head(data.201902.t)
data.201902.t <- cbind(c(1:nrow(data.201902.t)), data.201902.t)
colnames(data.201902.t) <- c("Latitude", 1:72)
data.201902.clean <- gather(data.201902.t, Longitude, temp, -Latitude)
data.201902.clean$Longitude <- as.integer(data.201902.clean$Longitude)
# dim(data.201902.clean)
# head(data.201902.clean)
# str(data.201902.clean)
ggplot(data = data.201902.clean, aes(x = Longitude, y = Latitude)) +
  geom_tile(aes(fill = temp)) +scale_fill_gradient(low = "blue", high = "red")

# median(data[,,1], na.rm = TRUE)

# # Another way of calculating the means
#data.mean2 <- colMeans(data, dims = 2, na.rm = TRUE)
# # Check if this is (mostly) the same as the other method - TRUE
#sum((data.mean2 - data.mean) > 0.00000000001) == 0

# # This code is to see the attributes of a variable.
ncatt_get(nc, "temperature_anomaly")
ncatt_get(nc, 0)
print(nc)

# # This is for the text file, we're not going to use it though
# # because we can now calculate the mean/median for the other data
# hadcrut4 <- read.table('HadCRUT.4.6.0.0.monthly_ns_avg.txt')
# # We only need the first two columns, time and median temp
# # Reference: https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/series_format.html
# hadcrut4 <- hadcrut4[, -c(3:12)]
# colnames(hadcrut4) <- c('Time', 'Median')


# Data Manipulation -------------------------------------------------------
# Calculate the global mean/median
# Iterate over the 3rd dimension, giving mean/median for each time period
data.mean <- apply(data, MARGIN = 3, mean, na.rm = TRUE)
data.median <- apply(data, MARGIN = 3, median, na.rm = TRUE)

#data.median <- log(data.median+1.5)

# Export as CSV
write.csv(data.mean, "mean.csv")

# Convert to time series
hadcrut4_ts <- ts(hadcrut4$Mean, start = 1850, frequency = 12)
data_ts <- ts(data.median, start = 1850, frequency = 12)

# Random models -----------------------------------------------------------
fit <- decompose(data_ts, type="multiplicative")
plot(fit)
AAZ <- ets(data_ts, model="AAZ")
AAZ_pred <- forecast(AAZ, h=1200, level=c(0.9))
plot(AAZ_pred, xlab="Month", ylab="Predicted Temp")

fit_tbats <- tbats(data_ts)
plot(fit_tbats)
tbats_pred <-forecast(fit_tbats, h=1200, level=c(0.9))
par(mfrow=c(1,2))
par(mfrow=c(1,1))
plot(tbats_pred, xlab="Year", ylab="Log Temperature")

Pacf(diff(data_ts,12),main="") 
arima <- auto.arima(data_ts,seasonal=TRUE)
arima2 <- auto.arima(data_ts,D=1)
arima
arima2
plot(forecast(arima2,1200))


# TBATS with msts ---------------------------------------------------------
# data_msts <- msts(data.median, seasonal.periods=c(12,60,1200), ts.frequency = 12, start = 1850)
# data_msts <- msts(data.median, seasonal.periods=c(12,60,600), ts.frequency = 12, start = 1850)
# data_msts <- msts(data.median, seasonal.periods=c(12,60,600), start = 1850, end = c(2019, 2))
data_msts <- msts(data.median, seasonal.periods=c(12,60,1200))
data_median_tbats <- tbats(data_msts)
plot(data_median_tbats) #plot decomposition
median_tbats_pred <- forecast(data_median_tbats, h=1200, level=0.9)
plot(median_tbats_pred, xlab="Time", ylab="Temperature Anomaly")

write.csv(median_tbats_pred, "final_tbats_84.csv")

rh(data_msts, 5, 120)

# lm + ARIMA --------------------------------------------------------------
lm_msts <- tslm(data_msts ~ trend + season) # Build a linear model for trend and seasonality
summary(lm_msts)

residarima1 <- auto.arima(lm_msts$residuals) # Build ARIMA on it's residuals
residarima1
residualsArimaForecast <- forecast(residarima1, h=1200) #forecast from ARIMA
residualsF <- as.numeric(residualsArimaForecast$mean)

regressionForecast <- forecast(lm_msts,h=1200,level=0.9) #forecast from lm
regressionF <- as.numeric(regressionForecast$mean)

forecastR <- regressionF+residualsF # Total prediction
# print(forecastR)
# for (i in 1:1200){points(i,forecastR[i],col="red",pch=19, cex=0.5)}

#compare with TBATS
# plot(median_tbats_pred, xlab="Time", ylab="Temp", ylim = c(-1,1))
plot(median_tbats_pred, xlab="Time", ylab="Temperature Anomaly", main = "TBATS vs lm+ARIMA")
# plot(regressionForecast)
# for (i in 1:1200){points((i/12)+2019,forecastR[i],col="red",pch=19, cex=0.5)}
for (i in 1:1200){points((i+2030+1200)/1200,forecastR[i],col="red",pch=19, cex=0.5)}
# for (i in 1:1200){points(i+2019,regressionF[i],col="red",pch=19, cex=0.5)}

# Dates -------------------------------------------------------------------
dates <- c(ymd('1850-01-01'))
print(dates)
tail(dates)
for (i in c(1:2029)) {
  date <- ymd('1850-01-01')
  month(date) <- month(date) + i
  dates <- c(dates, date)  
}
length(dates)

data_withdates <- as.data.frame(cbind(dates, data.mean))
colnames(data_withdates) <- c("Date", "Temp")
# data_withdates$Date2 <- as.character(data_withdates$Date)
write.csv(data_withdates, "data_withdates.csv")


# Rolling Horizon TBATS Fun -----------------------------------------------------------------
rh.tbats <- function(data, partitions, period) {
  # (msts object, int, int) -> array
  # Performs rolling horizon test with the msts object and return a 
  freq <- max(attr(data, 'msts'))
  accuracy.tbats <- 0
  for(i in 1:partitions) {
    nTest <- period*i  
    nTrain <- length(data)- nTest - 1

    train <- window(data, start=1, end=1+(nTrain)/freq)
    test <- window(data, start=1+(nTrain+1)/freq, end=1+(nTrain+period)/freq)
    
    s <- tbats(train)
    sp <- predict(s, h=period)
    
    cat("----------------------------------
Data Partition ",i,"
        
Training Set includes ",nTrain," time periods. Observations 1 to ",nTrain,".
From ",as.character(date_decimal(1850))," to ",as.character(date_decimal(1850+(nTrain)/600)),".
Test Set includes ",period," time periods. Observations ", nTrain+1, " to ", nTrain+period,".
From ",as.character(date_decimal(1850+(nTrain+1)/600))," to ",as.character(date_decimal(1850+(nTrain+120)/600)),".
\n", sep = "")
    print(accuracy(sp,test))
    
    accuracy.tbats <- rbind(accuracy.tbats,accuracy(sp,test)[2,2])
  }
  accuracy.tbats <- accuracy.tbats[-1]
  cat("----------------------------------
RMSE for TBATS with ",partitions," partitions of ",period," periods:
Mean: ",mean(accuracy.tbats),"\nStandard Deviation: ",sd(accuracy.tbats), sep = '')
}

# TBATS raw ---------------------------------------------------------------
accuracy.tbats=0 # we will check average 10-years-out accuracy for 50 years
for (i in 1:5)
{ 
  nTest <- 120*i  
  nTrain <- length(data_msts)- nTest - 1
  # print(i)
  # print(c("nTest", nTest))
  # print(c("nTrain", nTrain))
  # print(1850+(nTrain)/12)
  # print(1850+(nTrain+1)/12)
  # print(1850+(nTrain+120)/12)
  # train <- window(data_msts, start=1850, end=1850+(nTrain)/12)
  # test <- window(data_msts, start=1850+(nTrain+1)/12, end=1850+(nTrain+120)/12)

  train <- window(data_msts, start=1, end=1+(nTrain)/1200)
  test <- window(data_msts, start=1+(nTrain+1)/1200, end=1+(nTrain+120)/1200)
  
  s <- tbats(train)
  sp<- predict(s,h=120)
  
  cat("----------------------------------
    
    Data Partition ",i,"
    
    Training Set includes ",nTrain," time periods. Observations 1 to ",nTrain,".
    From ",as.character(date_decimal(1850))," to ",as.character(date_decimal(1850+(nTrain)/600)),".
    Test Set includes 120 time periods. Observations ", nTrain+1, " to ", nTrain+120,".
    From ",as.character(date_decimal(1850+(nTrain+1)/600))," to ",as.character(date_decimal(1850+(nTrain+120)/600)),".
    ", sep = "")
  print(accuracy(sp,test))
  
  accuracy.tbats<-rbind(accuracy.tbats,accuracy(sp,test)[2,2])
  
  #print(sp$model)
}
accuracy.tbats <- accuracy.tbats[-1]

##### Rolling-horizon holdout: ARIMA on residuals ---------------------
accuracy.arima=0 
for (i in 1:5)
{ 
  nTest <- 120*i  
  nTrain <- length(data_msts)- nTest -1
  train <- window(data_msts, start=1850, end=1850+(nTrain)/12)
  test <- window(data_msts, start=1850+(nTrain+1)/12, end=1850+(nTrain+120)/12)
  
  trainlm <- tslm(train ~ trend + season)
  trainlmf <- forecast(trainlm,h=120)
  
  residauto <- auto.arima(trainlm$residuals)
  residf <- forecast(residauto,h=120)
  
  y <- as.numeric(trainlmf$mean)
  x <- as.numeric(residf$mean)
  sp <- x+y
  
  cat("----------------------------------
    
    Data Partition ",i,"
    
    Training Set includes ",nTrain," time periods. Observations 1 to ",nTrain,".
    From ",as.character(date_decimal(1850))," to ",as.character(date_decimal(1850+(nTrain)/12)),".
    Test Set includes 120 time periods. Observations ", nTrain+1, " to ", nTrain+120,".
    From ",as.character(date_decimal(1850+(nTrain+1)/12))," to ",as.character(date_decimal(1850+(nTrain+120)/12)),".
    ", sep = "")
  
  print(accuracy(sp,test))
  #  print(residauto)
  
  accuracy.arima<-rbind(accuracy.arima,accuracy(sp,test)[1,2])
  
  #print(sp$model)
}
accuracy.arima<-accuracy.arima[-1]



# RH ARIMA fun ------------------------------------------------------------
rh.arima <- function(data, partitions, period) {
  # (msts object, int, int) -> array
  # Performs rolling horizon test with the msts object and return a 
  freq <- max(attr(data, 'msts'))
  accuracy.arima <- 0 
  for (i in 1:partitions)
  { 
    nTest <- period*i  
    nTrain <- length(data) - nTest - 1

    train <- window(data, start=1, end=1+(nTrain)/freq)
    test <- window(data, start=1+(nTrain+1)/freq, end=1+(nTrain+period)/freq)
    
    trainlm <- tslm(train ~ trend + season)
    trainlmf <- forecast(trainlm,h=period)
    
    residauto <- auto.arima(trainlm$residuals)
    residf <- forecast(residauto,h=period)
    
    y <- as.numeric(trainlmf$mean)
    x <- as.numeric(residf$mean)
    sp <- x+y
    
    cat("----------------------------------
Data Partition ",i,"
        
Training Set includes ",nTrain," time periods. Observations 1 to ",nTrain,".
From ",as.character(date_decimal(1850))," to ",as.character(date_decimal(1850+(nTrain)/12)),".
Test Set includes period time periods. Observations ", nTrain+1, " to ", nTrain+period,".
From ",as.character(date_decimal(1850+(nTrain+1)/12))," to ",as.character(date_decimal(1850+(nTrain+period)/12)),".
", sep = "")
    
    print(accuracy(sp,test))
    accuracy.arima <- rbind(accuracy.arima,accuracy(sp,test)[1,2])
  }
  accuracy.arima<-accuracy.arima[-1]
  cat("----------------------------------
RMSE for lm + ARIMA with ",partitions," partitions of ",period," periods:
Mean: ",mean(accuracy.arima),"\nStandard Deviation: ",sd(accuracy.arima), sep = '')
}

# RH ARIMA ----------------------------------------------------------------
accuracy.arima=0 
for (i in 1:5)
{ 
  nTest <- 120*i  
  nTrain <- length(data_msts)- nTest -1
  # print(i)
  # print(c("nTest", nTest))
  # print(c("nTrain", nTrain))
  # print(1+(nTrain)/600)
  # print(1+(nTrain+1)/600)
  # print(1+(nTrain+120)/600)
  train <- window(data_msts, start=1, end=1+(nTrain)/1200)
  test <- window(data_msts, start=1+(nTrain+1)/1200, end=1+(nTrain+120)/1200)
  
  trainlm <- tslm(train ~ trend + season)
  trainlmf <- forecast(trainlm,h=120)
  
  residauto <- auto.arima(trainlm$residuals)
  residf <- forecast(residauto,h=120)
  
  y <- as.numeric(trainlmf$mean)
  x <- as.numeric(residf$mean)
  sp <- x+y
  
  cat("----------------------------------
    
    Data Partition ",i,"
    
    Training Set includes ",nTrain," time periods. Observations 1 to ",nTrain,".
    From ",as.character(date_decimal(1850))," to ",as.character(date_decimal(1850+(nTrain)/12)),".
    Test Set includes 120 time periods. Observations ", nTrain+1, " to ", nTrain+120,".
    From ",as.character(date_decimal(1850+(nTrain+1)/12))," to ",as.character(date_decimal(1850+(nTrain+120)/12)),".
    ", sep = "")
  
  print(accuracy(sp,test))
  #  print(residauto)
  
  accuracy.arima<-rbind(accuracy.arima,accuracy(sp,test)[1,2])
  
  #print(sp$model)
}
accuracy.arima<-accuracy.arima[-1]

# Fancy results 1
results <- data.frame((mean(accuracy.arima)),sd(accuracy.arima))
results <- rbind(results, c(mean(accuracy.tbats), sd(accuracy.tbats)))
colnames(results) <- c("Mean", "StDev")
rownames(results) <- c("ARIMA", "TBATS")
print(results)

# Fancy results 2
cat("Rolling Horizon RMSE:\nTBATS:",mean(accuracy.tbats), sd(accuracy.tbats),"\nARIMA:", mean(accuracy.arima), sd(accuracy.arima))

