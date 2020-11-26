# One function to rule them all
rh <- function(data, partitions, period) {
	#(msts object, int, int) -> NoneType
	freq <- max(attr(data, 'msts'))
	accuracy.tbats <- 0
	accuracy.arima <- 0
	for (i in 1:partitions) {
		cat("Working... ",(i-1)/partitions*100,"% complete.\n", sep = '')
		nTest <- period*i  
  	nTrain <- length(data)- nTest - 1

  	train <- window(data, start=1, end=1+(nTrain)/freq)
  	test <- window(data, start=1+(nTrain+1)/freq, end=1+(nTrain+period)/freq)

  	s <- tbats(train)
  	spt <- predict(s, h=period)
  	accuracy.tbats <- rbind(accuracy.tbats,accuracy(spt,test)[2,2])

  	trainlm <- tslm(train ~ trend + season)
    trainlmf <- forecast(trainlm,h=period)
    
    residauto <- auto.arima(trainlm$residuals)
    residf <- forecast(residauto,h=period)
    
    y <- as.numeric(trainlmf$mean)
    x <- as.numeric(residf$mean)
    spa <- x+y
    accuracy.arima <- rbind(accuracy.arima,accuracy(spa,test)[1,2])
	}
	accuracy.tbats <- accuracy.tbats[-1]
	accuracy.arima <- accuracy.arima[-1]
	cat("Complete.\n----------------------------------
RMSE for TBATS with ",partitions," partitions of ",period," periods:
Mean: ",mean(accuracy.tbats),"\nStandard Deviation: ",sd(accuracy.tbats),'\n', sep = '')
	cat("----------------------------------
RMSE for lm + ARIMA with ",partitions," partitions of ",period," periods:
Mean: ",mean(accuracy.arima),"\nStandard Deviation: ",sd(accuracy.arima),'\n', sep = '')
}

# Rolling Horizon TBATS Fun -----------------------------------------------------------------
rh.tbats <- function(data, partitions, period) {
  # (msts object, int, int) -> NULL
  # Performs rolling horizon test with the msts object and print out RMSE
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
From ",as.character(date_decimal(1850))," to ",as.character(date_decimal(1850+(nTrain)/freq)),".
Test Set includes ",period," time periods. Observations ", nTrain+1, " to ", nTrain+period,".
From ",as.character(date_decimal(1850+(nTrain+1)/freq))," to ",as.character(date_decimal(1850+(nTrain+period)/freq)),".
\n", sep = "")
    print(accuracy(sp,test))
    
    accuracy.tbats <- rbind(accuracy.tbats,accuracy(sp,test)[2,2])
  }
  accuracy.tbats <- accuracy.tbats[-1]
  cat("----------------------------------
RMSE for TBATS with ",partitions," partitions of ",period," periods:
Mean: ",mean(accuracy.tbats),"\nStandard Deviation: ",sd(accuracy.tbats), sep = '')
}

# RH ARIMA fun ------------------------------------------------------------
rh.arima <- function(data, partitions, period) {
  # (msts object, int, int) -> NULL
  # Performs rolling horizon test with the msts object and print out RMSE
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
From ",as.character(date_decimal(1850))," to ",as.character(date_decimal(1850+(nTrain)/freq)),".
Test Set includes period time periods. Observations ", nTrain+1, " to ", nTrain+period,".
From ",as.character(date_decimal(1850+(nTrain+1)/freq))," to ",as.character(date_decimal(1850+(nTrain+period)/freq)),".
", sep = "")
    
    print(accuracy(sp,test))
    accuracy.arima <- rbind(accuracy.arima,accuracy(sp,test)[1,2])
  }
  accuracy.arima<-accuracy.arima[-1]
  cat("----------------------------------
RMSE for lm + ARIMA with ",partitions," partitions of ",period," periods:
Mean: ",mean(accuracy.arima),"\nStandard Deviation: ",sd(accuracy.arima), sep = '')
}