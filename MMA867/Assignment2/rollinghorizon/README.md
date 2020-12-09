# rollinghorizon
A function in R that performs rolling horizon holdout testing.

Just pass in the data, the number of holdout partitions, and the seasonality period.

The function will create a TBATS model and a linear regression + ARIMA on residuals model, test them on each partition and prints the mean and standard deviation RMSE into the console.

## Dependencies
* forecast
* lubridate (for now)

## Usage
Load dependent packages
```r
library(forecast)
library(lubridate)
```

Use the one function
```r
rh(data, partitions, period)
```