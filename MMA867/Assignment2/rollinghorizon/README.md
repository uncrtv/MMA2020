# rollinghorizon
Functions in R that allow you to perform rolling horizon holdout testing.

## Dependencies
* forecast
* lubridate (for now)

## Usage
Load dependent packages
```r
library(forecast)
library(lubridate)
```
```r
rh.tbats(data, partitions, period)
rh.arima(data, partitions, period)
```