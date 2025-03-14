# Prep: Load R packages ########################################################
options(repos = c(CRAN = "https://cran.r-project.org"))
if(!require(xts)){
  install.packages("xts")
  library(xts)
}
if(!require(stats)){
  install.packages("stats")
  library(stats)
}
if(!require(tseries)){
  install.packages("tseries")
  library(tseries)
}
if(!require(urca)){
  install.packages("urca")
  library(urca)
}
if(!require(forecast)){
  install.packages("forecast")
  library(forecast)
}
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}

rm(list=ls())

# Change the working directory to the directory where we want to read and save data ############################################
wd <- "C:\\Users\\yuewa\\Downloads\\Term 4 DBA\\DBA Client\\Data we using"
setwd(wd)

# Read the data from the data file ###################################################################
ccdata <- read.csv("Data SO_WH.csv", header = TRUE) #change to file name

# Display the dataframe
ccdata

# Filter the rows by project name
ccdata <- ccdata %>%
  filter(Project == "MAZAK")
ccdata

# Delete columns not used for simplicity 
ccdata <- select(ccdata, 2, 13)
ccdata

# Convert the date column to a date format
ccdata$SumDate <- as.Date(ccdata$SO.Date, format = "%d/%m/%Y")
# Assign 0 to missing days
ccdata <- ccdata %>% complete(SumDate = seq.Date(min(SumDate), max(SumDate), by = "month"), fill = list(OrdersAmount = 0))
# Sum the values by month
ccdata_month <- aggregate(ccdata$OrdersAmount, by = list(year_month = format(ccdata$SumDate, "%Y-%m")), FUN = sum)
# Change column name
colnames(ccdata_month) <- c("Year_Month", "SO")
# Verify that dataframe is correct
SO <- ccdata_month$SO

# Convert to time series
ccdata_ts <- ts(SO, start = c(2021, 11), frequency = 12)
ccdata_ts

# Holt-Winters ##################################################################################################################
# Estimate value of alpha parameter
ccdata_forecast <- HoltWinters(ccdata_ts, gamma=FALSE)
# Display the results
ccdata_forecast

# The HOlt Winters data structure is complicated , We want to extract the first column of the fitted values.
ccdatafitted <- fitted(ccdata_forecast)[,1]
# The fitted values are a time series so we convert it to numeric (and round them to whole number for simplicity)
ccdata_forecast2 <- round(as.numeric(fitted(ccdata_forecast)[,1]))
ccdata_forecast2 
# Plot the original timeseries against forecast
actual_data <- tail(SO, length(ccdata_forecast2))
summary(actual_data)
actual_data
plot(actual_data)
lines(actual_data)

# Now add the corporate forecasts as red circles
points(ccdata_forecast2, col=c("red"))
title("Forecast")

# Calculate the quantitative measure of forecast accuracy.
errors <- ccdata_forecast2-SO
# Take the absolute difference of forecasts versus actuals
abserrors <- abs(errors)
# Compute the total of the absolute errors
totalerror <- sum(abserrors)
# Compute the total actual demand
totalactual <- sum(SO)
# The ratio is the relative absolute error of forecast
if (totalactual>0) {
  relativeabsoluteerror <- totalerror/totalactual
} else {
  relativeabsoluteerror <- 0
}
# Practitioners multiply the relative absolute error this by 100 to get what they call the WAPE (Weighted Average Percent Error)
WAPE <- relativeabsoluteerror*100
WAPE

# Prediction interval range at 80% and 95%
ccdata_forecasts2 <- forecast:::forecast.HoltWinters(ccdata_forecast, h=3)
ccdata_forecasts2
plot(ccdata_forecasts2)

#Change to a Dataframe
ccdata_forecasts2_df <- as.data.frame(ccdata_forecasts2)

#Delete 80% Prediction interval
ccdata_forecasts2_df <- select(ccdata_forecasts2_df, -2, -3)

# write to CSV file
write.csv(ccdata_forecasts2_df, file = "ccdata_forecasts2_df", row.names = FALSE)


# Monthly Overall ##############################################
# Prep: Load R packages ########################################################
options(repos = c(CRAN = "https://cran.r-project.org"))
if(!require(xts)){
  install.packages("xts")
  library(xts)
}
if(!require(stats)){
  install.packages("stats")
  library(stats)
}
if(!require(tseries)){
  install.packages("tseries")
  library(tseries)
}
if(!require(urca)){
  install.packages("urca")
  library(urca)
}
if(!require(forecast)){
  install.packages("forecast")
  library(forecast)
}
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}

rm(list=ls())

# Change the working directory to the directory where we want to read and save data ############################################
wd <- "C:\\Users\\yuewa\\Downloads\\Term 4 DBA\\DBA Client\\Data we using"
setwd(wd)

# Read the data from the data file ###################################################################
ccdata <- read.csv("Data SO_WH.csv", header = TRUE) #change to file name

# Display the dataframe
ccdata

# Delete columns not used for simplicity 
ccdata <- select(ccdata, 2, 13)
ccdata

# Convert the date column to a date format
ccdata$SumDate <- as.Date(ccdata$SO.Date, format = "%d/%m/%Y")
# Sum the values by month
ccdata_month <- aggregate(ccdata$OrdersAmount, by = list(year_month = format(ccdata$SumDate, "%Y-%m")), FUN = sum)
# Change column name
colnames(ccdata_month) <- c("Year_Month", "SO")
# Verify that dataframe is correct
SO <- ccdata_month$SO

# Convert to time series
ccdata_ts <- ts(SO, start = c(2021, 11), frequency = 12)
ccdata_ts

# Holt-Winters ##################################################################################################################
# Estimate value of alpha parameter
ccdata_forecast <- HoltWinters(ccdata_ts, gamma=FALSE)
# Display the results
ccdata_forecast

# The HOlt Winters data structure is complicated , We want to extract the first column of the fitted values.
ccdatafitted <- fitted(ccdata_forecast)[,1]
# The fitted values are a time series so we convert it to numeric (and round them to whole number for simplicity)
ccdata_forecast2 <- round(as.numeric(fitted(ccdata_forecast)[,1]))
ccdata_forecast2 
# Plot the original timeseries against forecast
actual_data <- tail(SO, length(ccdata_forecast2))
summary(actual_data)
actual_data
plot(actual_data)
lines(actual_data)

# Now add the corporate forecasts as red circles
points(ccdata_forecast2, col=c("red"))
title("Forecast")

# Calculate the quantitative measure of forecast accuracy.
errors <- ccdata_forecast2-SO
# Take the absolute difference of forecasts versus actuals
abserrors <- abs(errors)
# Compute the total of the absolute errors
totalerror <- sum(abserrors)
# Compute the total actual demand
totalactual <- sum(SO)
# The ratio is the relative absolute error of forecast
if (totalactual>0) {
  relativeabsoluteerror <- totalerror/totalactual
} else {
  relativeabsoluteerror <- 0
}
# Practitioners multiply the relative absolute error this by 100 to get what they call the WAPE (Weighted Average Percent Error)
WAPE <- relativeabsoluteerror*100
WAPE

# Prediction interval range at 80% and 95%
ccdata_forecasts2 <- forecast:::forecast.HoltWinters(ccdata_forecast, h=3)
ccdata_forecasts2
plot(ccdata_forecasts2)

#Change to a Dataframe
ccdata_forecasts2_df <- as.data.frame(ccdata_forecasts2)

#Delete 80% Prediction interval
ccdata_forecasts2_df <- select(ccdata_forecasts2_df, -2, -3)

# write to CSV file
write.csv(ccdata_forecasts2_df, file = "ccdata_forecasts2_df", row.names = FALSE)

