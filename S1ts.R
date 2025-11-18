setwd('/Users/zal/Dropbox/My PC (DESKTOP-PGE24LL)/Desktop/grad school shit/Thesis Data/Laguna Lake/data/R_Scripts_DA/TS_data/')

# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggfortify)
library(forecast)

# Load your data
data <- read.csv("S16_NDimp.csv", header = TRUE)
data

# Convert date column to a date format
data$Date <- ymd(data$Date)

# Create a time series object
ts_data <- ts(data[, 2:ncol(data)], start = c(year(data$Date[1]), month(data$Date[1])), frequency = 12)
plot(ts_data[,-1])



# Interpolate missing values
ts_data[, "NO3"] <- na.interp(ts_data[, "NO3"])

# Univariate plots
autoplot(ts_data[, "pH"]) + ggtitle("pH") + ylab("pH") + xlab("Date")
autoplot(ts_data[, "Dox"]) + ggtitle("Dissolved O2") + ylab("Dox") + xlab("Date")
autoplot(ts_data[, "Temp"]) + ggtitle("Temperature") + ylab("Temperature") + xlab("Date")
autoplot(ts_data[, "TSS"]) + ggtitle("TSS") + ylab("TSS") + xlab("Date")
autoplot(ts_data[, "BOD"]) + ggtitle("BOD") + ylab("BOD") + xlab("Date")
autoplot(ts_data[, "PO4"]) + ggtitle("PO4") + ylab("PO4") + xlab("Date")
autoplot(ts_data[, "NH3"]) + ggtitle("NH3") + ylab("NH3") + xlab("Date")
autoplot(ts_data[, "FC"]) + ggtitle("FC") + ylab("FC") + xlab("Date")
autoplot(ts_data[, "Chloride"]) + ggtitle("Chloride") + ylab("Chloride") + xlab("Date")
autoplot(ts_data[, "NO3"]) + ggtitle("NO3") + ylab("NO3") + xlab("Date")
autoplot(ts_data[, "TC"]) + ggtitle("TC") + ylab("TC") + xlab("Date")

# Multivariate plot
ggseasonplot(ts_data, year.labels = TRUE, year.labels.left = TRUE, season.labels = TRUE) +
  xlab("Date") + ylab("Water Quality Parameters") +
  ggtitle("Seasonality of Water Quality Parameters")

# Time series analysis
fit <- ets(ts_data[, "pH"])
summary(fit)
forecast(fit)

fit <- ets(ts_data[, "Dox"])
summary(fit)
forecast(fit)


fit <- ets(ts_data[, "Temp"])
summary(fit)
forecast(fit)

fit <- ets(ts_data[, "BOD"])
summary(fit)
forecast(fit)


fit <- ets(ts_data[, "TSS"])
summary(fit)
forecast(fit)

fit <- ets(ts_data[, "PO4"])
summary(fit)
forecast(fit)


fit <- ets(ts_data[, "NH3"])
summary(fit)
forecast(fit)

fit <- ets(ts_data[, "FC"])
summary(fit)
forecast(fit)


fit <- ets(ts_data[, "TC"])
summary(fit)
forecast(fit)


fit <- ets(ts_data[, "Chloride"])
summary(fit)
forecast(fit)

fit <- ets(ts_data[, "NO3"])
summary(fit)
forecast(fit)


# Seasonal decomposition
decomp <- stl(ts_data[, "pH"], s.window = 12)
autoplot(decomp)

# Seasonal decomposition
decomp <- stl(ts_data[, "Dox"], s.window = 12)
autoplot(decomp)

# Seasonal decomposition
decomp <- stl(ts_data[, "Temp"], s.window = 12)
autoplot(decomp)

# Seasonal decomposition
decomp <- stl(ts_data[, "TSS"], s.window = 12)
autoplot(decomp)

# Seasonal decomposition
decomp <- stl(ts_data[, "BOD"], s.window = 12)
autoplot(decomp)

# Seasonal decomposition
decomp <- stl(ts_data[, "PO4"], s.window = 12)
autoplot(decomp)

# Seasonal decomposition
decomp <- stl(ts_data[, "NH3"], s.window = 12)
autoplot(decomp)

# Seasonal decomposition
decomp <- stl(ts_data[, "FC"], s.window = 12)
autoplot(decomp)

# Seasonal decomposition
decomp <- stl(ts_data[, "Chloride"], s.window = 12)
autoplot(decomp)

# Seasonal decomposition
decomp <- stl(ts_data[, "NO3"], s.window = 12)
autoplot(decomp)

# Seasonal decomposition
decomp <- stl(ts_data[, "TC"], s.window = 12)
autoplot(decomp)

checkresiduals(remainder(decomp))

autoplot(ts_data)
decomp <- stl((ts_data, s.window =12))
checkresiduals(remainder(ts_data))
