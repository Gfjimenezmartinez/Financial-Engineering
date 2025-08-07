# ===========================================
# Problem 2.5 - GPD Tail Risk Analysis for BTC
# Using Yahoo Finance Price Data
# ===========================================

# Load required libraries
library(quantmod)   # For downloading financial data
library(Rsafd)      # For EVT and GPD analysis
library(evir)       # For additional EVT utilities

# -------------------------------------------
# Step 1: Get BTC Daily Closing Prices from Yahoo
# -------------------------------------------

# Download BTC data from 2021 to 2023
getSymbols("BTC-USD", from = "2021-01-01", to = "2023-12-31", src = "yahoo")

# Extract closing prices
BTC_price <- Cl(`BTC-USD`)

# Convert to numeric vector
BTC_price <- as.numeric(BTC_price)

# -------------------------------------------
# Step 2: Plot the Time Series of Prices
# -------------------------------------------

plot(BTC_price, type = "l",
     main = "Daily BTC Closing Price (2021–2023)",
     xlab = "Day Index", ylab = "Price (USD)")

# -------------------------------------------
# Step 3: Explore the Distribution
# -------------------------------------------

# Set plot layout
par(mfrow = c(2, 1))

# Histogram with density overlay
hist(BTC_price, freq = FALSE, breaks = 50,
     main = "Histogram of BTC Prices", xlab = "Price (USD)", col = "lightblue")
lines(density(BTC_price), col = "darkblue", lwd = 2)

# Q-Q plot vs Log-Normal
set.seed(1)
x_ln <- rlnorm(1000, meanlog = mean(log(BTC_price)), sdlog = sd(log(BTC_price)))
qqplot(x_ln, BTC_price,
       xlab = "Log-Normal Quantiles",
       ylab = "BTC Price Quantiles")
abline(0, 1, col = "red")
title("Q-Q Plot: Log-Normal vs BTC Prices")

# -------------------------------------------
# Step 4: Test for Heavy Tails
# -------------------------------------------

par(mfrow = c(1, 2))

# Q-Q plot vs Exponential
qqexp(BTC_price, main = "Q-Q Plot: BTC vs Exponential")

# Shape plot (Hill estimator)
shape.plot(BTC_price)

# -------------------------------------------
# Step 5: Fit GPD to the Upper Tail (Threshold = 95th percentile)
# -------------------------------------------

threshold_95 <- quantile(BTC_price, 0.95)

BTC_gpd_95 <- fit.gpd(BTC_price, tail = "upper", upper = threshold_95)

# Tail plot for visual diagnostics
tailplot(BTC_gpd_95, tail = "upper")

# Print the estimated shape parameter ξ
cat("Estimated shape parameter (ξ) at 95th percentile: ",
    BTC_gpd_95@upper.par.ests[2], "\n")

# -------------------------------------------
# Step 6: Refit with Higher Threshold (98th percentile)
# -------------------------------------------

threshold_98 <- quantile(BTC_price, 0.98)

BTC_gpd_98 <- fit.gpd(BTC_price, tail = "upper", upper = threshold_98)

# Plot the updated tail fit
tailplot(BTC_gpd_98, tail = "upper")

# Print the new estimate of the shape parameter
cat("Estimated shape parameter (ξ) at 98th percentile: ",
    BTC_gpd_98@upper.par.ests[2], "\n")

# Reset plotting layout
par(mfrow = c(1, 1))
