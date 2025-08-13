# ==============================================================================
# Script: ETH_Trend_Analysis.R
# Description: Comprehensive ETH price trend analysis using regression and smoothing
# Data Source: Binance API (ETH/USDT hourly)
# ==============================================================================

# ----------------------------
# 1. Setup and Data Fetching
# ----------------------------
library(httr)
library(jsonlite)
library(splines)

# Fetch live ETH price data
get_eth_prices <- function(limit = 500) {
  url <- paste0("https://api.binance.com/api/v3/klines?symbol=ETHUSDT",
                "&interval=1h&limit=", limit)
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  data.frame(
    time = as.POSIXct(data[,1]/1000, origin = "1970-01-01"),
    price = as.numeric(data[,4])  # Closing price
  )
}

eth_data <- get_eth_prices()
TIME <- 1:nrow(eth_data)
PRICE <- eth_data$price

# ----------------------------
# 2. Linear Regression Analysis
# ----------------------------
# Least squares regression
ls_fit <- lm(PRICE ~ TIME)
# Robust regression (L1)
l1_fit <- L1pack::l1fit(TIME, PRICE)

plot(TIME, PRICE, main = "ETH Price Trend: Linear vs Robust Regression",
     xlab = "Time Period", ylab = "Price (USDT)", pch = 19, col = "gray50")
abline(ls_fit, col = "red", lwd = 2)
abline(l1_fit, col = "blue", lwd = 2)
legend("topleft", legend = c("Least Squares", "Robust (L1)"),
       col = c("red", "blue"), lwd = 2)

# ----------------------------
# 3. Polynomial Regression
# ----------------------------
degrees <- c(2, 4, 6, 8)
colors <- c("red", "blue", "green", "purple")

plot(TIME, PRICE, main = "ETH Price: Polynomial Regression",
     xlab = "Time Period", ylab = "Price (USDT)", pch = 19, col = "gray50")

for (i in seq_along(degrees)) {
  poly_fit <- lm(PRICE ~ poly(TIME, degrees[i]))
  lines(TIME, fitted(poly_fit), col = colors[i], lwd = 2)
}

legend("topleft", legend = paste("Degree", degrees),
       col = colors, lwd = 2)

# ----------------------------
# 4. Natural Spline Smoothing
# ----------------------------
dfs <- c(2, 6, 10, 14, 18)
colors <- c("black", "red", "blue", "green", "purple")

plot(TIME, PRICE, main = "ETH Price: Natural Splines",
     xlab = "Time Period", ylab = "Price (USDT)", pch = 19, col = "gray50")

for (i in seq_along(dfs)) {
  ns_fit <- lm(PRICE ~ ns(TIME, df = dfs[i]))
  lines(TIME, fitted(ns_fit), col = colors[i], lwd = 2)
}

legend("topleft", legend = paste("DF =", dfs),
       col = colors, lwd = 2)

# ----------------------------
# 5. Kernel Smoothing
# ----------------------------
bandwidths <- c(1, 5, 20, 50, 125)
colors <- c("red", "blue", "green", "orange", "purple")

plot(TIME, PRICE, main = "ETH Price: Kernel Smoothing",
     xlab = "Time Period", ylab = "Price (USDT)", pch = 19, col = "gray50")

for (i in seq_along(bandwidths)) {
  ks <- ksmooth(TIME, PRICE, "normal", bandwidth = bandwidths[i])
  lines(ks, col = colors[i], lwd = 2)
}

legend("topleft", legend = paste("BW =", bandwidths),
       col = colors, lwd = 2)

# ----------------------------
# 6. Model Comparison
# ----------------------------
cat("=== Model Selection Guide ===\n")
cat("1. For long-term trends: Degree 2 polynomial or DF=6 spline\n")
cat("2. For medium-term patterns: Degree 4 polynomial or DF=10 spline\n")
cat("3. For short-term fluctuations: BW=20 kernel smoother\n")
cat("4. Robust regression (L1) recommended when outliers are present\n")
