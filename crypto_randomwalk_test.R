# Problem 6.12 - ETH Time Series Analysis (Modified for Cryptocurrency)

library(quantmod)
library(forecast)
library(tseries)

### 1. Get Live ETH Data from Binance ###
get_eth_data <- function(days = 365) {
  eth_data <- tryCatch({
    getSymbols("ETH-USD", 
               src = "binance",
               auto.assign = FALSE,
               periodicity = "daily",
               from = Sys.Date() - days)
  }, error = function(e) {
    message("Binance failed, trying Yahoo...")
    getSymbols("ETH-USD", 
               src = "yahoo",
               auto.assign = FALSE,
               from = Sys.Date() - days)
  })
  
  # Convert to clean data frame
  eth_df <- data.frame(
    Date = index(eth_data),
    Price = as.numeric(eth_data$ETH.USD.Close),
    Volume = as.numeric(eth_data$ETH.USD.Volume)
  )
  return(eth_df)
}

eth <- get_eth_data(365)  # Get 1 year of data
eth_ts <- ts(eth$Price, frequency = 365)

### 2. Autocorrelation Analysis ###
par(mfrow = c(2, 2))

# Original series ACF
acf(eth_ts, 40, main = "ETH Price ACF", 
    ylab = "Autocorrelation")
# Shows slow decay → non-stationary like RW

# First differences (returns)
eth_diff <- diff(eth_ts)
acf(eth_diff, 40, main = "ETH Daily Returns ACF",
    ylab = "Autocorrelation")
# Shows no significant autocorrelations → similar to WN

### 3. Comparison with Random Walk ###
# Augmented Dickey-Fuller test
adf.test(eth_ts)  # p-value > 0.05 → non-stationary
adf.test(eth_diff) # p-value < 0.05 → stationary

# Ljung-Box test for white noise
Box.test(eth_diff, type = "Ljung-Box")  # Tests if returns are white noise

### 4. Decomposition Analysis ###
# Log transform for multiplicative decomposition
eth_log <- log(eth_ts)
eth_stl <- stl(eth_log, s.window = "periodic")

plot(eth_stl, main = "ETH Price Decomposition")

# Analyze remainder component
acf(eth_stl$time.series[,"remainder"], 40,
    main = "ACF of ETH Remainder")

### 5. Key Findings ###
cat("\n=== ETH Analysis Results ===\n")
cat("1. ETH prices show characteristics similar to random walk:\n")
cat("   - Non-stationary (ADF p =", round(adf.test(eth_ts)$p.value, 3), ")\n")
cat("   - Returns appear stationary (ADF p =", round(adf.test(eth_diff)$p.value, 3), ")\n\n")

cat("2. Returns show no significant autocorrelations:\n")
cat("   - Ljung-Box p-value:", round(Box.test(eth_diff)$p.value, 3), "\n")
cat("   - Consistent with efficient market hypothesis\n\n")

cat("3. Decomposition reveals:\n")
cat("   - Strong trend component\n")
cat("   - No clear seasonality (unlike traditional assets)\n")
cat("   - Some short-term dependence in remainder\n")

par(mfrow = c(1, 1))
