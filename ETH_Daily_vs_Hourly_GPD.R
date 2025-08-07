# ETH Tail Risk Analysis (Problem 2.6 Equivalent)
# Daily vs Hourly Returns using Binance API and GPD fitting

# ------------------------------
# 1. Load Required Libraries
# ------------------------------
install.packages("binancer")
install.packages("fExtremes")

library(binancer)
library(fExtremes)

# ------------------------------
# 2. Fetch ETH Price Data
# ------------------------------

# Get 1000 days of daily ETH prices
ETH_Daily <- binance_klines("ETHUSDT", interval = "1d", limit = 1000)
ETH_Daily_Close <- na.omit(as.numeric(ETH_Daily$close))

# Get 1500 hourly ETH prices (~2 months)
ETH_Hourly <- binance_klines("ETHUSDT", interval = "1h", limit = 1500)
ETH_Hourly_Close <- na.omit(as.numeric(ETH_Hourly$close))

# ------------------------------
# 3. Compute Log Returns
# ------------------------------

ETH_Daily_LogRet <- diff(log(ETH_Daily_Close))
ETH_Hourly_LogRet <- diff(log(ETH_Hourly_Close))

# ------------------------------
# 4. Q-Q Plot of Log Returns
# ------------------------------

qqplot(ETH_Hourly_LogRet, ETH_Daily_LogRet,
       xlab = "ETH Daily Log Returns",
       ylab = "ETH Hourly Log Returns",
       main = "Q-Q Plot: ETH Daily vs Hourly Log Returns")
abline(0, 1, col = "red")

# ------------------------------
# 5. Compare Means and Variances
# ------------------------------

cat("=== Summary Statistics ===\n")
cat("Mean (Daily):", mean(ETH_Daily_LogRet), "\n")
cat("Variance (Daily):", var(ETH_Daily_LogRet), "\n\n")

cat("Mean (Hourly):", mean(ETH_Hourly_LogRet), "\n")
cat("Variance (Hourly):", var(ETH_Hourly_LogRet), "\n")

# ------------------------------
# 6. Fit Generalized Pareto Distribution (GPD)
# ------------------------------

# Use 95th percentile as threshold (u)
threshold_daily <- quantile(ETH_Daily_LogRet, 0.95)
threshold_hourly <- quantile(ETH_Hourly_LogRet, 0.95)

ETH_Daily_GPD <- gpdFit(ETH_Daily_LogRet, u = threshold_daily, type = "mle")
ETH_Hourly_GPD <- gpdFit(ETH_Hourly_LogRet, u = threshold_hourly, type = "mle")

# ------------------------------
# 7. Extract and Compare Shape Parameters
# ------------------------------

cat("\n=== GPD Shape Parameters ===\n")
cat("Shape (ETH Daily):", ETH_Daily_GPD@fit$par.ests["xi"], "\n")
cat("Shape (ETH Hourly):", ETH_Hourly_GPD@fit$par.ests["xi"], "\n")

# ------------------------------
# 8. Visualize GPD Fit
# ------------------------------

par(mfrow = c(2, 1))
plot(ETH_Daily_GPD, main = "GPD Fit: ETH Daily Returns")
plot(ETH_Hourly_GPD, main = "GPD Fit: ETH Hourly Returns")
par(mfrow = c(1, 1))
