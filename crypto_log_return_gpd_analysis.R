

# Install binancer if you haven't already
if (!requireNamespace("binancer", quietly = TRUE)) {
  install.packages("binancer")
}
library(binancer)

# For GPD fitting and tail plots, use fExtremes or extRemes packages
if (!requireNamespace("fExtremes", quietly = TRUE)) {
  install.packages("fExtremes")
}
library(fExtremes)

# 1. Fetch Daily Data for BTC and ETH for last 2 years
btc_daily <- binance_klines("BTCUSDT", interval = "1d", start_time = Sys.Date() - 365*2)
eth_daily <- binance_klines("ETHUSDT", interval = "1d", start_time = Sys.Date() - 365*2)

# 2. Fetch 1-minute data for a recent volatile day (choose recent date)
# For example: 2023-08-01 (replace with your chosen date)
date_minute <- "2023-08-01"
start_minute <- as.POSIXct(paste(date_minute, "00:00:00"), tz = "UTC")
end_minute <- as.POSIXct(paste(date_minute, "23:59:59"), tz = "UTC")

btc_minute <- binance_klines("BTCUSDT", interval = "1m", start_time = start_minute, end_time = end_minute)
eth_minute <- binance_klines("ETHUSDT", interval = "1m", start_time = start_minute, end_time = end_minute)

# 3. Compute log returns for daily and minute prices
btc_daily_ret <- diff(log(btc_daily$close))
eth_daily_ret <- diff(log(eth_daily$close))

btc_minute_ret <- diff(log(btc_minute$close))
eth_minute_ret <- diff(log(eth_minute$close))

# 4. Q-Q Plot comparing ETH and BTC daily returns
qqplot(eth_daily_ret, btc_daily_ret,
       xlab = "ETH Daily Log Returns",
       ylab = "BTC Daily Log Returns",
       main = "Q-Q Plot: ETH vs BTC Daily Returns")
abline(0, 1, col = "red")

# 5. Means and variances
cat("ETH Daily Mean:", mean(eth_daily_ret), "Variance:", var(eth_daily_ret), "\n")
cat("BTC Daily Mean:", mean(btc_daily_ret), "Variance:", var(btc_daily_ret), "\n")
cat("ETH Minute Mean:", mean(eth_minute_ret), "Variance:", var(eth_minute_ret), "\n")
cat("BTC Minute Mean:", mean(btc_minute_ret), "Variance:", var(btc_minute_ret), "\n")

# 6. Fit GPD to upper tails (95% quantile) of daily returns
eth_daily_gpd <- gpdFit(eth_daily_ret, u = quantile(eth_daily_ret, 0.95), type = "mle")
btc_daily_gpd <- gpdFit(btc_daily_ret, u = quantile(btc_daily_ret, 0.95), type = "mle")

# 7. Fit GPD to upper tails (95% quantile) of minute returns
eth_minute_gpd <- gpdFit(eth_minute_ret, u = quantile(eth_minute_ret, 0.95), type = "mle")
btc_minute_gpd <- gpdFit(btc_minute_ret, u = quantile(btc_minute_ret, 0.95), type = "mle")

# 8. Extract shape parameters
cat("GPD Shape ETH Daily:", eth_daily_gpd@fit$par.ests["xi"], "\n")
cat("GPD Shape BTC Daily:", btc_daily_gpd@fit$par.ests["xi"], "\n")
cat("GPD Shape ETH Minute:", eth_minute_gpd@fit$par.ests["xi"], "\n")
cat("GPD Shape BTC Minute:", btc_minute_gpd@fit$par.ests["xi"], "\n")

# 9. Plot GPD fits
par(mfrow = c(2, 2))
plot(eth_daily_gpd, main = "ETH Daily GPD Fit")
plot(btc_daily_gpd, main = "BTC Daily GPD Fit")
plot(eth_minute_gpd, main = "ETH Minute GPD Fit")
plot(btc_minute_gpd, main = "BTC Minute GPD Fit")
par(mfrow = c(1, 1))
