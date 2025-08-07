# Install the package if not already installed
# install.packages("remotes")
# remotes::install_github("daroczig/binancer")

library(binancer)
library(xts)

# Load Binance data for BTC/USDT and ETH/USDT
binance_coins_pairs()

# Set timezone
Sys.setenv(TZ = "UTC")

# Download historical prices (1d candles)
btc_data <- binance_klines("BTCUSDT", interval = "1d", limit = 1000)
eth_data <- binance_klines("ETHUSDT", interval = "1d", limit = 1000)

# Convert to xts and compute log returns
btc_xts <- xts(btc_data$close, order.by = btc_data$close_time)
eth_xts <- xts(eth_data$close, order.by = eth_data$close_time)

btc_ret <- diff(log(btc_xts))
eth_ret <- diff(log(eth_xts))

# Merge and clean
data <- na.omit(merge(btc_ret, eth_ret))
colnames(data) <- c("X", "Y")  # X = BTC log returns, Y = ETH log returns
