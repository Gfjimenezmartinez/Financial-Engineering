# ------------------------------------------
# 0. Load required packages
library(binancer)   # For Binance data
library(Rsafd)      # Tail analysis
library(zoo)        # Rolling calculations
library(ggplot2)    # Plotting
library(dplyr)      # Data manipulation
library(reshape2)   # Data reshaping

# ------------------------------------------
# 1. Fetch ETH/USDT data from Binance (spot market)

# Download historical daily klines for ETHUSDT, limit = 1000 days max (adjust as needed)
eth_binance <- binance_klines("ETHUSDT", interval = "1d", limit = 1000)

# Data structure: open, high, low, close, volume, close_time, etc.
# Convert to data.frame and arrange by time ascending
eth_binance <- eth_binance %>% 
  arrange(open_time) %>% 
  mutate(close_price = as.numeric(close))

# Use closing prices and convert timestamps to Date
eth_binance$Date <- as.Date(as.POSIXct(eth_binance$open_time / 1000, origin="1970-01-01"))

# ------------------------------------------
# 2. Calculate returns and rolling volatility

eth_binance <- eth_binance %>%
  mutate(log_return = c(NA, diff(log(close_price))))

# Rolling 20-day volatility of returns
eth_binance <- eth_binance %>%
  mutate(roll_vol = zoo::rollapply(log_return, width = 20, FUN = sd, fill = NA, align = "right"))

# Volatility shock = deviation from mean rolling vol
mean_vol <- mean(eth_binance$roll_vol, na.rm = TRUE)
eth_binance <- eth_binance %>%
  mutate(vol_shock = roll_vol - mean_vol)

# Remove NA rows from volatility and returns calculation
eth_clean <- eth_binance %>%
  filter(!is.na(vol_shock) & !is.na(close_price)) %>%
  select(Date, vol_shock, close_price)

# Rename for clarity
colnames(eth_clean) <- c("Date", "Vol_Shock", "Price")

# ------------------------------------------
# 3. Proceed with tail analysis, correlation, and plots (same as before)

# (Repeat your tail analysis, correlation calculations, and plotting here 
#  using eth_clean$Vol_Shock and eth_clean$Price)

# Example tail histogram for Vol_Shock
hist(eth_clean$Vol_Shock, main = "Histogram: ETH Volatility Shock (Binance)", col = "steelblue", breaks = 50)

# ... (continue with QQ plots, shape.plot, GPD fitting, correlation, etc.)

