# Load required libraries
library(quantreg)  # For LAD regression
library(httr)      # For API calls
library(jsonlite)  # For JSON parsing
library(ggplot2)   # For advanced visualizations

# 1. Fetch live ETH market data from Binance API
get_eth_data <- function(symbol = "ETHUSDT", interval = "1d", limit = 1000) {
  url <- paste0("https://api.binance.com/api/v3/klines?symbol=", symbol,
               "&interval=", interval, "&limit=", limit)
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  df <- data.frame(
    date = as.POSIXct(as.numeric(data[,1])/1000, origin = "1970-01-01"),
    close = as.numeric(data[,5]),       # Closing price (target variable)
    volume = as.numeric(data[,6]),     # Trading volume (predictor 1)
    volatility = (as.numeric(data[,3]) - as.numeric(data[,4])) / as.numeric(data[,2]) * 100,  # Daily % range
    gas_fee = rnorm(limit, mean = 50, sd = 20)  # Simulated gas fees in Gwei
  )
  return(df)
}

eth_data <- get_eth_data()

## 1. Simple Regression Analysis ##

# 1.1 ETH Price vs Trading Volume
eth_volume <- lm(close ~ volume, data = eth_data)
summary(eth_volume)

# 1.2 ETH Price vs Daily Volatility
eth_volatility <- lm(close ~ volatility, data = eth_data)
summary(eth_volatility)

# 1.3 ETH Price vs Gas Fees
eth_gas <- lm(close ~ gas_fee, data = eth_data)
summary(eth_gas)

# 1.4 Compare model performance
cat("R-squared Values:\n",
    "Volume Model:", summary(eth_volume)$r.squared, "\n",
    "Volatility Model:", summary(eth_volatility)$r.squared, "\n",
    "Gas Fee Model:", summary(eth_gas)$r.squared, "\n")

## 2. Multiple Regression Analysis ##

# 2.1 Price ~ Volume + Volatility
eth_multi1 <- lm(close ~ volume + volatility, data = eth_data)
summary(eth_multi1)

# 2.2 Price ~ Volume + Volatility + Gas Fees
eth_multi2 <- lm(close ~ volume + volatility + gas_fee, data = eth_data)
summary(eth_multi2)

# Model comparison
cat("Multiple Regression R-squared:\n",
    "Volume + Volatility:", summary(eth_multi1)$r.squared, "\n",
    "Volume + Volatility + Gas:", summary(eth_multi2)$r.squared, "\n")

## 3. Robust Regression (LAD) Analysis ##

# 3.1 LAD: Price ~ Volume
eth_volume_lad <- rq(close ~ volume, data = eth_data)
summary(eth_volume_lad)

# 3.2 LAD: Price ~ Volatility
eth_volatility_lad <- rq(close ~ volatility, data = eth_data)
summary(eth_volatility_lad)

# 3.3 LAD: Price ~ Gas Fees
eth_gas_lad <- rq(close ~ gas_fee, data = eth_data)
summary(eth_gas_lad)

# Compare OLS vs LAD coefficients
cat("Volume Coefficient Comparison:\n",
    "OLS:", coef(eth_volume)[2], "\n",
    "LAD:", coef(eth_volume_lad)[2], "\n\n")

cat("Volatility Coefficient Comparison:\n",
    "OLS:", coef(eth_volatility)[2], "\n",
    "LAD:", coef(eth_volatility_lad)[2], "\n")

# Visualization: Actual vs Predicted Prices
eth_data$predicted <- predict(eth_multi1)

ggplot(eth_data, aes(x = date)) +
  geom_line(aes(y = close, color = "Actual Price")) +
  geom_line(aes(y = predicted, color = "Predicted Price")) +
  labs(title = "ETH: Actual vs Predicted Prices",
       subtitle = "Multiple Regression (Volume + Volatility)",
       x = "Date", y = "Price (USDT)") +
  scale_color_manual(values = c("Actual Price" = "black", "Predicted Price" = "blue")) +
  theme_minimal()

# Residual Analysis
par(mfrow = c(1, 2))
plot(fitted(eth_multi1), residuals(eth_multi1),
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

qqnorm(residuals(eth_multi1), main = "Q-Q Plot of Residuals")
qqline(residuals(eth_multi1), col = "red")
par(mfrow = c(1, 1))
