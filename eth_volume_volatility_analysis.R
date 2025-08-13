# Load required packages
library(httr)       # For API calls
library(jsonlite)   # For JSON parsing
library(quantreg)   # For LAD regression
library(ggplot2)    # For advanced plots

# 1. Get live ETH data from Binance API
get_binance_data <- function(symbol = "ETHUSDT", interval = "1d", limit = 2000) {
  url <- paste0("https://api.binance.com/api/v3/klines?symbol=", symbol,
                "&interval=", interval, "&limit=", limit)
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  df <- data.frame(
    date = as.POSIXct(as.numeric(data[,1])/1000, origin = "1970-01-01"),
    open = as.numeric(data[,2]),
    high = as.numeric(data[,3]),
    low = as.numeric(data[,4]),
    close = as.numeric(data[,5]),
    volume = as.numeric(data[,6]),
    stringsAsFactors = FALSE
  )
  
  # Calculate daily volatility (range as % of open)
  df$volatility <- (df$high - df$low) / df$open * 100
  return(df)
}

# Fetch data (2000 most recent daily observations)
eth_data <- get_binance_data()

# 2. Assess linear relationship
ggplot(eth_data, aes(x = volume, y = volatility)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "ETH: Trading Volume vs. Price Volatility",
       x = "Daily Trading Volume (ETH)",
       y = "Daily Volatility (% range)") +
  theme_minimal()

# 3. OLS Regression
eth_lm <- lm(volatility ~ volume, data = eth_data)
summary(eth_lm)

# Extract coefficients
lm_coef <- coef(eth_lm)
cat("OLS Results:\n",
    "Intercept:", lm_coef[1], "\n",
    "Slope:", lm_coef[2], "\n",
    "Noise Variance:", sigma(eth_lm)^2, "\n")

# 4. Model assessment
eth_data$fitted <- fitted(eth_lm)

# Fitted vs actual plot
ggplot(eth_data, aes(x = volatility, y = fitted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "OLS: Fitted vs Actual Volatility",
       subtitle = paste("R² =", round(summary(eth_lm)$r.squared, 3)),
       x = "Actual Volatility (%)",
       y = "Fitted Volatility (%)") +
  theme_minimal()

# Predict at specific volume levels (20K, 50K, 75K ETH)
vol_pred <- predict(eth_lm, newdata = data.frame(volume = c(2e4, 5e4, 7.5e4)))
cat("\nPredicted Volatility at:\n",
    "20K ETH volume:", vol_pred[1], "%\n",
    "50K ETH volume:", vol_pred[2], "%\n",
    "75K ETH volume:", vol_pred[3], "%\n")

# 5. Residual analysis
eth_data$residuals <- residuals(eth_lm)

# Residual plots
ggplot(eth_data, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "OLS Residual Analysis",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

qqnorm(eth_data$residuals, main = "Q-Q Plot of Residuals")
qqline(eth_data$residuals, col = "red")

# 6. LAD Regression (Robust to outliers)
eth_rq <- rq(volatility ~ volume, data = eth_data)
summary(eth_rq)

# Compare coefficients
cat("\nLAD vs OLS Coefficients:\n")
print(data.frame(
  Method = c("OLS", "LAD"),
  Intercept = c(lm_coef[1], coef(eth_rq)[1]),
  Slope = c(lm_coef[2], coef(eth_rq)[2])
))

# LAD predictions
rq_pred <- predict(eth_rq, newdata = data.frame(volume = c(2e4, 5e4, 7.5e4)))
cat("\nLAD Predictions at:\n",
    "20K ETH volume:", rq_pred[1], "%\n",
    "50K ETH volume:", rq_pred[2], "%\n",
    "75K ETH volume:", rq_pred[3], "%\n")
