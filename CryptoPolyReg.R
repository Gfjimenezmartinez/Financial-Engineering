# ==============================================================================
# Script: CryptoPolyReg.R
# Description: Fits L2 (Least Squares) and L1 (Least Absolute Deviations) 
#              polynomial regressions to live Binance BTC/USDT price data.
# Author: [Your Name]
# Date: [Current Date]
# ==============================================================================

# ----------------------------
# 1. Install & Load Libraries
# ----------------------------
if (!require("httr")) install.packages("httr")       # API requests
if (!require("jsonlite")) install.packages("jsonlite") # JSON parsing
if (!require("L1pack")) install.packages("L1pack")   # L1 regression
if (!require("ggplot2")) install.packages("ggplot2")  # Advanced plotting

library(httr)
library(jsonlite)
library(L1pack)
library(ggplot2)

# ----------------------------
# 2. Fetch Live Binance Data
# ----------------------------
get_binance_data <- function(symbol = "BTCUSDT", interval = "1d", limit = 100) {
  url <- paste0(
    "https://api.binance.com/api/v3/klines?",
    "symbol=", symbol,
    "&interval=", interval,
    "&limit=", limit
  )
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  df <- data.frame(
    Time = as.POSIXct(data[, 1]/1000, origin = "1970-01-01"),
    Close = as.numeric(data[, 5])  # Closing prices
  )
  return(df)
}

# Get BTC/USDT daily closing prices
btc_data <- get_binance_data()
X <- 1:nrow(btc_data)  # Time index
Y <- btc_data$Close    # Price data

# ----------------------------
# 3. Fit Polynomial Regressions
# ----------------------------
# Design matrix (X, X², X³)
X_design <- cbind(X, X^2, X^3)

# L2 Regression (Least Squares)
L2_model <- lm(Y ~ X_design)
L2_coef <- coef(L2_model)
L2_fitted <- fitted(L2_model)
L2_criterion <- sum(resid(L2_model)^2)

# L1 Regression (Least Absolute Deviations)
L1_model <- l1fit(X_design, Y, intercept = TRUE)
L1_coef <- L1_model$coefficients
L1_fitted <- cbind(1, X_design) %*% L1_coef
L1_criterion <- sum(abs(Y - L1_fitted))

# ----------------------------
# 4. Results Comparison
# ----------------------------
cat("=== Regression Results ===\n")
cat("L2 Coefficients (Least Squares):\n")
print(L2_coef)
cat("Sum of Squared Residuals:", L2_criterion, "\n\n")

cat("L1 Coefficients (Least Absolute Deviations):\n")
print(L1_coef)
cat("Sum of Absolute Residuals:", L1_criterion, "\n")

# ----------------------------
# 5. Advanced Visualization
# ----------------------------
plot_data <- data.frame(
  Time = btc_data$Time,
  Price = Y,
  L2_Fitted = L2_fitted,
  L1_Fitted = L1_fitted
)

ggplot(plot_data, aes(x = Time)) +
  geom_point(aes(y = Price), color = "gray50", alpha = 0.7) +
  geom_line(aes(y = L2_Fitted), color = "red", linetype = "dashed", size = 1) +
  geom_line(aes(y = L1_Fitted), color = "blue", size = 1) +
  labs(
    title = "BTC/USDT Price: L2 vs L1 Polynomial Regression",
    subtitle = paste("Last", length(X), "days from Binance"),
    x = "Date",
    y = "Price (USDT)"
  ) +
  scale_x_datetime(date_labels = "%b %d") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  annotate("text", 
           x = min(plot_data$Time), 
           y = max(plot_data$Price),
           label = paste("L2 Residuals:", round(L2_criterion, 2)),
           color = "red",
           hjust = 0) +
  annotate("text", 
           x = min(plot_data$Time), 
           y = max(plot_data$Price) * 0.95,
           label = paste("L1 Residuals:", round(L1_criterion, 2)),
           color = "blue",
           hjust = 0)

# ----------------------------
# 6. Save Output
# ----------------------------
png("CryptoPolyReg_Plot.png", width = 1000, height = 600)
print(last_plot())
dev.off()

cat("\nScript executed successfully. Plot saved as 'CryptoPolyReg_Plot.png'.\n")
