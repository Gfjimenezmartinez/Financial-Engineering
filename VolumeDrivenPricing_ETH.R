# ==============================================================================
# Script: ETH_Price_Analysis.R
# Description: Performs nonlinear regression on ETH trading volume vs price data
#              using a modified Michaelis-Menten model adapted for crypto markets
# ==============================================================================

# ----------------------------
# 1. Load Required Libraries
# ----------------------------
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
library(httr)
library(jsonlite)

# ----------------------------
# 2. Fetch Live ETH Market Data from Binance
# ----------------------------
get_eth_data <- function(limit = 100) {
  url <- "https://api.binance.com/api/v3/klines?symbol=ETHUSDT&interval=1d&limit=100"
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  data.frame(
    time = as.POSIXct(data[,1]/1000, origin = "1970-01-01"),
    price = as.numeric(data[,4]),  # Closing price
    volume = as.numeric(data[,5])  # Trading volume
  )
}

eth_data <- get_eth_data()

# ----------------------------
# 3. Prepare Data for Modeling
# ----------------------------
# We'll model price as a function of trading volume (analogous to enzyme kinetics)
x <- eth_data$volume  # Trading volume as independent variable
y <- eth_data$price   # ETH price as dependent variable

# ----------------------------
# 4. Define Modified Michaelis-Menten Model for Crypto
# ----------------------------
# Adapted to represent price saturation effects at high trading volumes
crypto_mm <- function(volume, Pmax, K) {
  Pmax * volume / (K + volume)
}

# ----------------------------
# 5. Perform Nonlinear Regression
# ----------------------------
# Starting parameters:
# Pmax = maximum possible price (saturation point)
# K = volume needed to reach half of Pmax
model <- nls(y ~ crypto_mm(x, Pmax, K),
            start = list(Pmax = max(y)*1.2, K = median(x)),
            control = nls.control(maxiter = 100))

# Print parameter estimates
cat("\nETH Price-Volume Relationship Model:\n")
print(summary(model))

# ----------------------------
# 6. Visualization
# ----------------------------
# Create sequence for smooth curve
x_seq <- seq(min(x), max(x), length.out = 100)
y_pred <- predict(model, newdata = list(x = x_seq))

plot(x, y, 
     pch = 19, col = "dodgerblue",
     main = "ETH Price vs Trading Volume (Michaelis-Menten Model)",
     xlab = "24h Trading Volume (ETH)", 
     ylab = "Price (USDT)")

lines(x_seq, y_pred, col = "red", lwd = 2)

legend("bottomright",
       legend = c("Actual Price", "Model Prediction"),
       col = c("dodgerblue", "red"),
       pch = c(19, NA),
       lty = c(NA, 1),
       lwd = c(NA, 2))

# ----------------------------
# 7. Interpretation
# ----------------------------
cat("\nKey Findings:\n")
cat("- Pmax (price saturation point):", coef(model)["Pmax"], "USDT\n")
cat("- K (volume for half-saturation):", coef(model)["K"], "ETH volume\n")
cat("- Model suggests ETH price approaches", round(coef(model)["Pmax"], 2),
    "USDT at very high trading volumes\n")
