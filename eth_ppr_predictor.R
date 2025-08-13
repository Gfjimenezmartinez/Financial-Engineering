# ETH Price Prediction using Projection Pursuit Regression with Binance Data
# Install required packages if needed
if (!require("quantmod")) install.packages("quantmod")
if (!require("ppr")) install.packages("ppr")

library(quantmod)
library(ppr)

# 1. Get live ETH data from Binance
get_eth_data <- function(days = 90) {
  # Get ETH/USDT daily data
  eth_data <- tryCatch({
    getSymbols("ETH-USD", src = "binance", 
               auto.assign = FALSE, 
               periodicity = "daily",
               from = Sys.Date() - days)
  }, error = function(e) {
    message("Error fetching from Binance. Trying Yahoo as fallback...")
    getSymbols("ETH-USD", src = "yahoo", 
               auto.assign = FALSE, 
               from = Sys.Date() - days)
  })
  
  # Convert to data frame and clean
  df <- data.frame(Date = index(eth_data), coredata(eth_data))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  
  # Calculate technical indicators
  df$Return <- c(NA, diff(log(df$Close)))
  df$MA5 <- TTR::SMA(df$Close, n = 5)
  df$MA20 <- TTR::SMA(df$Close, n = 20)
  df$RSI <- TTR::RSI(df$Close, n = 14)
  df$ATR <- TTR::ATR(df[,c("High","Low","Close")], n=14)$atr
  
  # Remove rows with NA values
  df <- na.omit(df)
  
  return(df)
}

# Fetch data (last 180 days)
eth_data <- get_eth_data(180)
cat("Latest ETH data:\n")
print(tail(eth_data))

# 2. Prepare data for PPR
# Predict next day's return using current technicals
target <- "Return"
predictors <- c("MA5", "MA20", "RSI", "ATR", "Volume")

# Create lagged dataset
model_data <- data.frame(
  Y = eth_data$Return[-1],  # Next day's return
  eth_data[-nrow(eth_data), predictors]  # Current day's predictors
)

# 3. Run Projection Pursuit Regression
set.seed(123)
ppr_model <- ppr(Y ~ ., data = model_data, nterms = 3, max.terms = 5)

# 4. Model Evaluation
# Calculate predictions and residuals
predictions <- predict(ppr_model, newdata = model_data)
residuals <- model_data$Y - predictions

# Performance metrics
ssr <- sum(residuals^2)
mse <- mean(residuals^2)
rsq <- 1 - (ssr / sum((model_data$Y - mean(model_data$Y))^2))

cat("\nModel Performance:\n")
cat("Sum of Squared Residuals:", ssr, "\n")
cat("Mean Squared Error:", mse, "\n")
cat("R-squared:", rsq, "\n")

# 5. Diagnostic Plots
par(mfrow = c(2, 2))

# Term plots
for(i in 1:min(3, ppr_model$nterms)) {
  plot(ppr_model, what = "terms", term = i,
       main = paste("PPR Term", i))
}

# Actual vs Predicted
plot(model_data$Y, predictions,
     xlab = "Actual Returns", ylab = "Predicted Returns",
     main = "Actual vs Predicted Returns")
abline(0, 1, col = "red")

# Residuals plot
plot(predictions, residuals,
     xlab = "Predicted Returns", ylab = "Residuals",
     main = "Residuals vs Predicted")
abline(h = 0, col = "red")

# Histogram of residuals
hist(residuals, breaks = 20,
     main = "Distribution of Residuals",
     xlab = "Residuals")

par(mfrow = c(1, 1))

# 6. Latest Prediction
latest_data <- tail(eth_data, 1)[, predictors]
next_day_pred <- predict(ppr_model, newdata = latest_data)

cat("\nLatest Prediction for Next Day's ETH Return:", next_day_pred, "\n")
cat("Which would imply a price change to:", 
    tail(eth_data$Close, 1) * exp(next_day_pred), "\n")
