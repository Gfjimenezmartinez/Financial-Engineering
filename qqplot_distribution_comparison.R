
# eth_qqplot_analysis.R
# ----------------------------------------
# Script to analyze Ethereum (ETH) daily returns distribution
# using Q-Q plots against Normal and Cauchy distributions.
#
# Sections:
# 1. Fetch ETH price data and calculate daily log returns
# 2. Q-Q plot: ETH returns vs Normal distribution
# 3. Q-Q plot: ETH returns vs Cauchy distribution
#
# Author: [Your Name]
# Date: [Date]
# ----------------------------------------

# Load necessary package
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

# ----------------------------------------
# Part 1: Download ETH data and calculate returns
# ----------------------------------------

# Get ETH price data from Yahoo Finance (last 3 years for good sample size)
getSymbols("ETH-USD", src = "yahoo", from = "2020-01-01")

# Extract closing prices
eth_prices <- Cl(`ETH-USD`)

# Calculate daily log returns
eth_returns <- diff(log(eth_prices))
eth_returns <- na.omit(eth_returns)  # Remove NA created by diff

# Convert to numeric vector for plotting
eth_returns_vec <- as.numeric(eth_returns)

# ----------------------------------------
# Part 2: Q-Q plot comparing ETH returns to Normal distribution
# ----------------------------------------

qqnorm(eth_returns_vec,
       main = "Normal Q-Q Plot: ETH Daily Returns vs Normal")
qqline(eth_returns_vec, col = "red")

# ----------------------------------------
# Part 3: Q-Q plot comparing ETH returns to Cauchy distribution
# ----------------------------------------

# Generate Cauchy sample with same length and scale as ETH returns
set.seed(123)  # For reproducibility
cauchy_sample <- rcauchy(length(eth_returns_vec), location = 0, scale = sd(eth_returns_vec))

qqplot(eth_returns_vec, cauchy_sample,
       xlab = "ETH Daily Returns",
       ylab = "Cauchy Quantiles",
       main = "Q-Q Plot: ETH Returns vs Cauchy Distribution")
abline(0, 1, col = "blue")
