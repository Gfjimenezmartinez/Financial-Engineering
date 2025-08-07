# Lupe Jimenez

# Problem 2.4 (Crypto Version): GPD Fit and Goodness-of-Fit Tests on ETH Lower Tail

# ========================
# Load required libraries
# ========================
library(Rsafd)     # For GPD fitting and plotting
library(quantmod)  # To get crypto price data from Yahoo Finance

# ========================
# Step 1: Download ETH data and compute daily log returns
# ========================

# Download Ethereum (ETH-USD) daily prices for last 3 years
getSymbols("ETH-USD", src = "yahoo", from = Sys.Date() - 3*365)

# Extract adjusted closing prices
eth_prices <- Cl(`ETH-USD`)

# Calculate daily log returns
eth_log_returns <- diff(log(eth_prices))
eth_log_returns <- na.omit(eth_log_returns)

# Convert to numeric vector
eth_returns <- coredata(eth_log_returns)

# ========================
# Step 2: Fit GPD to lower 5% tail of ETH log returns
# ========================

# Define the 5% quantile threshold
eth_thresh <- quantile(eth_returns, 0.05)

# Calculate exceedances over the threshold (note: transform to positive excesses)
eth_exc <- -(eth_returns[eth_returns < eth_thresh] - eth_thresh)

# Fit GPD to the exceedances using maximum likelihood
gpd_fit <- gpd.ml(eth_exc)

# Extract estimated parameters: location (m), scale (lambda), shape (xi)
gpd_params <- gpd_fit$param.est
cat(sprintf("ETH GPD Fit Parameters:\nm = %.4f, lambda = %.4f, xi = %.4f\n",
            gpd_params[1], gpd_params[2], gpd_params[3]))

# ========================
# Step 3: Generate Monte Carlo sample from fitted GPD
# ========================

# Set sample size to 5 times number of exceedances
sample_size <- 5 * length(eth_exc)

# Generate random sample from fitted GPD
Montecarlo_sample <- rpareto(n = sample_size,
                             m = gpd_params[1],
                             lambda = gpd_params[2],
                             xi = gpd_params[3])

# Transform back to original scale of returns (negative tail)
Montecarlo_sample <- -Montecarlo_sample + eth_thresh

# ========================
# Step 4: Q-Q plot comparing ETH lower tail and Monte Carlo sample
# ========================

qqplot(eth_returns[eth_returns < eth_thresh], 
       sample(Montecarlo_sample, length(eth_exc)),
       xlab = "Empirical ETH Lower Tail Returns",
       ylab = "Simulated GPD Sample",
       main = "Q-Q Plot: ETH Negative Returns vs Simulated GPD")
abline(0, 1, col = "red")  # 45-degree reference line

# ========================
# Step 5: Kolmogorov-Smirnov test for goodness-of-fit
# ========================

ks_result <- ks.test(eth_returns[eth_returns < eth_thresh], 
                     sample(Montecarlo_sample, length(eth_exc)))

print(ks_result)
