
# ----------------------------------------
# Compare normality of Ethereum daily log returns (2023)
# using Q-Q plots and real Yahoo Finance data (via tidyquant).
# ----------------------------------------
# Install and load required packages
library(tidyquant)

# ----------------------------------------
# 1. Download ETH-USD prices from Yahoo Finance via tidyquant
# ----------------------------------------
eth_data <- tq_get("ETH-USD", from = "2023-01-01", to = Sys.Date())

# View structure of downloaded data
# str(eth_data)

# Calculate daily log returns from adjusted closing prices
eth_data <- eth_data %>%
  dplyr::mutate(log_return = log(adjusted / dplyr::lag(adjusted))) %>%
  dplyr::filter(!is.na(log_return))

# Sample size for Q-Q plot
N <- 128
set.seed(42)
eth_sample <- sample(eth_data$log_return, N)

# ----------------------------------------
# 2. Q-Q Plot: ETH Returns vs Normal distribution
# ----------------------------------------
qqnorm(eth_sample,
       main = "Q-Q Plot: ETH Returns vs Normal Distribution")
qqline(eth_sample, col = "red")

# ----------------------------------------
# 3. Q-Q Plot: Synthetic Normal Sample vs Normal distribution
# ----------------------------------------
normal_sample <- rnorm(N)
qqnorm(normal_sample,
       main = "Q-Q Plot: Synthetic Normal Sample vs Normal Distribution")
qqline(normal_sample, col = "red")

# ----------------------------------------
# 4. Q-Q Plot: Synthetic Exponential Sample vs Normal distribution
# ----------------------------------------
exp_sample <- rexp(N, rate = 1)
qqnorm(exp_sample,
       main = "Q-Q Plot: Exponential Sample vs Normal Distribution")
qqline(exp_sample, col = "red")

# ----------------------------------------
# End of script
# ----------------------------------------
