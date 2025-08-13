library(mvtnorm)
library(ggplot2)
library(dplyr)
library(tidyr)

# Problem: Crypto Copula Dependence Analysis
# Simulate BTC and ETH returns with varying dependence (ρ) using Gaussian copulas
# Analyze correlations under different marginal distributions

# 1. Parameters
rhos <- seq(0, 1, by = 0.1)  # Correlation parameters
N <- 2000                     # Sample size
df_t <- 3                     # Degrees of freedom for Student-t distribution

# 2. Initialize results storage
results <- tibble(
  rho = numeric(),
  type = character(),
  pearson = numeric(),
  kendall = numeric(),
  spearman = numeric()
)

# 3. Main simulation loop
for (rho in rhos) {
  # 3.1 Generate Gaussian copula samples
  sigma <- matrix(c(1, rho, rho, 1), nrow = 2)
  gaussian_copula <- pnorm(rmvnorm(N, mean = c(0, 0), sigma = sigma))
  
  # 3.2 Transform to different marginals
  
  # Normal returns (mean 0, volatility 1)
  btc_norm <- qnorm(gaussian_copula[, 1])
  eth_norm <- qnorm(gaussian_copula[, 2])
  
  # Student-t returns (fat-tailed, df=3)
  btc_t <- qt(gaussian_copula[, 1], df = df_t)
  eth_t <- qt(gaussian_copula[, 2], df = df_t)
  
  # 3.3 Compute correlations for normal marginals
  pearson_norm <- cor(btc_norm, eth_norm, method = "pearson")
  kendall_norm <- cor(btc_norm, eth_norm, method = "kendall")
  spearman_norm <- cor(btc_norm, eth_norm, method = "spearman")
  
  # 3.4 Compute correlations for t marginals
  pearson_t <- cor(btc_t, eth_t, method = "pearson")
  kendall_t <- cor(btc_t, eth_t, method = "kendall")
  spearman_t <- cor(btc_t, eth_t, method = "spearman")
  
  # 3.5 Store results
  results <- results %>%
    add_row(
      rho = rho,
      type = "Normal",
      pearson = pearson_norm,
      kendall = kendall_norm,
      spearman = spearman_norm
    ) %>%
    add_row(
      rho = rho,
      type = "Student-t",
      pearson = pearson_t,
      kendall = kendall_t,
      spearman = spearman_t
    )
}

# 4. Visualization

# 4.1 Pearson correlation comparison
ggplot(results, aes(x = rho, y = pearson, color = type)) +
  geom_line() +
  geom_point() +
  labs(title = "Pearson Correlation: Normal vs Student-t Marginals",
       subtitle = "Simulated BTC-ETH Returns",
       x = "Copula Parameter (ρ)",
       y = "Pearson Correlation") +
  theme_minimal()

# 4.2 Rank correlations comparison
results_long <- results %>%
  pivot_longer(cols = c(kendall, spearman), 
               names_to = "cor_type", 
               values_to = "cor_value")

ggplot(results_long, aes(x = rho, y = cor_value, color = type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~cor_type, scales = "free_y",
             labeller = labeller(cor_type = c(
               kendall = "Kendall's Tau",
               spearman = "Spearman's Rho"))) +
  labs(title = "Rank Correlations: Normal vs Student-t Marginals",
       subtitle = "Simulated BTC-ETH Returns",
       x = "Copula Parameter (ρ)",
       y = "Correlation Value") +
  theme_minimal()

# 5. Summary statistics
results %>%
  group_by(type) %>%
  summarise(
    avg_pearson = mean(pearson),
    avg_kendall = mean(kendall),
    avg_spearman = mean(spearman)
  )
