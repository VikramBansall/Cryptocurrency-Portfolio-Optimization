#Required libraries
library(tidyverse)
library(crypto2)
library(lubridate)
library(rvest)
library(stats)
library(magrittr)
library(quantmod)
library(tidyquant)
library(dendextend)
library(PortfolioAnalytics)

#list of cryptocurrencies
View(crypto_list())

#historical data for a single cryptocurrency
cryptocurrency <- crypto_history(limit = 1)
View(cryptocurrency)
summary(cryptocurrency)
class(cryptocurrency$timestamp)
typeof(cryptocurrency$timestamp)

# Visualize historical data for the single cryptocurrency
cryptocurrency %>%
  mutate(timestamp = as.Date(as.character(timestamp))) %>%
  ggplot() + geom_line(aes(timestamp, close))


# top 100 cryptocurrencies for Portfolio
top100marcap <- crypto_list() %>% arrange(rank) %>% slice(1:100)
top100prices <- crypto_history(top100marcap)

# Calculate daily returns for each cryptocurrency
cryptoreturns <- top100prices %>%
  arrange(timestamp, symbol) %>%
  mutate(Daily_Returns = close/lag(close, 1) - 1) %>%
  select(timestamp, name, symbol, Daily_Returns)

# Hierarchical clustering for cryptocurrency returns
hc_crypto <- cryptoreturns %>%
  pivot_wider(id_cols = timestamp, names_from = name, values_from = Daily_Returns) %>%
  select(-timestamp) %>%
  cor(use = "complete.obs") %>%
  abs() %>%
  dist() %>%
  hclust()

# Visualize the dendrogram
hc_crypto %>%
  as.dendrogram() %>%
  color_branches(k = number_clusters) %>%
  color_labels(k = number_clusters) %>%
  set("labels_cex", 0.3) %>%
  as.ggdend() %>%
  ggplot() +
  labs(title = "Dendrogram of the top 100 Cryptocurrencies by market cap")

# Assign clusters to cryptocurrencies
crypto_clusters <- cutree(hc_crypto, k = number_clusters) %>%
  data.frame() %>%
  rename(cluster = 1) %>%
  mutate(token_name = row.names(.)) %>%
  filter(cluster == 4)

# Retrieve historical prices for the selected cluster
crypto_portfolio <- crypto_history(crypto_clusters$token_name)

# Visualize and compare cumulative returns of the cryptocurrency portfolio
crypto_returns <- crypto_portfolio %>%
  arrange(timestamp, symbol) %>%
  mutate(Daily_Returns = close/lag(close, 1) - 1) %>%
  pivot_wider(id_cols = timestamp, names_from = symbol, values_from = Daily_Returns)

crypto_returns_xts <- xts(x = crypto_returns[-1], order.by = crypto_returns$timestamp)
crypto_portfolio_returns <- Return.portfolio(crypto_returns_xts)

# Visualize and compare cumulative returns of the cryptocurrency portfolio
crypto_portfolio_returns %>%
  as.data.frame() %>%
  mutate(date = rownames(.),
         date = ymd(date)) %>%
  ggplot() +
  geom_line(aes(date, 1), color = "black") +
  geom_line(aes(date, cumprod(1 + .)), color = "blue") +
  labs(title = "Cryptocurrency Portfolio Returns",
       y = "cumulative return (%)") +
  scale_y_continuous(labels = scales::percent)

# Calculate daily returns for the top 100 cryptocurrencies
cryptoreturns <- top100prices %>%
  arrange(timestamp, symbol) %>%
  mutate(Daily_Returns = close/lag(close, 1) - 1) %>%
  select(timestamp, name, symbol, Daily_Returns)
View(cryptoreturns)

# Hierarchical clustering of cryptocurrencies based on returns
hc <- cryptoreturns %>%
  pivot_wider(id_cols = timestamp, names_from = name, values_from = Daily_Returns) %>%
  select(-timestamp) %>%
  cor(use = "complete.obs") %>%
  abs() %>%
  dist() %>%
  hclust()

# Visualize the dendrogram
hc %>% as.dendrogram() %>% plot()

# Determine clusters and visualize
number_clusters <- 4
hc %>%
  as.dendrogram() %>%
  color_branches(k = number_clusters) %>%
  color_labels(k = number_clusters) %>%
  set("labels_cex", 0.3) %>%
  as.ggdend() %>%
  ggplot() +
  labs(title = "Dendrogram of the top 100 Cryptocurrencies by market cap")

# Cut the tree to obtain clusters
cutree(hc, k = number_clusters) %>% data.frame() %>%
  rename(cluster = 1) %>%
  mutate(token_name = row.names(.)) %>%
  filter(cluster == 4)
rect.hclust(hc, 4)
###############################################

# Extract a specific cluster and a random crypto from that cluster
crypto_cluster <- cutree(hc, k = 5) %>%
  data.frame() %>%
  rename(cluster = 1) %>%
  mutate(Company = rownames(.))
randomcluster <- crypto_cluster %>%
  group_by(cluster) %>%
  sample_n(size = 1)
target_cluster <- 2
individual_cluster <- crypto_cluster %>%
  filter(cluster == target_cluster) %>%
  sample_n(size = 1)
individual_cluster

# Select cryptos for Portfolio 1 and Portfolio 2
crypto_portfolio_1 <- crypto_portfolio %>%
  filter(symbol %in% randomcluster$Company) %>%
  arrange(timestamp, symbol) %>%
  group_by(symbol) %>%
  mutate(closing_price = close/lag(close, 1) - 1) %>%
  pivot_wider(id_cols = timestamp, names_from = symbol, values_from = closing_price)

crypto_portfolio_2 <- crypto_portfolio %>%
  filter(symbol %in% individual_cluster$Company) %>%
  arrange(timestamp, symbol) %>%
  group_by(symbol) %>%
  mutate(closing_price = close/lag(close, 1) - 1) %>%
  pivot_wider(id_cols = timestamp, names_from = symbol, values_from = closing_price)

# Convert data to xts for portfolio returns calculation
crypto_portfolio_1_xts <- xts(x = crypto_portfolio_1[-1], order.by = crypto_portfolio_1$date)
crypto_portfolio1_returns <- Return.portfolio(crypto_portfolio_1_xts)

crypto_portfolio_2_xts <- xts(x = crypto_portfolio_2[-1], order.by = crypto_portfolio_2$date, frequency = 252)
crypto_portfolio2_returns <- Return.portfolio(crypto_portfolio_2_xts)

# Visualize and compare cumulative returns of the portfolios
crypto_portfolio1_returns %>%
  as.data.frame() %>%
  mutate(date = rownames(.),
         date = ymd(date)) %>%
  rename(DiversifiedCryptoPortfolio = 1) %>%
  inner_join(
    crypto_portfolio2_returns %>%
      as.data.frame() %>%
      mutate(date = rownames(.),
             date = ymd(date)) %>%
      rename(SingleClusterCryptoPortfolio = 1)
  ) %>%
  arrange(date) %>%
  mutate(DiversifiedCryptoPortfolio_Cumulative = cumprod(1 + DiversifiedCryptoPortfolio) - 1,
         SingleClusterCryptoPortfolio_Cumulative = cumprod(1 + SingleClusterCryptoPortfolio) - 1) %>%
  ggplot() +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_line(aes(date, SingleClusterCryptoPortfolio_Cumulative), color = "blue") +
  geom_line(aes(date, DiversifiedCryptoPortfolio_Cumulative), color = "orange") +
  labs(title = "Comparison of Cryptocurrency Portfolio Returns",
       subtitle = "Portfolio using hierarchical clustering (orange) vs. Single Cluster",
       y = "cumulative return (%)") +
  scale_y_continuous(labels = scales::percent)
