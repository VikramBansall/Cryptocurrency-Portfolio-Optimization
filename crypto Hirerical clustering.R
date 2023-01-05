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
#
View(crypto_list())
cryptocurrency<-crypto_history(limit = 1)
View(cryptocurrency)
summary(cryptocurrency)
class(cryptocurrency$timestamp)
typeof(cryptocurrency$timestamp)
cryptocurrency%>%mutate(timestamp=as.Date(as.character(timestamp)))%>%
  ggplot()+geom_line(aes(timestamp,close))
#
top10marcap<-crypto_list()%>%arrange(rank)%>%slice(1,2,3,4,5,6,7,8,9,10)
View(top10marcap)                                                  
top10prices<-crypto_history(top10marcap)
View(top10prices)
summary(top10prices)
#
#
cryptoreturns<-top10prices%>%arrange(timestamp,symbol)%>%mutate(Daily_Returns=close/lag(close,1)-1)%>%
select(timestamp,name,symbol,Daily_Returns)
View(cryptoreturns)
#
hc <-
  cryptoreturns%>%
  pivot_wider(id_cols = timestamp, names_from = name, values_from = Daily_Returns) %>%
  select(-timestamp) %>%
  cor(use = "complete.obs") %>%
  abs() %>%
  dist() %>%
  hclust()
hc%>%as.dendrogram() %>%plot()
number_clusters <- 4
hc %>%
  as.dendrogram() %>%
  color_branches(k = number_clusters) %>%
  color_labels(k = number_clusters) %>%
  set("labels_cex", 0.3) %>%
  # plot()
  as.ggdend() %>%
  ggplot() +
  labs(title = "Dendrogram of the top 100 Cryptocurrencies by market cap")
#
cutree(hc,k=number_clusters)%>%data.frame()%>%rename(cluster=1)%>%mutate(token_name=row.names(.))%>%filter(cluster==4)
rect.hclust(hc,4)
#
stock_cluster<-cutree(hc,k=5)%>%data.frame()%>%rename(cluster=1) %>%mutate(Company=rownames(.))
randomstock<-stock_cluster%>%group_by(cluster)%>%sample_n(size = 1)
target_cluster <- 2
individual_cluster <-
  stock_cluster %>%
  filter(cluster == target_cluster) %>%
  sample_n(size = 1)
individual_cluster
#
library(tidyquant)
tq_get("AAPL") %>%
  ggplot() +
  geom_line(aes(date, adjusted))
stock_table_url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
all_historical_spx_ticker <-
  stock_table_url %>%
  read_html() %>%
  html_nodes(css = "table") %>%
  extract() %>%
  html_table(fill = T)
#View(all_historical_spx_ticker)
summary(all_historical_spx_ticker)
library(janitor)
current_spx_tickers <-
  all_historical_spx_ticker[[1]] %>%
  clean_names()
pull_all_data <- . %>% print() %>% tq_get() %>% as.data.frame()
tq_get("MMM")
"MMM" %>% pull_all_data()
all_spx_prices <-
  current_spx_tickers %>%
  mutate(symbol = stringr::str_replace_all(string = symbol, pattern = "[.]", replacement = "-")) %>%
  mutate(data = map(symbol, pull_all_data)) %>%
  select(-symbol) %>%
  unnest_legacy()

all_spx_prices %>%
  +   select(-symbol) %>%
  +   unnest_legacy() %>% #pull(symbol) %>% unique
  +   ggplot() +
  +   geom_line(aes(date, adjusted))
 #+
  #facet_wrap(symbol~.)
#
wide_stock_returns <-
  all_spx_prices %>%
  select(date, security, symbol, adjusted) %>%
  arrange(symbol, date) %>%
  group_by(symbol) %>%
  mutate(daily_return = adjusted/lag(adjusted)-1) %>%
  na.omit()%>%
  pivot_wider(id_cols = date, names_from = security, values_from = daily_return)
#
hc <-
  wide_stock_returns %>%
  select(-date) %>%
  cor(use = "complete.obs") %>%
  dist() %>%
  hclust()
#
hc %>%
  as.dendrogram() %>%
  color_branches(k = 5) %>%
  color_labels(k = 5) %>%
  set("labels_cex", 0.3) %>%
  # plot()
  as.ggdend() %>%
  ggplot() +
  labs(title = "Dendrogram of the S&P500 Constituents")
#
#seq(1,5)
portfolio_1 <-
  all_spx_prices %>%
  filter(security %in% randomstock$Company) %>%
  arrange(symbol, date) %>%
  group_by(symbol) %>%
  mutate(closing_price = adjusted/lag(adjusted, 1)-1) %>%
  pivot_wider(id_cols = date, names_from = symbol, values_from = closing_price)
#
portfolio_2 <-
  all_spx_prices %>%
  filter(security %in% individual_cluster$Company) %>%
  arrange(symbol, date) %>%
  group_by(symbol) %>%
  mutate(closing_price = adjusted/lag(adjusted, 1)-1) %>%
  pivot_wider(id_cols = date, names_from = symbol, values_from = closing_price)
#
portfolio_1_xts <- xts(x = portfolio_1[-1], order.by = portfolio_1$date)
portfolio1_returns <- Return.portfolio(portfolio_1_xts)
portfolio_2_xts <- xts(x = portfolio_2[-1], order.by = portfolio_2$date, frequency = 252)
portfolio2_returns <- Return.portfolio(portfolio_2_xts)


#

portfolio1_returns %>%
  as.data.frame() %>%
  mutate(date = rownames(.),
         date = ymd(date)) %>%
  rename(DiversifiedPortfolio=1) %>%
  inner_join(
    portfolio2_returns %>%
      as.data.frame() %>%
      mutate(date = rownames(.),
             date = ymd(date)) %>%
      rename(SingleClusterPortfolio=1)
  ) %>%
  arrange(date) %>%
  mutate(DiversifiedPortfolio_Cumulative = cumprod(1+DiversifiedPortfolio)-1,
         SingleClusterPortfolio_Cumalative = cumprod(1+SingleClusterPortfolio)-1) %>%
  ggplot() +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_line(aes(date, SingleClusterPortfolio_Cumalative), color = "blue") +
  geom_line(aes(date, DiversifiedPortfolio_Cumulative), color = "orange")
labs(title = "Comparison of Portfolio Returns",
     subtitle = "Portfolio using hierarchial clustering (orange) vs. Single Cluster",
     y = "cumulative return (%)") +
  scale_y_continuous(labels = scales::percent)







