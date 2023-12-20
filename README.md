
The R code examines cryptocurrency information by looking at past prices, grouping cryptocurrencies based on how their values change over time, building investment portfolios from these groups, and showing how well these portfolios perform. This analysis involves exploring the data visually, organizing cryptocurrencies into clusters using a specific method, and then comparing the returns of different portfolios. The goal is to gain insights into how cryptocurrencies behave and perform in the market.
Loading Libraries:

The code begins by loading several R libraries, including data manipulation (tidyverse), cryptocurrency data retrieval (crypto2), date-time handling (lubridate), web scraping (rvest), statistical functions (stats), financial data analysis (quantmod, tidyquant, PortfolioAnalytics), and hierarchical clustering (dendextend).
Getting Cryptocurrency List:

1.It retrieves a list of cryptocurrencies using the crypto_list() function.
Fetching Historical Data for a Single Cryptocurrency:

2.The code fetches historical data for a single cryptocurrency using the crypto_history() function.
Visualizing Historical Data:

3.It uses ggplot to visualize the historical data of the single cryptocurrency by plotting a line chart of closing prices over time.
Selecting Top 100 Cryptocurrencies:

4.It selects the top 100 cryptocurrencies based on market capitalization.
Calculating Daily Returns for Each Cryptocurrency:

5.It calculates daily returns for each cryptocurrency based on their closing prices.
Hierarchical Clustering for Cryptocurrency Returns:

6.It performs hierarchical clustering on the daily returns of the top 100 cryptocurrencies.
Visualizing Dendrogram:

7.It visualizes the resulting dendrogram from hierarchical clustering.
Assigning Clusters to Cryptocurrencies:

8.It assigns clusters to cryptocurrencies based on the hierarchical clustering results.
Retrieving Historical Prices for Selected Cluster:

9.It retrieves historical prices for the cryptocurrencies in a specific cluster.
Visualizing and Comparing Cumulative Returns:

10,It visualizes and compares cumulative returns of the cryptocurrency portfolio using ggplot.
Hierarchical Clustering of Cryptocurrencies Based on Returns:

11.It performs hierarchical clustering again, this time using the daily returns of the top 100 cryptocurrencies.
Visualizing the Second Dendrogram:

12.It visualizes the resulting dendrogram from the second hierarchical clustering.
Determining Clusters and Visualizing:

13.It determines clusters and visualizes them in a dendrogram.
Cutting the Tree and Extracting a Specific Cluster:

14.It cuts the hierarchical tree to obtain clusters and extracts a specific cluster.
Selecting Cryptos for Portfolios:

15.It selects cryptocurrencies for two different portfolios.
Converting Data for Portfolio Returns Calculation:

16.It converts data to xts format for portfolio returns calculation using Return.portfolio.
Visualizing and Comparing Cumulative Returns of Portfolios:

It visualizes and compares cumulative returns of two portfolios using ggplot.




