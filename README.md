# Portfolio-Optimization
Stock portfolio optimization in R

library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(tidyverse)
library(tidyr)
library(dplyr)

#Create a vector of portfolio tickers
tickers <- c('CLX', 'KO', 'AAPL', 'JNJ', 'VZ', 'PG', 'FB', 'AMZN')

#Set portfolio to NULL
portfolio_prices <- NULL

#Populate portfolio_prices with price data for each ticker
for (ticker in tickers) {
  portfolio_prices <- cbind(portfolio_prices, 
                            getSymbols.yahoo(ticker, 
                                             from = '2015-08-01',
                                             periodicity = 'daily', 
                                             auto.assign = F)[,6])
}

#Generate daily return data from portfolio_prices
portfolio_returns <- na.omit(ROC(portfolio_prices, 
                                 type = 'discrete'))

# Set column names for portfolio_returns
colnames(portfolio_returns) <- tickers


# Formulate a portfolio optimization model with 
# an objective function and constraints

# Create a portfolio formulation object that will contain the objective function
# and constraints
portfolio_form <- portfolio.spec(colnames(portfolio_returns))

# add constraints
# Constraint 1: Full investment. Sum of weights must = 1
portfolio_form <- add.constraint(portfolio_form, 
                                 type = 'weight_sum', min_sum = 1, max_sum = 1)

# Constraint 2: Box constraint. Min and Max for each asset within portfolio
portfolio_form <- add.constraint(portfolio_form, 
                                 type = 'box', min = .08, max = .20)

# add objectives
# Objective 1: Maximize expected return
portfolio_form <- add.objective(portfolio_form,
                                type = 'return', name = 'mean')

# Objective 2: Minimize variance 
portfolio_form <- add.objective(portfolio_form,
                                type = 'risk', name = 'StdDev')

# Optimization using solver
optimized_portfolio <- optimize.portfolio(portfolio_returns, 
                                          portfolio_form, 
                                          optimize_method = 'ROI')

print(optimized_portfolio)
