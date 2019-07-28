# 
# R script for trend following system for gold and silver prices 
# 
# Author: Joseph Loss, loss2@illinois.edu
# Created on 6/30/2019
#

## BUY & HOLD STRATEGY ------------------------------------------------------------------------
# Buy & Hold Performance Summary:
BnH.symbols = c(etf.symbols)

length(BnH.symbols)

start_date = as.Date("2010-01-02")  # set start date
end_date = as.Date("2019-07-01")    # set end date

# create new environment to contain stock price data
dataEnv = new.env()

# download data          
suppressMessages(getSymbols(BnH.symbols,
                            env = dataEnv,
                            from = start_date,
                            to = end_date,
                            src = "yahoo",
                            auto.assign = TRUE,
                            adjust = TRUE))

# load Systematic Investor Toolbox for helpful functions:
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

# helper function for extracting Closing price of getSymbols output and for date alignment:
bt.prep(dataEnv, align = 'remove.na')   # now all time series are correctly aligned


# price data
stock_prices = dataEnv$prices
head(stock_prices[,1:3])


# calculate returns
stock_returns = Return.calculate(stock_prices, 
                                 method = c("discrete"))
head(stock_returns[,1:3])

# plot performance for each security
charts.PerformanceSummary(stock_returns[,], 
                          main = 'ETF Absolute Performance Comparison', 
                          legend.loc = "topleft")


# GOLD vs. SP500 ---------------------------------------------------------------------------
# GOLD_vs_SP.Env = new.env()
GOLD.index = Quandl("LBMA/GOLD", type="xts")[,c(1)]
colnames(GOLD.index) = c("LBMA Gold Index")

GOLD.index <- subset(GOLD.index,
                     + index(GOLD.index) >= "1968-01-02" &
                         + index(GOLD.index) <= "2019-07-01")
GOLD.index = na.omit(GOLD.index)

GOLD.returns = Return.calculate(GOLD.index, 
                                method = c("discrete"))

# Import S&P500 data and calculate returns
SP500.data = suppressMessages(getSymbols("^GSPC",
                                         env = dataEnv,
                                         from = "1968-01-02",
                                         to = "2019-07-01",
                                         src = "yahoo",
                                         auto.assign = FALSE,
                                         adjust = TRUE))
SP500.data = SP500.data[,6]
SP500.data = na.omit(SP500.data)
colnames(SP500.data) = c("SP500 Index")

SP500.returns = Return.calculate(SP500.data, 
                                 method = c("discrete"))

GOLD.SP500.returns = cbind(GOLD.returns, SP500.returns) 

# plot performance for Gold vs. S&P500:
charts.PerformanceSummary(GOLD.SP500.returns[,c(1,2)], 
                          main = 'LBMA/Gold Index vs. S&P500',
                          legend.loc = "topleft")

