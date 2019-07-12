# main.r
# R script for trend following system for gold and silver prices 
# 
# Author: Joseph Loss, loss2@illinois.edu
# Created on 6/30/2019
#

# LIBRARY REQUIREMENTS ---------------------------------------------------
# for debugging and/or setup help, visit the Quantstrat tutorial:
# https://www.youtube.com/watch?v=61_F2fcvrsw
#
require(quantstrat)
require(devtools)
require(DSTrading)      # additional trading functionality
require(IKTrading)      # allows Digital-Signal trading
require(TTR)
require(xts)
require(PerformanceAnalytics)
require(Quandl)
require(quantmod)
require(qmao)   # Quantmod add-on

# if you want premium data you must enter a Premium API key:
Quandl.api_key(MY_QUANDL_API_KEY)      



## BEGIN QUANTSTRAT TRADING -------------------------------------------------------------------------
# Configuration Metadata ----------------------------------------------------------------------------
options(width=70)
options("getSymbols.warning4.0" = FALSE)

# NOTE: Stock blotter must be reset before every portfolio run
rm(list = ls(.blotter), envir = .blotter)   # reset stock blotter
currency('USD')                             # set portfolio currency
Sys.setenv(TZ = "UTC")                      # set timezone

etf.symbols = c("GDX", "GDXJ", "SPY", "ZJG.TO")

symbols = etf.symbols[2:3]        # choose which symbol or symbols to use in strategy

suppressMessages(getSymbols(symbols,
                            from = "2010-01-02",
                            to = "2019-07-01",
                            src = "yahoo",
                            adjust = TRUE))
stock(symbols, currency = "USD", multiplier = 1)
ZJG.TO = na.approx(ZJG.TO)

initDate = "1960-01-01"         # must have initDate BEFORE first day of price data
tradeSize <- 10000                    # trade sizing
initEq <- tradeSize*length(symbols)   # initial equity size


## Create strategy, portfolio, and account (blotter) with name "SMAC-4m-12m"
strategy.st <- portfolio.st <- account.st <- "SMAC-4m-12m"

rm.strat(portfolio.st)  # remove portfolio after each backtest or quantstrat will fail
rm.strat(strategy.st)   # remove strategy after each backtest or quantstrat will fail


# Initialization -------------------------------------------------------------
# init portfolio, account, orders, and strategy
initPortf(portfolio.st, 
          symbols = symbols,
          initDate = initDate, 
          currency = "USD")
initAcct(account.st, 
         portfolios = portfolio.st, 
         initDate = initDate, 
         currency = "USD",
         initEq = initEq)
initOrders(portfolio.st,
           initDate = initDate)

strategy(strategy.st, store=TRUE)


# Parameters -----------------------------------------------------------------
nRSI = 2
thresh1 = 10
thresh2 = 6

nSMAexit = 5
nSMAfilter = 200

period = 10
pctATR = 0.02 # 2% of notional trade size 
maxPct = 0.04 


# Indicators ------------------------------------------------------------------
# Average True Range (1-day lag):
add.indicator(strategy.st, name="lagATR",
              arguments = list(HLC = quote(HLC(mktdata)), n = period),
              label="atrX")

# Relative Strength Index:
add.indicator(strategy.st, name="RSI",
              arguments = list(price = quote(Cl(mktdata)), n=nRSI),
              label="rsi")

# 5-day Moving Average:
add.indicator(strategy.st, name="SMA",
              arguments=list(x = quote(Cl(mktdata)), n=nSMAexit),
              label="quickMA")

# 200-day Moving Average:
add.indicator(strategy.st, name="SMA",
              arguments=list(x = quote(Cl(mktdata)), n=nSMAfilter),
              label="filterMA")


# Signals ---------------------------------------------------------------------
# sigComparison: compares 2 signals
# sigThreshold: compares > or < number
# sigAnd: intersection of 2 values
# sigCrossover: whether one signal crosses another
# relationship: gt, lt, gte, lte: (>, <, >=, <=)

add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("Close", "filterMA"),
                          relationship = "gt"), 
           label="upTrend")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="rsi", threshold = thresh1,
                          relationship = "lt"), cross=FALSE, 
           label="rsiThresh1")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="rsi", threshold = thresh2,
                          relationship = "lt"), cross=FALSE, 
           label="rsiThresh2")

add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("rsiThresh1", "upTrend"),
                          cross=TRUE),
           label="longEntry1")

add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("rsiThresh2", "upTrend"),
                          cross=TRUE),
           label="longEntry2")

add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("Close", "quickMA"),
                          relationship="gt"),
           label="exitLongNormal")

add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("Close", "filterMA"),
                          relationship="lt"),
           label="exitLongFilter")


# Rules --------------------------------------------------------------------------
# always use "ruleSignal"
# ordertype: can also use limits and stop-limits
# orderside: separates rules into LONG and SHORT
# replace: always use FALSE
# prefer: Quantstrat is a next-bar execution system; use "Open" instead of "Close" to simulate correctly
# osFUN: threshold for trade sizes. This is how you size orders.
# pctATR: 
# maxPctATR: position limit for a specific rule
# type: Entry or Exit position rule
# path.dep: 
# label: naming convention for rule

# Entry Rule #1
add.rule(strategy=strategy.st, name="ruleSignal",
         argument =list(sigcol = "longEntry1",
                        sigval = TRUE,
                        ordertype = "market",
                        orderside = "long",
                        replace = FALSE,
                        prefer = "Open",
                        osFUN = osDollarATR,
                        tradeSize = tradeSize,
                        pctATR = pctATR,        # buy when strategy crosses under 10
                        maxPctATR = pctATR,     # buy again when strategy crosses under 6
                        atrMod="X"),
         type="enter", path.dep = TRUE, label="enterLong1")

# Entry Rule #2
add.rule(strategy=strategy.st, name="ruleSignal",
         argument =list(sigcol = "longEntry2",
                        sigval = TRUE,
                        ordertype = "market",
                        orderside = "long",
                        replace = FALSE,
                        prefer = "Open",
                        osFUN = osDollarATR, 
                        tradeSize = tradeSize,
                        pctATR = pctATR,          
                        maxPctATR = maxPct,      # don't continuously buy when strategy crosses under max
                        atrMod="X"),
         type="enter", path.dep = TRUE, label="enterLong2")

# Exit Rule #1
add.rule(strategy=strategy.st, name="ruleSignal",
         argument =list(sigcol = "exitLongNormal",
                        sigval = TRUE,
                        orderqty = "all",        # don't pyramid-up shares; exit entire position instead
                        ordertype = "market",
                        orderside = "long",
                        replace = FALSE,
                        prefer = "Open"),
         type="enter", path.dep = TRUE, label="normalExitLong")

# Exit Rule #2
# in a position, our price crossed 200-day SMA which indicates steep-drop
# so let's cut our losses and exit the position entirely
add.rule(strategy=strategy.st, name="ruleSignal",
         argument =list(sigcol = "exitLongFilter",
                        sigval = TRUE,
                        ordertype = "market",
                        orderside = "long",
                        replace = FALSE,
                        prefer = "Open"),
         type="enter", path.dep = TRUE, label="filterExitLong")


# Run Strategy --------------------------------------------------------------------------
t1 = Sys.time()

out = applyStrategy(strategy=strategy.st, portfolios=portfolio.st)

t2 = Sys.time()
print(t2-t1)


# Strategy Analytics ----------------------------------------------------------------------
updatePortf(portfolio.st)
dateRange = time(getPortfolio(portfolio.st)$summary)[-1]

updateAcct(portfolio.st, dateRange)
updateEndEq(account.st)

# Chart
chart.Posn(portfolio.st)
# chart.Posn(portfolio.st, symbol = "GDXJ")


# Trade Statistics ----------------------------------------------------------------------
tStats = tradeStats(Portfolios = portfolio.st, use="trades", inclZeroDays=FALSE)
tStats[,4:ncol(tStats)] = round(tStats[,4:ncol(tStats)], 2)
print(data.frame(t(tStats[,-c(1,2)])))

(aggPF = sum(tStats$Gross.Profits) / -sum(tStats$Gross.Losses))
(aggCorrect = mean(tStats$Percent.Positive))
(numTrades = sum(tStats$Num.Trades))
(meanAvgWLR = mean(tStats$Avg.WinLoss.Ratio[tStats$Avg.WinLoss.Ratio < Inf], na.rm=TRUE))


# Daily & Duration Statistics -------------------------------------------------------------------------
# calculates how many days each trade took (maximum winner, minimum loser, etc.)
dStats = dailyStats(Portfolios = portfolio.st, use="Equity")
rownames(dStats) = gsub(".DailyEndEq", "", rownames(dStats))

print(data.frame(t(dStats)))

durStats = durationStatistics(Portfolio = portfolio.st, 
                              Symbols = sort(symbols))
print(t(durStats))

# for portfolios with > 1 instruments:
indivDurStats = durationStatistics(Portfolio = portfolio.st, 
                                   Symbols = sort(symbols), 
                                   aggregate = FALSE)
print(t(indivDurStats))


# Portfolio Cash PnL -------------------------------------------------------------------------
portString = paste0("portfolio.", portfolio.st)
portPL = .blotter[[portString]]$summary$Net.Trading.PL


# Cash Sharpe --------------------------------------------------------------------------------
(SharpeRatio.annualized(portPL, geometric = FALSE))


# Portfolio vs. SPY --------------------------------------------------------------------------
instRets = PortfReturns(account.st)


# Correlations -------------------------------------------------------------------------------
instCors = cor(instRets)
diag(instRets) = NA
corMeans = rowMeans(instCors, na.rm=TRUE)
names(corMeans) = gsub(".DailyEndEq", "", names(corMeans))
print(round(corMeans, 3))
mean(corMeans)


portfRets = xts(rowSums(instRets), 
                order.by = index(instRets))
portfRets = portfRets[!is.na(portfRets)]
cumPortfRets = cumprod(1 + portfRets)
firstNonZeroDay = as.character(index(portfRets)[min(which(portfRets != 0))])

getSymbols("SPY", from = firstNonZeroDay, 
           to = index(portfRets)[length(portfRets)], 
           adjust = TRUE)

SPYrets = diff(log(Cl(SPY)))[-1]
cumSPYrets = cumprod(1 + SPYrets)
comparison = cbind(cumPortfRets, cumSPYrets)
colnames(comparison) = c("Portfolio", "SPY")

chart.TimeSeries(comparison,
                 legend.loc = "topleft",
                 colorset = (2:1), 
                 lwd = 2.0,
                 lty = c(1,1),
                 cex.legend = 1.2,
                 cex.axis = 1.2,
                 cex.main = 2.5,
                 main = "Portfolio Strategy vs SPY")

chart.RelativePerformance(portfRets, SPYrets, 
                          colorset = "darkblue", 
                          lwd = 2,
                          main = "Ratio of Cumulative Returns: Portfolio vs. SPY")

SharpeRatio.annualized(portfRets)
Return.annualized(portfRets)
maxDrawdown(portfRets)

dailyRetComparison = cbind(portfRets, SPYrets)
colnames(dailyRetComparison) = c("Portfolio", "SPY")
round(apply.yearly(dailyRetComparison, Return.cumulative), 3)
round(apply.yearly(dailyRetComparison, SharpeRatio.annualized), 3)
round(apply.yearly(dailyRetComparison,maxDrawdown), 3)



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

