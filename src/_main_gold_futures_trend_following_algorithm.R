# GOLD-TrendFollow-or-BuyHold.r
# R script for trend following system for gold and silver prices 
# 
# Author: Joseph Loss, loss2@illinois.edu
# Created on 6/30/2019
#

# LIBRARY REQUIREMENTS ---------------------------------------------------
require(quantstrat)
require(devtools)
require(DSTrading)      # additional trading functionality
require(IKTrading)      # allows Digital-Signal trading
require(TTR)
require(xts)
require(PerformanceAnalytics)
require(Quandl)
require(quantmod)
require(qmao)           # Quantmod add-on

# if you want premium data you must enter a Premium API key:
Quandl.api_key(MY_QUANDL_API_KEY)      


# pull gold continuous futures from Quandl 
goldFutures <- quandClean(stemCode = "CHRIS/CME_GC",
                          start_date = "2000-01-02",
                          end_date = "2019-07-01",
                          verbose = TRUE)
ticker <-"CHRIS/CME_GC"
CrossCurrency <- paste(substr(ticker, 1,12), substr(ticker,-1,-12), sep="")
assign(ticker,goldFutures)


# Configuration Metadata ----------------------------------------------------------
options(width=70)
options("getSymbols.warning4.0" = FALSE)

# Charting Parameters
myPars <- chart_pars()
myPars$mar <- c(3, 2, 0, .2) # default is c(3, 1, 0, 1)  # bottom, left, top, right
myPars$cex <- 1.5 #' Increase font size of both x and y axis scale ticks
mychartTheme <- chart_theme()
mychartTheme$rylab = FALSE  #' Don't show y-axis on right side of plot to save space


# NOTE: Stock blotter must be reset before every portfolio run
rm(list = ls(.blotter), envir = .blotter)   # reset stock blotter
currency('USD')                             # set portfolio currency
Sys.setenv(TZ = "UTC")                      # set timezone


symbols = c("CHRIS/CME_GC")        # choose which symbol or symbols to use in strategy

# future(symbols, currency = "USD", multiplier = 1)
stock(symbols, currency = "USD", multiplier = 1)


initDate = "2000-01-01"         # must have initDate BEFORE first day of price data
tradeSize <- 50000                    # trade sizing
initEq <- tradeSize*length(symbols)   # initial equity size


## Create strategy, portfolio, and account (blotter) with name "SMAC-4m-12m"
strategy.st <- portfolio.st <- account.st <- "SMAC-4m-12m"

rm.strat(portfolio.st)  # remove portfolio after each backtest or quantstrat will fail
rm.strat(strategy.st)   # remove strategy after each backtest or quantstrat will fail



# Strategy Initialization -------------------------------------------------------------
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

out = applyStrategy(strategy=strategy.st, portfolios=portfolio.st, mktdata = `CHRIS/CME_GC`)

t2 = Sys.time()
print(t2-t1)


# Strategy Analytics ----------------------------------------------------------------------
updatePortf(portfolio.st)
dateRange = time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st, dateRange)
updateEndEq(account.st)

# get trading data for future use...
book    = getOrderBook(portfolio.st)
stats   = tradeStats(portfolio.st, use = "trades", inclZeroDays = TRUE)
ptstats = perTradeStats(portfolio.st)
txns    = getTxns(portfolio.st, symbols)

chart.Posn(portfolio.st, pars=myPars, theme=mychartTheme)      # Position and Cum. PnL Chart


# Trade Statistics ----------------------------------------------------------------------
tStats = tradeStats(Portfolios = portfolio.st, use="trades", inclZeroDays=TRUE)
tStats[,4:ncol(tStats)] = round(tStats[,4:ncol(tStats)], 2)
print(data.frame(t(tStats[,-c(1,2)])))

(aggPF = sum(tStats$Gross.Profits) / -sum(tStats$Gross.Losses))
(aggCorrect = mean(tStats$Percent.Positive))
(numTrades = sum(tStats$Num.Trades))
(meanAvgWLR = mean(tStats$Avg.WinLoss.Ratio[tStats$Avg.WinLoss.Ratio < Inf], na.rm=TRUE))

Return.cumulative(daily.returns)*100

sd.annualized(daily.returns)
# ga = getAccount(account.st)$summary
# gp = getPortfolio(portfolio.st)$summary["2019"] 


# Annualized Returns Summary ------------------------------------------------------
equity.curve <- getAccount(account.st)$summary$End.Eq
daily.returns <- Return.calculate(equity.curve$End.Eq, "discrete")
names(daily.returns) <- "return"

# get annualized summary
table.AnnualizedReturns(daily.returns, scale = 260.85) # adjusted for weekdays 
# per year of ~ 260.85

# chart performance
charts.PerformanceSummary(daily.returns, main = "Strategy Performance Summary ")

# get some summary trade statistics
stats[,c("Symbol", "Num.Trades", "Percent.Positive", "Net.Trading.PL",
         "Profit.Factor", "Max.Drawdown")] 

# get table of monthly returns
monthly.returns <-  Return.calculate(to.monthly(equity.curve)[, 4], "discrete")
names(monthly.returns) <- "Total"
table.CalendarReturns(monthly.returns)


# Daily & Duration Trade Statistics ------------------------------------------------------
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

# pull Gold continuous futures from CME for GOLD Index
GOLD.futuresindex <- quandClean(stemCode = "CHRIS/CME_GC",
                                start_date = "2000-01-02",
                                end_date = "2019-07-01",
                                verbose = TRUE)


GOLD.futuresindex.rets = diff(log(Cl(GOLD.futuresindex)))[-1]
cumGOLDrets = cumprod(1 + GOLD.futuresindex.rets)
comparison = cbind(cumPortfRets, cumGOLDrets)
colnames(comparison) = c("GOLD Trend Strategy", "GOLD B/H")


# GOLD Trend Following vs Buy/Hold ----------------------------------------
chart.TimeSeries(comparison,
                 legend.loc = "topleft",
                 colorset = (2:1), 
                 lwd = 2.0,
                 lty = c(1,1),
                 cex.legend = 1.2,
                 cex.axis = 1.2,
                 cex.main = 2.5,
                 main = "GOLD Trend Strategy vs GOLD B/H")

chart.RelativePerformance(portfRets, GOLD.futuresindex.rets, 
                          colorset = "darkblue", 
                          lwd = 2,
                          main = "Ratio of Cumulative Returns: GOLD Trend Strategy vs GOLD B/H")

SharpeRatio.annualized(portfRets)
Return.annualized(portfRets)
maxDrawdown(portfRets)

dailyRetComparison = cbind(portfRets, GOLD.futuresindex.rets)
colnames(dailyRetComparison) = c("GOLD Trend Strategy", "GOLD B/H")
round(apply.yearly(dailyRetComparison, Return.cumulative), 3)
round(apply.yearly(dailyRetComparison, SharpeRatio.annualized), 3)
round(apply.yearly(dailyRetComparison,maxDrawdown), 3)

