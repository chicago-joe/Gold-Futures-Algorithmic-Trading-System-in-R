#
# R script for trend following system for gold and silver prices 
# 
# Author: Joseph Loss, loss2@illinois.edu
# Created on 6/30/2019
#

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
colnames(comparison) = c("strategy", "SPY")

# chart.TimeSeries(comparison, 
#                  legend.loc = "topleft",
#                  colors = c("green", "red"))

chart.RelativePerformance(portfRets, SPYrets)

SharpeRatio.annualized(portfRets)
Return.annualized(portfRets)
maxDrawdown(portfRets)

dailyRetComparison = cbind(portfRets, SPYrets)
colnames(dailyRetComparison) = c("strategy", "SPY")
round(apply.yearly(dailyRetComparison, Return.cumulative), 3)
round(apply.yearly(dailyRetComparison, SharpeRatio.annualized), 3)
round(apply.yearly(dailyRetComparison,maxDrawdown), 3)

