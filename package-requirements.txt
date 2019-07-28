# PACKAGE REQUIREMENTS
#
# for debugging and/or setup help, visit the Quantstrat tutorial:
# https://www.youtube.com/watch?v=61_F2fcvrsw
#

install.packages("devtools") # if not installed
install.packages("FinancialInstrument") #if not installed
install.packages("PerformanceAnalytics") #if not installed


devtools::install_github("braverock/blotter")   # next install blotter from GitHub
devtools::install_github("braverock/quantstrat") # next install quantstrat from GitHub
install.packages("zoo")
install.packages("TTR", repos="http://R-Forge.R-project.org")
install.packages("Defaults")

install_github(repo="IKTrading", username="IlyaKipnis")
install_github(repo="DSTrading", username="IlyaKipnis")
require(DSTrading)      # additional trading functionality
require(IKTrading)      # allows Digital-Signal trading
require(TTR)
require(xts)
require(PerformanceAnalytics)

install.packages("Quandl")
install.packages("quantmod")
require(Quandl)
require(quantmod)
install.packages("qmao", repos="http://R-Forge.R-project.org")
require(qmao)           # Quantmod add-on
