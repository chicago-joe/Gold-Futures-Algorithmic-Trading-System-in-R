# miscellanous code

# OPTIONAL Data Import ----------------------------------------------------
# GOLD / SILVER import from Quandl
metals.tickers = c("LBMA/GOLD", "LBMA/SILVER")
metals.colnames = c("GOLD","SILVER")
metals.raw = Quandl(metals.tickers, type="xts")[,c(1,7)]
colnames(metals.raw) = metals.colnames

metals.raw <- na.locf(metals.raw, fromLast=TRUE) # Last Observation Carried Forwawrd

# filter on 1968 - 2019:
metals.data<-subset(metals.raw,
                    + index(metals.raw) >= "1968-01-02" &
                        + index(metals.raw) <= "2019-07-01")

# create returns data and omit NA:
metals.returns.tmp <- diff(log(metals.data))
metals.returns <- na.omit(metals.returns.tmp)

# pull ETF tickers from Yahoo:
etf.tickers = c("GDX","GDXJ","SPY","ZJG.TO")
tmp <- suppressMessages(getSymbols(etf.tickers[1], src="yahoo", env=NULL))
adj.col <- last(colnames(tmp))
etf.raw <- tmp[,adj.col]

for (j in 2:length(etf.tickers)) {
    tmp <- suppressMessages(getSymbols(etf.tickers[j], src="yahoo", env=NULL))
    adj.col <- last(colnames(tmp))
    etf.raw <- cbind(etf.raw, tmp[,adj.col])
}

etf.raw <- na.locf(etf.raw, fromLast=TRUE) # LOCF

# ZJG.TO has no data until 2009-11-11, so start in 2010:
etf.data<-subset(etf.raw, 
                 + index(etf.raw) >= "2010-01-02" &
                     + index(etf.raw) <= "2019-07-01")

# create returns data and omit NA:
etf.returns.tmp <- diff(log(etf.data))
etf.returns <- na.omit(etf.returns.tmp)




# custom SMA function (not required) -------------------------------------------------------
# custom SMA function if you want:
# sma = SMA(x = Cl(SPY), n = 200)
# SMA <- function(x, n = 10, ...)
# {
#      ma <- runMean(x, n)
#      if (!is.null(dim(ma))) {
#          colnames(ma) <- "SMA"
#      }
#      return(ma)
# }


# Market Exposure ---------------------------------------------------------
# tmp = list()
# length(tmp) = length(symbols)
# for(i in 1:nrow(dStats))
# {
#     totalDays = nrow(get(rownames(dStats)[i]))
#     mktExposure = dStats$Total.Days[i]/totalDays
#     tmp[[i]] = c(rownames(dStats)[i], round(mktExposure, 3))
# }
# 
# mktExposure = data.frame(do.call(rbind,tmp))
# colnames(mktExposure) = c("Symbol","MktExposure")
# 
# print(mktExposure)
# print(mean(as.numeric(as.array(mktExposure$MktExposure))))

