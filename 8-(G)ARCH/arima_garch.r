# ARIMA & GARCH day strategy to S&P500
# Strategy :
#   - for each n day, the previous k(=500) days RETURN data will be used to build optimal ARIMA & GARCH model
#   - combined model will be used to predict next day returns
#   - if tomorrow_return = positive: buy , negative: sell , same: nothing 
# Source : https://www.quantstart.com/articles/ARIMA-GARCH-Trading-Strategy-on-the-SP500-Stock-Market-Index-Using-R/

# import installed libraries
library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)


# get data from yahoo (quantmod)
getSymbols("^GSPC", from="1950-01-01")

# get log return from data
spReturns = diff(log(Cl(GSPC)))

# set the 1st return data (which was NA) to 0
spReturns[as.character(head(index(Cl(GSPC)), 1))] = 0

print("No error so far")