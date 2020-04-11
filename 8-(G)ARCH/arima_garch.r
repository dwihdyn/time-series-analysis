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

# create empty vector to store 'forecast' value
windowLength = 500   # k
foreLength = length(spReturns) - windowLength
forecasts = vector(mode="character", length=foreLength)         # create vector filled with empty CHARACTER, with length of foreLength


# === Steps by step after this ===
# fit the first 500 data to create first ARIMA() 
# then loop thru every day (d) after the first 500 data to roll forward 



# fit training data into ARIMA model to find best (p,0,q) | differencing = 0 because we are using log-return data NOT pure price anymore
for (d in 0:foreLength) {

    # Obtain S&P500 rolling windows for this day 
    spReturnsOffset = spReturns[(1+d) : (windowLength + d)]

    # === Fit data to ARIMA ===

    # init variables
    final.aic = Inf     # start with infinity AIC & work way down to converge to smallest & best AIC
    final.order = c(0,0,0)
    
    # find best p q for ARIMA(p,0,q)
    for (p in 0:5) for (q in 0:5){

        # skip p=q=0
        if (p==0 && q==0){
            next
        }

        # finding best p q with d = 0
        arimaFit = tryCatch(
            arima(spReturnsOffset, order=c(p,0,q)),
            error = function(err) FALSE,
            warning = function(err) FALSE
        )

        # getting best ARIMA by ensuring with the current loop p & q, it has the least AIC value
        if(!is.logical(arimaFit)) {
            current.aic = AIC(arimaFit)
            
            # if current model has lesser AIC than the past model, replace this model with this
            if (current.aic < final.aic) {
                final.aic = current.aic
                final.order = c(p,0,q)    # save this model
                final.arima = arima(spReturnsOffset, order=final.order)
            }
        } else {
            next
        }
    }

    # Fit best ARIMA(p,0,q) to GARCH(1,1) model
    spec = ugarchspec(
        variance.model = list(garchOrder=c(1,1)),  # GARCH(1,1)
        mean.model = list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),  # ARIMA(p,0,q)
        distribution.model = "sged"
    )

    fit = tryCatch(
        ugarchsfit(
            spec, spReturnsOffset, solver = 'hybrid'
        ), error = function(e) e, warning = function(W) W
    )

    # if GARCH model doesnt converge, set direction to "long" else
    # choose the correct forecast direction based on returns prediction
    # Output the result to the screen & forecasts vector
    if (is(fit, "warning")) {
        forecasts[d+1] = paste(index(spReturnsOffset[windowLength]), 1, sep=",")
        print(paste(index(spReturnsOffset[windowLength]), 1, sep="," ))
    }

}










# == Backtest ==
# Now ARIMA & GARCH model done build on S&P500, get data from SPDR

# create new empty CSV file
write.csv(forecasts, file="forecasts.csv", row.names=FALSE)

# input python-refined CSV file
spArimaGarch = as.xts(
    read.zoo(
        file="forecasts_new.csv", format="%Y-%m-%d", header=F, sep=","
    )
)

# create ARIMA+GARCH return 
spIntersect = merge(spArimaGarch[,1]. spReturns, all=F)
spArimaGarchReturns = spIntersect[,1] * spIntersect[,2]

# create backtest ARIMA+GARCH vs Buy&Hold
spArimaGarchCurve = log(cumprod(1+spArimaGarchReturns))



print("----------------------------------")
print("No error so far")