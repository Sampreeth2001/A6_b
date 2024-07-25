install.packages("quantmod")
install.packages("rugarch")
setwd('D:\\R Studio')
data=read.csv('SUZLON.csv')

# Load necessary libraries
library(quantmod)
library(rugarch)

returns <- diff(log(data$Adj.Close)) * 100
returns <- na.omit(returns)

# Plot the returns
plot(returns, main = "Daily Returns of the S&P 500 Index", col = "blue")

# Specify the GARCH(1,1) model
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0)),
                   distribution.model = "norm")
# Fit the model
fit <- ugarchfit(spec = spec, data = returns)

# Print the summary of the fit
print(fit)

# Forecast the next 5 days
forecast <- ugarchforecast(fit, n.ahead = 5)

# Plot the forecasted variance
plot(forecast@forecast$sigmaFor, main = "Forecasted Variance for the Next 5 Days", col = "red", type = "l")
