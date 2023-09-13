###################################################### ################################
############################ HOMEWORK N.1 - CRM CASE ############### #############
###################################################### ################################

################################ Loading libraries ################# ###############
library("quantmod")
library("astsa")
libraries("readxl")
library("tidyverse")
library("fImport")
libraries("fBasics")
library("rugarch")
library("highfrequency")
library("xts")
library("zoo")
library("chron")
library("forecast")

################################## Technical analytes ###############################
load("CRM1secprice.RData")


## Daily prices
matrix.1 = secprice[seq(23400, nrow(secprice), by = 23400), ]
price_1 = vec(matrix.1)


################################## ROC Indicator #################################
h = 5

# Indicator for daily prices

n = 1259

roc = 100*(price_1[6:n]-price_1[(6-h):(n-h)])/(price_1[(6-h):(n-h)])
par(mfrow=c(2,1))
tsplot(roc[500:700])
tsplot(price_1[500:700])
par(mfrow=c(1,1))

# Indicator for one-second prices

n = length(secdayprice)
h = 5*23400
t = 6*23400

roc = 100*(secdayprice[t:n]-secdayprice[(t-h):(n-h)])/(secdayprice[(t-h):(n-h)])


{############# Insert the dates to the price series that I consider ##############

#install.packages("quantmod")
library(quantmod)

# I define the symbol of a publicly traded company
symbol <- "CRM"

# I get the historical stock price data of the publicly traded company
getSymbols(symbol, from = "2018-01-02", to = "2022-12-31")

# I extract the closing price data of the shares
prices <- Ad(get(symbol))

# I extract a vector of dates from the stock closing price data
date_vec <- index(prices)
}


## Graphical representations of daily data

baseline = rep(0.1259)
baseline_d=xts(x=baseline,order.by=date_vec)
ma_price = ma(price_1[200:555], order = 22)
ma_price_d=xts(x=ma_price,order.by=date_vec[200:555])

roc_d=xts(x=roc,order.by=date_vec[6:1259])
price_1_d=xts(x=price_1,order.by=date_vec)
par(mfrow=c(2,1))
plot(roc_d[200:550], main = "ROC curve")
lines(baseline_d, col = "red")
plot(price_1_d[205:555], main = "Price curve")
lines(ma_price_d, col = "purple", type = "l")
par(mfrow=c(1,1))

plot(price_1_d[205:555], main = "Price curve")
lines(ma_price_d, col = "purple", type = "l")


## Graphical representations of data at 1 second

roc_set = roc[4680000:12870000]
price = secdayprice[4680000:12870000]
date_old_rep = sort(rep(date_old,23400))
date_vec_rep = date_vec_rep[4680000:12870000]
ma_price = ma(price, order = 514800)
ma_price_d=xts(x=ma_price,order.by=date_vec_rep)

baseline = rep(0.8190001)
baseline_d=xts(x=baseline,order.by=date_vec_rep)

roc_set_d=xts(x=roc_set,order.by=date_vec_rep)
price_d=xts(x=price,order.by=date_vec_rep)
par(mfrow=c(2,1))
plot(roc_set_d, main = "ROC curve")
lines(baseline_d, col = "red")
plot(price_d, main = "Price curve")
lines(ma_price_d, col = "purple", type = "l")
par(mfrow=c(1,1))

plot(price_1_d[205:555], main = "Price curve")
lines(ma_price_d, col = "purple", type = "l")
