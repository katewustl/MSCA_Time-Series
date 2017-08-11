#
# Loyalty Coverage
#
# Retailer relaunch loyalty program to encourage participation
# Relaunch date May 18, 2017


# Clear working directory, Initialize libraries, & Set working directory
rm(list=ls())
suppressMessages(library(tseries))
suppressMessages(library(forecast))
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
library(zoo)
setwd("/nfs/analysis/users/k630359/MSCA/31006 Time Series")


# Read in data
raw <- read.csv("Retailer_Loyalty_Txn.csv")
raw$DATE_ID <- dmy(raw$DATE_ID)


date <- raw[,c(1,5,6)]
date <- aggregate(list(date$CARD_TXN, date$ALL_TXN), by=list(date$DATE_ID), FUN=sum)
names(date) <- c('DATE_ID', 'CARD_TXN', 'ALL_TXN')
date <- mutate(date, COVERAGE = CARD_TXN / ALL_TXN)
date <- date[c(1,4)]
head(date)

county <- raw[,c(3,1,5,6)]
county <- aggregate(list(county$CARD_TXN, county$ALL_TXN), by=list(county$COOK_COUNTY,county$DATE_ID), FUN=sum)
names(county) <- c('COOK','DATE_ID', 'CARD_TXN', 'ALL_TXN')
county <- mutate(county, COVERAGE = CARD_TXN / ALL_TXN)
county <- county[c(1,2,5)]
head(county)




#### AGGREGATE LEVEL TIME SERIES
date.ts <- zoo(date$COVERAGE,order.by = date$DATE_ID, frequency = 7)
plot(date.ts)

date.cy <- date[date$DATE_ID >= dmy('01-JAN-2017'),]
cy.ts <- zoo(date.cy$COVERAGE,order.by = date.cy$DATE_ID, frequency = 7)
plot(cy.ts)
acf(coredata(cy.ts))
pacf(coredata(cy.ts))


##---------Linear Regression

##---------Seasonal Auto ARIMA
sarima <- auto.arima(coredata(date.ts), seasonal = TRUE)
summary(sarima)
plot(forecast(sarima))

sarima.cy <- auto.arima(coredata(cy.ts), seasonal = TRUE)
summary(sarima.cy)
plot(forecast(sarima.cy))


##---------VAR? ARFIMA? TBATS? STATE SPACE?


?tbats
x.msts <- msts(date.ts,seasonal.periods=c(7,365.25))
model <- tbats(x.msts)
plot(forecast(model,h=100))







#### COUNTY LEVEL TIME SERIES
county <- county[order(county$COOK),]
cook <- county[county$COOK == 'Y',]
cook.ts <- ts(cook$COVERAGE, frequency = 7)
suburban <- county[county$COOK == 'N',]
suburban.ts <- ts(suburban$COVERAGE, frequency = 7)
ts.plot(cook.ts,suburban.ts, col = c('red','blue'))




































#### STORE LEVEL TIME SERIES.  PROBABLY NOT NECESSARY FOR ANALYSIS, TOO VARIABLE. 
store <- raw[,c(3,4,1,5,6)]
store <- aggregate(list(store$CARD_TXN, store$ALL_TXN), by=list(store$COOK_COUNTY,store$STORE_ID,store$DATE_ID), FUN=sum)
names(store) <- c('COOK','STORE','DATE_ID', 'CARD_TXN', 'ALL_TXN')
store <- mutate(store, COVERAGE = CARD_TXN / ALL_TXN)
store <- store[c(1:3,6)]
head(store)
store <- store[order(store$STORE),]
stores <- unique(store$STORE)

store.ts <- list()
for( i in 1:length(stores)){
  temp <- store[store$STORE == stores[i],]
  store.ts[[i]] <- ts(temp$COVERAGE, frequency = 7)
 }

ts.plot(store.ts[[1]])
ts.plot(store.ts[[25]])
ts.plot(store.ts[[41]])
