# Load libraries 
library(tidyverse)
library(changepoint)

# Load image from Script 1
load("image/AggregatedData.RData")

# Check data
head(fileResults[[1]])

# Convert to ts
# Bitcoin
bitcoin_open_ts <- ts(fileResults[[1]]$Open)
bitcoin_high_ts <- ts(fileResults[[1]]$High)
bitcoin_low_ts <- ts(fileResults[[1]]$Low)
bitcoin_close_ts <- ts(fileResults[[1]]$Close)

# Dodgecoin
dodgecoin_open_ts <- ts(fileResults[[2]]$Open)
dodgecoin_high_ts <- ts(fileResults[[2]]$High)
dodgecoin_low_ts <- ts(fileResults[[2]]$Low)
dodgecoin_close_ts <- ts(fileResults[[2]]$Close)

# Ethereum
ethereum_open_ts <- ts(fileResults[[3]]$Open)
ethereum_high_ts <- ts(fileResults[[3]]$High)
ethereum_low_ts <- ts(fileResults[[3]]$Low)
ethereum_close_ts <- ts(fileResults[[3]]$Close)

# Determine penalty param
cptfn <- function(data, pen) {
  ans <- cpt.mean(data, test.stat="Normal", method = "PELT", penalty = "Manual", pen.value = pen) 
  length(cpts(ans)) +1
}

# evaluate and plot results:
plot.new()
frame()
nf <- layout(matrix(c(1,2),ncol=1), widths=c(4,4), heights=c(2,2), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)
#layout.show(nf)

# Bitcoin ----
# P1 - run cptfn for the signal with a known change point ----
pen.vals <- seq(1000000000, 15000000000, 1000000000)

elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = bitcoin_open_ts, pen = p)))

plot.ts(bitcoin_open_ts,type='l',col='red',
        xlab = "Time",
        ylab = "Opening Price (USD)",
        main = "Bitcoin Opening Price - Time Series Plot")
plot(pen.vals,elbowplotData,
     xlab = "PELT penalty parameter",
     ylab = "Changepoint Location",
     main = "Bitcoin Opening Prices - Change in mean signal")


# P2 - run cptfn for the signal with a known change point ----
pen.vals <- seq(1000000000, 15000000000, 1000000000)

elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = bitcoin_high_ts, pen = p)))

plot.ts(bitcoin_high_ts,type='l',col='red',
        xlab = "Time",
        ylab = "Highest 24-hour Price (USD)",
        main = "Bitcoin Highest Price - Time Series Plot")
plot(pen.vals,elbowplotData,
     xlab = "PELT penalty parameter",
     ylab = "Changepoint Location",
     main = "Bitcoin Highest Price - Change in mean signal")

# P3 - run cptfn for the signal with a known change point ----
pen.vals <- seq(1000000000, 15000000000, 1000000000)

elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = bitcoin_low_ts, pen = p)))

plot.ts(bitcoin_low_ts,type='l',col='red',
        xlab = "Time",
        ylab = "Lowest last 24-hour Price (USD)",
        main = "Bitcoin Lowest Price - Time Series Plot")
plot(pen.vals,elbowplotData,
     xlab = "PELT penalty parameter",
     ylab = "Changepoint Location",
     main = "Bitcoin Lowest Price - Change in mean signal")

# P4 - run cptfn for the signal with a known change point ----
pen.vals <- seq(1000000000, 15000000000, 1000000000)

elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = bitcoin_close_ts, pen = p)))

plot.ts(bitcoin_close_ts,type='l',col='red',
        xlab = "Time",
        ylab = "Closing Price (USD)",
        main = "Bitcoin Closing Price - Time Series Plot")
plot(pen.vals,elbowplotData,
     xlab = "PELT penalty parameter",
     ylab = "Changepoint Location",
     main = "Bitcoin Closing Price - Change in mean signal")

# Dogecoin ----
# P1 - run cptfn for the signal with a known change point ----
pen.vals <- seq(0, 10, 0.65)

elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = dodgecoin_open_ts, pen = p)))

plot.ts(dodgecoin_open_ts,type='l',col='red',
        xlab = "Time",
        ylab = "Opening Price (USD)",
        main = "Dodgecoin Opening Price - Time Series Plot")
plot(pen.vals,elbowplotData,
     xlab = "PELT penalty parameter",
     ylab = "Changepoint Location",
     main = "Dodgecoin Opening Price - Change in mean signal")


# P2 - run cptfn for the signal with a known change point ----
elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = dodgecoin_high_ts, pen = p)))

plot.ts(dodgecoin_high_ts,type='l',col='red',
        xlab = "Time",
        ylab = "Highest 24-hour Price (USD)",
        main = "Dodgecoin Highest Price - Time Series Plot")
plot(pen.vals,elbowplotData,
     xlab = "PELT penalty parameter",
     ylab = "Changepoint Location",
     main = "Dodgecoin Highest Price - Change in mean signal")

# P3 - run cptfn for the signal with a known change point ----
elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = dodgecoin_low_ts, pen = p)))

plot.ts(dodgecoin_low_ts,type='l',col='red',
        xlab = "Time",
        ylab = "Lowest 24-hour Price (USD)",
        main = "Dodgecoin Lowest Price - Time Series Plot")
plot(pen.vals,elbowplotData,
     xlab = "PELT penalty parameter",
     ylab = "Changepoint Location",
     main = "Dodgecoin Lowest Price - Change in mean signal")

# P4 - run cptfn for the signal with a known change point ----
elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = dodgecoin_close_ts, pen = p)))

plot.ts(dodgecoin_close_ts,type='l',col='red',
        xlab = "Time",
        ylab = "Closing Price (USD)",
        main = "Dodgecoin Closing Price - Time Series Plot")
plot(pen.vals,elbowplotData,
     xlab = "PELT penalty parameter",
     ylab = "Changepoint Location",
     main = "Dodgecoin Closing Price - Change in mean signal")

# Ethereum ----
# P1 - run cptfn for the signal with a known change point ----
pen.vals <- seq(100000000, 300000000, 10000000)

elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = ethereum_open_ts, pen = p)))

plot.ts(ethereum_open_ts,type='l',col='red',
        xlab = "Time",
        ylab = "Opening Price (USD)",
        main = "Ethereum Opening Price - Time Series Plot")
plot(pen.vals,elbowplotData,
     xlab = "PELT penalty parameter",
     ylab = "Changepoint Location",
     main = "Ethereum Opening Price - Change in mean signal")


# P2 - run cptfn for the signal with a known change point ----
elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = ethereum_high_ts, pen = p)))

plot.ts(ethereum_high_ts,type='l',col='red',
        xlab = "Time",
        ylab = "Highest 24-hour Price (USD)",
        main = "Ethereum Highest Price - Time Series Plot")
plot(pen.vals,elbowplotData,
     xlab = "PELT penalty parameter",
     ylab = "Changepoint Location",
     main = "Ethereum Highest Price - Change in mean signal")

# P3 - run cptfn for the signal with a known change point ----
elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = ethereum_low_ts, pen = p)))

plot.ts(ethereum_low_ts,type='l',col='red',
        xlab = "Time",
        ylab = "Lowest 24-hour Price (USD)",
        main = "Ethereum Lowest Price - Time Series Plot")
plot(pen.vals,elbowplotData,
     xlab = "PELT penalty parameter",
     ylab = "Changepoint Location",
     main = "Ethereum Lowest Price - Change in mean signal")

# P4 - run cptfn for the signal with a known change point ----
elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = ethereum_close_ts, pen = p)))

plot.ts(ethereum_close_ts,type='l',col='red',
        xlab = "Time",
        ylab = "Closing Price (USD)",
        main = "Ethereum Closing Price - Time Series Plot")
plot(pen.vals,elbowplotData,
     xlab = "PELT penalty parameter",
     ylab = "Changepoint Location",
     main = "Ethereum Closing Price - Change in mean signal")

# Show change in ts ----
penalty.val <- 150000000
cptm_CP <- cpt.mean(ethereum_close_ts, penalty='Manual',pen.value=penalty.val,method='PELT') 
cptm_CP
# cpts_CP <- cpts(cptm_CP) # change point time points
# cpts_CP

par(cex.axis=0.8,cex.lab=0.9,cex=0.8)
plot(cptm_CP,
     xlab = "Time",
     ylab = "Closing Price (USD)",
     main = "Changepoints in Closing Price (Ethereum)")

