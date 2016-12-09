
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="884" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFEStopLossLogic** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet : SFEStopLossLogic

Published in : Statistics of Financial Markets I

Description : 'Generates and plots the path of two stocks and marks the corresponding buying
and selling time points of a stop-loss hedging strategy'

Keywords : stop-loss, hedging, asset, black-scholes, call, simulation, cost, delta, strategy

See also : SFESLDHPerf, SFESLDHConv, SFEDeltaHedgeGraph, SFSstoploss

Author : Simon Gstöhl, Florian Schulz

Submitted : 2016/12/05

Input: 
- S0: Stock price at t = 0
- sig: Volatility
- r: Risk free interest rate
- K: Strike price
- t0: Starting time (1 week = 1/52)
- mat: Maturity
- dt: Time period between steps.

Output : 'A plot of two simulated stocks with buy and sell times within the stop-loss hedging strategy
and two tables of the corresponding hedging costs.'

```

![Picture1](StopLossLogic.png)


### R Code:
```r

# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Declare stock price variables
S0    = 49                        # current stock price
sig   = 0.2                       # volatility 

# Declare option pricing variables
r     = 0.05                      # interest rate
K     = 50                        # strike price
t0    = 0/52                      # current time (1 week = 1/52)
mat   = 20/52                     # maturity

# Parameters for calculations
dt    = 0.2/52                    # period between steps
np    = (mat - t0)/dt             # number of periods
t     = seq(t0, mat, dt)          # maturity - t0 divided in n intervals
tau   = mat - t                   # times to maturity

# Simulate first stock price path
set.seed(288)                     # in order to make the simulation replicable
Wt  = c(0, cumsum(sqrt(dt) * rnorm(np)))
S   = S0 * exp((r - 0.5 * sig^2) * t + sig * Wt)

# Calculate buying and selling timepoints
Buy         = (S[1:np] < K) * (S[2:(np + 1)] > K)
Sell        = (S[1:np] > K) * (S[2:(np + 1)] < K)
hedge.costs = numeric(sum(Buy) + sum(Sell) + 1)
buytime     = numeric(sum(Buy))   # predefined zero vector for efficiency
selltime    = numeric(sum(Sell))  # length of buy/sell time is the number of purchases/sales

# Define counting variables for calculations
n   = 1
m   = 1
p   = 1

for (k in 1:length(Buy)) {
  if (Buy[k] == 1) {
    buytime[n] = t[k + 1]
    hedge.costs[p] = S[k + 1]
    n = n + 1
    p = p + 1
  }
  if (Sell[k] == 1) {
    selltime[m] = t[k + 1]
    hedge.costs[p] = -S[k + 1]
    m = m + 1
    p = p + 1
  }
}

hedge.costs[length(hedge.costs)] = -K * (S[np + 1] > K)

# Plot
par(mfrow = c(2, 1))
plot(t, S, type = "l", lty = 5, ylab = "StockPrice", xlab = "Time", xaxt = "n", yaxt = "n",
     main = "Stop-loss strategy - path 1")
lines(t, S, type = "p", pch = 19)
abline(h = 50, lty = 5)
axis(2, K, "K", las = 1)
buyaxis = rep("buy", length(buytime))
sellaxis = rep("sell", length(selltime))
axis(1, buytime, buyaxis)
axis(1, selltime, sellaxis)
abline(v = buytime, lty = 2, lwd = 2, col = "blue")
abline(v = selltime, lty = 2, lwd = 2, col = "red")
if (S[length(S)]>K){
  axis(1, mat, "T: sell")
  abline(v = mat, lty = 2, lwd = 2, col = "red")
}

StopLossStrat1 = data.frame(HedgeCosts = hedge.costs, CumHedgeCosts = cumsum(hedge.costs),
                            row.names = c(1:(length(hedge.costs) - 1), "Maturity"))


# Simulate second stock price path
set.seed(924)                     # in order to make the simulation replicable
Wt    = c(0, cumsum(sqrt(dt) * rnorm(np)))
S     = S0 * exp((r - 0.5 * sig^2) * t + sig * Wt)

# Calculate buying and selling timepoints
Buy         = (S[1:np] < K) * (S[2:(np + 1)] > K)
Sell        = (S[1:np] > K) * (S[2:(np + 1)] < K)
hedge.costs = numeric(sum(Buy) + sum(Sell) + 1)
buytime     = numeric(sum(Buy))   # predefined zero vector for efficiency
selltime    = numeric(sum(Sell))  # length of buy/sell time is the number of purchases/sales

# Define counting variables for calculations
n   = 1
m   = 1
p   = 1

for (k in 1:length(Buy)) {
  if (Buy[k] == 1) {
    buytime[n] = t[k + 1]
    hedge.costs[p] = S[k + 1]
    n = n + 1
    p = p + 1
  }
  if (Sell[k] == 1) {
    selltime[m] = t[k + 1]
    hedge.costs[p] = -S[k + 1]
    m = m + 1
    p = p + 1
  }
}

hedge.costs[length(hedge.costs)] = -K * (S[np + 1] > K)

# Plot
plot(t, S, type = "l", lty = 5, ylab = "StockPrice", xlab = "Time", xaxt = "n", yaxt = "n",
     main = "Stop-loss strategy - path 2")
lines(t, S, type = "p", pch = 19)
abline(h = 50, lty = 5)
axis(2, K, "K", las = 1)
buyaxis = rep("buy", length(buytime))
sellaxis = rep("sell", length(selltime))
axis(1, buytime, buyaxis)
axis(1, selltime, sellaxis)
abline(v = buytime, lty = 2, lwd = 2, col = "blue")
abline(v = selltime, lty = 2, lwd = 2, col = "red")
if (S[length(S)]>K){
  axis(1, mat, "T: sell")
  abline(v = mat, lty = 2, lwd = 2, col = "red")
}

StopLossStrat2 = data.frame(HedgeCosts = hedge.costs, CumHedgeCosts = cumsum(hedge.costs),
                            row.names = c(1:(length(hedge.costs) - 1), "Maturity"))

print(StopLossStrat1)
print(StopLossStrat2)

```

