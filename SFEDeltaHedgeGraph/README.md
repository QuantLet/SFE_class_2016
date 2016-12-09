
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="884" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFEDeltaHedgeGraph** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet : SFEDeltaHedgeGraph

Published in : Statistics of Financial Markets I

Description : 'Generates and plots a graphical describtion of the logic behind delta hedging and
calulates corresponding heding costs for given parameters'

Keywords : delta, hedging, asset, black-scholes, call, simulation, cost, delta, strategy

See also : 'SFESLDHPerf, SFESLDHConv, SFEStopLossLogic, SFEDeltaHedgeGraph, SFEStopLossThGraph,
SFSstoploss, SFEDeltaHedging, SFEDeltahedgingLogic, SFEDeltahedgingdepend'

Author : Simon Gstöhl, Florian Schulz

Submitted : 2016/12/05

Input: 
- S0: Stock price at t = 0
- sig: Volatility
- r: Risk free interest rate
- K: Strike price
- t0: Starting time (1 week = 1/52)
- mat: Maturity.

Output : A plot of the logic of delta hedging and assosiated costs with given parameters.

```

![Picture1](SFEDeltaHedgeGraph.png)


### R Code:
```r

# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Declare stock price variables
S0    = 49                  # current stock price
sig   = 0.2                 # volatility 

# Declare option pricing variables
r     = 0.05                # interest rate
K     = 50                  # strike price
t0    = 0/52                # current time (1 week = 1/52)
mat   = 20/52               # maturity
tau   = 10/52               # time to maturity

# Black-Scholes option price
X     = seq(40, 60, 0.5)    # range for the stock price
y     = (log(X/K) + (r - (sig^2)/2) * tau)/(sig * sqrt(tau))
C     = X * pnorm(y + sig * sqrt(tau)) - exp(-r * tau) * K * pnorm(y)

# Delta at point X[x]
x     = round(length(X)/2 + 2)
Delta = pnorm(y[x] + sig * sqrt(tau))

# Plot
plot(X, C, type = "l", xlab = "StockPrice", ylab = "OptionPrice", xaxt = "n", yaxt = "n",
     main = "Delta hedge strategy")
intercept = C[x] - X[x] * Delta
lines(X[(x - 12):(x + 12)], Delta * X[(x - 12):(x + 12)] + intercept, col = "red")
lines(X[1:x], rep(C[x], x), lty = 2)
lines(rep(X[x], x), C[1:x], lty = 2)
axis(1, X[x], "S", las = 1)
axis(2, C[x], "C(S)", las = 1)
text(X[x + 16], C[x + 4], expression(Delta), col = "red")
text(X[x + 11], C[x + 4], "slope = 0.6 =", col = "red", cex = 0.8)

DeltaStrat = data.frame(S = X[x], Delta = Delta, HedgeCosts = Delta * X[x])
print(DeltaStrat)

```
