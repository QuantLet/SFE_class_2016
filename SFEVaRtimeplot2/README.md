[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFEVaRtimeplot2** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml


Name of Quantlet : SFEVaRtimeplot2

Published in : Statistics of Financial Markets I

Description : 'Calculates the RMA and EMA of a portfolio of assets'

Keywords : VaR, data visualization, risk, portfolio, RMA, EMA, estimation, exceedance, financial, forecast, normal-distribution, time-series

Author : Marta Domagalska, Xiang Gao, Pegah Maham, David Pollack

Submitted : 2016/12/12

Input: 
- h: Time horizon for RMA and EMA
- alpha: Confidence-level of RMA and EMA
- gamma: decay of EMA

Datafile:
- SFEVaRbank.csv : 'Time-series of the nominal prices of the DAX30, FTSE 100, and an 
assortment of stocks of other large companies.  Originally this file was named 
"2004-2014_dax_ftse". (daily periodicity from 05/04 - 05/14)'

Example: 'Plot of exceedences of RMA and EMA (01/2007 - 01/2009)'


```

![Picture1](EMAoutlierst.png)

![Picture2](RMAoutlierst.png)

### R Code
```r

# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("dplyr", "lubridate", "quantmod")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Inputs
h     = 250
alpha = 0.2
gamma = 0.94

# Load Data
data = read.csv("SFEVaRbank.csv")

# Functions
calcRMA = function(serie, h = 250){
  # find the first non NA position
  s = rle(is.na(serie))$length[1]
  # Calculate the RMA of the serie for with the parameter h
  rma = sapply((s + h + 1): length(serie), FUN = function(n) {
    sum(serie[(n - h): (n - 1)] * serie[(n - h): (n - 1)], na.rm = T) / (h - 1)})
  rma = c(rep(NA, s + h), rma)
  return(rma)
}

emaWeight = function(h, gamma){
  weight = gamma^(h: 1) * (1 - gamma)
  return(weight)
}

calcEMA = function(serie, gamma = 0.94, h = 250){
  s      = rle(is.na(serie))$length[1]
  weight = emaWeight(h, gamma)
  # ema calculation
  ema = sapply((s + h + 1): length(serie), FUN = function(n) {
    sum(serie[(n - h): (n - 1)] * serie[(n - h): (n - 1)] * weight, na.rm = T)})
  ema = c(rep(NA, s + h), ema)
  return(ema)
}

# Colors
pcolors = data.frame(rma = "blue", ema = "red", stringsAsFactors=FALSE)

# Choose the Stocks
# which(colnames(data) == "LLOYDS.BANKING.GROUP") == 34
Stocks = data[year(data$Date) %in% c(2007, 2008, 2009), c(1, 34)]

# Build portfolio of log-returns
Portfolio = data.frame(Date       = as.Date(Stocks$Date), 
                       Price      = (Stocks[, 2]),
                       PercChange = c(NA,diff(log(Stocks[, 2]))),
                       AbsChange  = c(NA,diff(Stocks[, 2])))

# calculate t-value
tCrit = qnorm(1 - alpha) 
# calculate VaR
Portfolio$VaRRMAY = calcRMA(Portfolio$PercChange, h = h) ^ 0.5 * tCrit
Portfolio$VaREMAY = calcEMA(Portfolio$PercChange, gamma = gamma, h = h) ^ 0.5 * tCrit
Portfolio$VaRRMAL = Portfolio$VaRRMAY * Portfolio$Price
Portfolio$VaREMAL = Portfolio$VaREMAY * Portfolio$Price
# calculate exceedences
Portfolio$RugsRMAL = (Portfolio$AbsChange > Portfolio$VaRRMAL | 
                        Portfolio$AbsChange < -Portfolio$VaRRMAL)
Portfolio$RugsEMAL = (Portfolio$AbsChange > Portfolio$VaREMAL | 
                        Portfolio$AbsChange < -Portfolio$VaREMAL)
Portfolio$RugsRMAY = (Portfolio$PercChange > Portfolio$VaRRMAY | 
                        Portfolio$PercChange < -Portfolio$VaRRMAY)
Portfolio$RugsEMAY = (Portfolio$PercChange > Portfolio$VaREMAY | 
                        Portfolio$PercChange < -Portfolio$VaREMAY)
# remove rows without VaR
Portfolio = Portfolio[- c(1 :h), ]


# Plot RMA outliers over time
plot(Portfolio$Date, 
	Portfolio$AbsChange, 
	col  = "white",
	pch  = 16, 
	cex  = 0.3, 
	ylim = c(0, 1), 
	ylab = expression({Z}[t]), 
	xlab = "Year", 
	main = "Time plot for exceedances for RMA at 80% significance level",
	yaxt = "n")
# Add Rugs, for Rma at the bottom, for Ema at the top
rug(Portfolio$Date[Portfolio$RugsRMAL], side = 3, col = pcolors$rma, lwd = 2)
rug(Portfolio$Date[!Portfolio$RugsRMAL], side = 1, col = "black", lwd = 2)

dev.print(device = png, filename = 'RMAoutlierst.png', width = 1200, height = 600)



# Plot RMA outliers over time
plot(Portfolio$Date, 
	Portfolio$AbsChange, 
	col  = "white", 
	pch  = 16, 
	cex  = 0.3, 
	ylim = c(0, 1), 
	ylab = expression({Z}[t]), 
	xlab = "Year", 
	main = "Time plot for exceedances for EMA at 80% significance level",
	yaxt = "n")
# Add Rugs, for Rma at the bottom, for Ema at the top
rug(Portfolio$Date[Portfolio$RugsEMAL], side = 3, col = pcolors$ema, lwd = 2)
rug(Portfolio$Date[!Portfolio$RugsEMAL], side = 1, col = "black", lwd = 2)

dev.print(device = png, filename = 'EMAoutlierst.png', width = 1200, height = 600)



```

automatically created on 2018-05-28