# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("dplyr", "lubridate", "quantmod")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


# Inputs
h     = 250
alpha = 0.01
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
  s = rle(is.na(serie))$length[1]
  weight = emaWeight(h, gamma)
  ema = sapply((s + h + 1): length(serie), FUN = function(n) {
    sum(serie[(n - h): (n - 1)] * serie[(n - h): (n - 1)] * weight, na.rm = T)})
  ema = c(rep(NA, s + h), ema)
  return(ema)
}

# Choose the Stocks
# which(colnames(data) == "LLOYDS.BANKING.GROUP") == 34
Stocks = data[year(data$Date) %in% c(2007, 2008, 2009), c(1,34)]

# Build portfolio of log-returns
Portfolio = data.frame(Date       = as.Date(Stocks$Date), 
                       Price      = (Stocks[,2]),
                       PercChange = c(NA,diff(log(Stocks[,2]))),
                       AbsChange  = c(NA,diff(Stocks[,2])))

tCrit = qnorm(1 - alpha) 
# calculate VaR
Portfolio$VaRRMAY = calcRMA(Portfolio$PercChange, h = h) ^ 0.5 * tCrit
Portfolio$VaREMAY = calcEMA(Portfolio$PercChange, gamma = gamma, h = h) ^ 0.5 * tCrit
Portfolio$VaRRMAL = Portfolio$VaRRMAY * Portfolio$Price
Portfolio$VaREMAL = Portfolio$VaREMAY * Portfolio$Price
# calculate exceedences
Portfolio$RugsRMAL = (Portfolio$AbsChange > Portfolio$VaRRMAL | Portfolio$AbsChange < -Portfolio$VaRRMAL)
Portfolio$RugsEMAL = (Portfolio$AbsChange > Portfolio$VaREMAL | Portfolio$AbsChange < -Portfolio$VaREMAL)
Portfolio$RugsRMAY = (Portfolio$PercChange > Portfolio$VaRRMAY | Portfolio$PercChange < -Portfolio$VaRRMAY)
Portfolio$RugsEMAY = (Portfolio$PercChange > Portfolio$VaREMAY | Portfolio$PercChange < -Portfolio$VaREMAY)


# remove rows without VaR
Portfolio = Portfolio[- c(1 :h), ]


# Plot Result for Lt/AbsChange
plot(Portfolio$Date, Portfolio$AbsChange, pch = 16, cex = 0.3, #ylim = c(-0.18, 0.18), 
     ylab = expression({L}[t]), xlab = "Year", main = "VaR Timeplot")
lines(Portfolio$Date, Portfolio$VaRRMAL, col = "red", lty = 2, lwd = 2)
lines(Portfolio$Date, - Portfolio$VaRRMAL, col = "red", lty = 2, lwd = 2)
lines(Portfolio$Date, Portfolio$VaREMAL, col = "blue", lwd = 2)
lines(Portfolio$Date, - Portfolio$VaREMAL, col = "blue", lwd = 2)

# Add Rugs, for Rma at the bottom, for Ema at the top
rug(Portfolio$Date[Portfolio$RugsRMAL], side = 1, col = "red", lwd = 2)
rug(Portfolio$Date[Portfolio$RugsEMAL], side = 3, col = "blue", lwd = 2)
points(Portfolio$Date[Portfolio$RugsRMAL], Portfolio$AbsChange[Portfolio$RugsRMAL], col = "red", pch = 4, lwd = 2, cex = 1.5)
points(Portfolio$Date[Portfolio$RugsEMAL], Portfolio$AbsChange[Portfolio$RugsEMAL], col = "blue", pch = 0, lwd = 2, cex = 1.5)
dev.print(device = png, filename = 'VaR_LtAbsChange.png', width = 1200, height = 600)
dev.off()

# Plot Result for Yt/PercChange
plot(Portfolio$Date, Portfolio$PercChange, pch = 16, cex = 0.2, #ylim = c(-0.18, 0.18), 
     ylab = expression({Y}[t]), xlab = "Year", main = "VaR Timeplot")
lines(Portfolio$Date, Portfolio$VaRRMAY, col = "red", lty = 2, lwd = 2)
lines(Portfolio$Date, - Portfolio$VaRRMAY, col = "red", lty = 2, lwd = 2)
lines(Portfolio$Date, Portfolio$VaREMAY, col = "blue", lwd = 2)
lines(Portfolio$Date, - Portfolio$VaREMAY, col = "blue", lwd = 2)

# Add Rugs, for Rma at the bottom, for Ema at the top
Portfolio$RugsRMAY = (Portfolio$PercChange > Portfolio$VaRRMAY | Portfolio$PercChange < -Portfolio$VaRRMAY)
Portfolio$RugsEMAY = (Portfolio$PercChange > Portfolio$VaREMAY | Portfolio$PercChange < -Portfolio$VaREMAY)
# plot rug
rug(Portfolio$Date[Portfolio$RugsRMAY], side = 1, col = "red")
rug(Portfolio$Date[Portfolio$RugsEMAY], side = 3, col = "blue")
# plot exceedences
points(Portfolio$Date[Portfolio$RugsRMAY], Portfolio$PercChange[Portfolio$RugsRMAY], col = "red", pch = 4, lwd = 2, cex = 1.5)
points(Portfolio$Date[Portfolio$RugsEMAY], Portfolio$PercChange[Portfolio$RugsEMAY], col = "blue", pch = 0, lwd = 2, cex = 1.5)
dev.print(device = png, filename = 'VaR_YtPercChange.png', width = 1200, height = 600)
dev.off()

# Plot qqplots
qqnorm(Portfolio$AbsChange/Portfolio$VaRRMAL, main = "VaR (RMA) Reliability",
       xlab = "Theoretical Quantiles", ylab = "P&L over VaR Quantiles", ylim = c(-3,3))
qqline(Portfolio$AbsChange/Portfolio$VaRRMAL)
dev.print(device = png, filename = 'VaRReliability_RMA.png', width = 500, height = 500)
dev.off()
qqnorm(Portfolio$AbsChange/Portfolio$VaREMAL, main = "VaR (EMA) Reliability",
       xlab = "Theoretical Quantiles", ylab = "P&L over VaR Quantiles", ylim = c(-3,3))
qqline(Portfolio$AbsChange/Portfolio$VaREMAL)
dev.print(device = png, filename = 'VaRReliability_EMA.png', width = 500, height = 500)
dev.off()





########################################################################################
############## PLOT OUTLIERS AS function of time for 80% significance level  (alpha=20%)


# Plot RMA outliers over time
plot(Portfolio$Date, Portfolio$AbsChange, col="white", pch = 16, cex = 0.3, ylim = c(0,1), 
     ylab = expression({Z}[t]), 
     xlab = "Year", 
     main = "Time plot for exceedances for RMA at 80% significance level",
     yaxt="n")
# Add Rugs, for Rma at the bottom, for Ema at the top
rug(Portfolio$Date[Portfolio$RugsRMAL], side = 3, col = "red", lwd = 2)
rug(Portfolio$Date[!Portfolio$RugsRMAL], side = 1, col = "blue", lwd = 2)

dev.print(device = png, filename = 'RMA_outliers_t.png', width = 1200, height = 600)
dev.off()


# Plot RMA outliers over time
plot(Portfolio$Date, Portfolio$AbsChange, col="white", pch = 16, cex = 0.3, ylim = c(0,1), 
     ylab = expression({Z}[t]), 
     xlab = "Year", 
     main = "Time plot for exceedances for EMA at 80% significance level",
     yaxt="n")
# Add Rugs, for Rma at the bottom, for Ema at the top
rug(Portfolio$Date[Portfolio$RugsEMAL], side = 3, col = "red", lwd = 2)
rug(Portfolio$Date[!Portfolio$RugsEMAL], side = 1, col = "blue", lwd = 2)

dev.print(device = png, filename = 'EMA_outliers_t.png', width = 1200, height = 600)
dev.off()

###############################################
############## BACKTESTING - Expected Shortfall


# Calculates sigmas RMA/EMA for percentage and absolute changes
Portfolio$sigmaRMAY = Portfolio$VaRRMAY/tCrit
Portfolio$sigmaEMAY = Portfolio$VaREMAY/tCrit
Portfolio$sigmaRMAL = Portfolio$sigmaRMAY * Portfolio$Price
Portfolio$sigmaEMAL = Portfolio$sigmaEMAY * Portfolio$Price



ALPHA <- 0.20

U <- qnorm(1-ALPHA)


ES_n  <- dnorm(U, mean = 0, sd = 1, log = FALSE)/(ALPHA) 
sigmaES_n <- sqrt(1 + U*ES_n - (ES_n)^2)

# Define -Zt  for Expected Shortfall
Portfolio$Z_RMA <- -(Portfolio$AbsChange/(Portfolio$sigmaRMAL))
Portfolio$Z_EMA <- -(Portfolio$AbsChange/(Portfolio$sigmaEMAL))


Portfolio$Out_Z_RMA <-  ifelse (Portfolio$Z_RMA > U,
                                Portfolio$Z_RMA, 
                                NA) 
Portfolio$Out_Z_EMA <-  ifelse (Portfolio$Z_EMA > U,
                                Portfolio$Z_EMA, 
                               NA)


# Calculate Expected Shortfall
N_RMA <- nrow(Portfolio[!is.na(Portfolio$Out_Z_RMA),])
N_EMA <- nrow(Portfolio[!is.na(Portfolio$Out_Z_EMA),])

NU_RMA  <- sum(Portfolio$Out_Z_RMA, na.rm = TRUE)/N_RMA
NU_EMA  <- sum(Portfolio$Out_Z_EMA, na.rm = TRUE)/N_EMA

sigma_NU_RMA <- sqrt(abs(1 + U*NU_RMA - (NU_RMA)^2))
sigma_NU_EMA <- sqrt(abs(1 + U*NU_EMA - (NU_EMA)^2))


# Hypothesis testing for ES_n =1.4
test_RMA <- sqrt(N_RMA) * ((NU_RMA - ES_n)/sigma_NU_RMA)
test_EMA <- sqrt(N_EMA) * ((NU_EMA - ES_n)/sigma_NU_EMA)

sign_RMA <- 1-pnorm(test_RMA, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
sign_EMA <- 1-pnorm(test_EMA, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)



# Hypothesis testing for ES_t=1.47 and t(df=20)
# Wrong calculation for sigmaEs_t, take from slides 0.546

v <- 20
Ut <-qt(1-ALPHA, df=v)
ES_t <- dt(Ut, df=v)*((v+Ut^2)/(v-1))/(ALPHA)  #1.47

dt(Ut, df=v)*((v+Ut^2)/(v-1))/(ALPHA)
sigmaES_t <- sqrt(1 + Ut*ES_t - (ES_t)^2)*(v/(v-2))


test_RMA_t <- sqrt(N_RMA) * ((NU_RMA - ES_t)/sigma_NU_RMA)
test_EMA_t <- sqrt(N_EMA) * ((NU_EMA - ES_t)/sigma_NU_EMA)

sign_RMA_t <- 1-pnorm(test_RMA_t, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
sign_EMA_t <- 1-pnorm(test_EMA_t, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


# Results for N(0,1) assumption
NU_RMA
NU_EMA
sigma_NU_RMA
sigma_NU_EMA
test_RMA
test_EMA
sign_RMA*100
sign_EMA*100
N_RMA
N_EMA

# Results for t(20) assumption

test_RMA_t
test_EMA_t
sign_RMA_t*100
sign_EMA_t*100

##############################################################################
#################### REMOVE OUTLIERS 


# hist(Portfolio$AbsChange)
# nrow(Portfolio[abs(Portfolio$AbsChange)>20,])
# nrow(Portfolio)

# Delete 10 outliers with abs(Lt) above 20
Portfolio <- subset(Portfolio, abs(AbsChange)<=20)
#nrow(Portfolio)

Portfolio$Out_Z_RMA <-  ifelse (Portfolio$Z_RMA > U,
                                Portfolio$Z_RMA, 
                                NA) 
Portfolio$Out_Z_EMA <-  ifelse (Portfolio$Z_EMA > U,
                                Portfolio$Z_EMA, 
                                NA)


# Calculate Expected Shortfall
N_RMA <- nrow(Portfolio[!is.na(Portfolio$Out_Z_RMA),])
N_EMA <- nrow(Portfolio[!is.na(Portfolio$Out_Z_EMA),])

NU_RMA  <- sum(Portfolio$Out_Z_RMA, na.rm = TRUE)/N_RMA
NU_EMA  <- sum(Portfolio$Out_Z_EMA, na.rm = TRUE)/N_EMA

sigma_NU_RMA <- sqrt(abs(1 + U*NU_RMA - (NU_RMA)^2))
sigma_NU_EMA <- sqrt(abs(1 + U*NU_EMA - (NU_EMA)^2))


# Hypothesis testing for ES_n =1.4
test_RMA <- sqrt(N_RMA) * ((NU_RMA - ES_n)/sigma_NU_RMA)
test_EMA <- sqrt(N_EMA) * ((NU_EMA - ES_n)/sigma_NU_EMA)

sign_RMA <- 1-pnorm(test_RMA, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
sign_EMA <- 1-pnorm(test_EMA, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)



# Hypothesis testing for ES_t=1.47 and t(df=20)
# Wrong calculation for sigmaEs_t, take from slides 0.546

v <- 20
Ut <-qt(1-ALPHA, df=v)
ES_t <- dt(Ut, df=v)*((v+Ut^2)/(v-1))/(ALPHA)  #1.47

sigmaES_t <- sqrt(1 + Ut*ES_t - (ES_t)^2)*(v/(v-2))


test_RMA_t <- sqrt(N_RMA) * ((NU_RMA - ES_t)/sigma_NU_RMA)
test_EMA_t <- sqrt(N_EMA) * ((NU_EMA - ES_t)/sigma_NU_EMA)

sign_RMA_t <- 1-pnorm(test_RMA_t, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
sign_EMA_t <- 1-pnorm(test_EMA_t, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


# Results for N(0,1) assumption
NU_RMA
NU_EMA
sigma_NU_RMA
sigma_NU_EMA
test_RMA
test_EMA
sign_RMA*100
sign_EMA*100
N_RMA
N_EMA

# Results for t(20) assumption

test_RMA_t
test_EMA_t
sign_RMA_t*100
sign_EMA_t*100


##############################################################################
##############################################################################


# Standard Deviation for variable L(t+1)/VaR(t) see in the book: formula (16.19) 
sd_RMA <- sd(Portfolio$Z_RMA/tCrit, na.rm = TRUE)
sd_EMA <- sd(Portfolio$Z_EMA/tCrit, na.rm = TRUE)

(sd_RMA-sd_N)/sd_N
(sd_EMA-sd_N)/sd_N


##############################################################################
## Investigate QQ-plots to fit better distribution than N(0,1) because of leptocurtic tails
## The best fit for standard t-Student with df=4

y1 <- -(Portfolio$Z_RMA)
y2<- -(Portfolio$Z_EMA)


qqplot(rt(500, df = 4), y1,
       ylim = c(-3,3),
       xlim = c(-3,3))
qqline(y, col = 2)


qqplot(rt(500, df = 4), y2,
       ylim = c(-3,3),
       xlim = c(-3,3))
qqline(y, col = 2)

