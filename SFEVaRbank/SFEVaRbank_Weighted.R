# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("fExtremes", "dplyr", "lubridate", "ggplot2", "scales")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)



# Load Data
data = read.csv("SFEVaRbank.csv")
# names(datafull)



################  DEFINE METAPARAMETERS  ################ 

#### Choose names of the stocks

STOCKS <- c("HSBC.HDG."
            , "ALLIANZ"
            , "BARCLAYS"
            , "ADIDAS"
            , "UNILEVER"
            )

# STOCKS <- c("LLOYDS.BANKING.GROUP")


#### Define weights (lambda) - assumption (portfolio is not traded, weights are constant over time)

 WEIGHTS <- c(0.4
             ,0.2
             ,0.1
             ,0.2
             ,0.1
              )

# WEIGHTS <- 1

### Narrow the years span (available 2004:2014)
YEARS <- c(2007:2009)

### Define time horizon for historical observations
H <- 250

### Define alpha level for VaR calculation
ALPHA <- 0.01

################################################ 



tCrit <- qnorm(1 - ALPHA)
d <- length(STOCKS)


data <- as.data.frame(datafull[year(datafull$Date) %in% YEARS,c("Date", STOCKS)])


#### DIFFERENCE OF LOG RETURNS for each stock
data_logs <-as.data.frame(data$Date)

for (i in STOCKS)
{
  data_logs[paste0("l_",i)] <- c(NA,diff(log(data[,c(i)])))
}


#delete first row as no differences for this period, also for data
data_logs <-data_logs[-1,]


DF <- cbind(data[-nrow(data),],data_logs[,-1])
DF$Date<- as.Date(DF$Date)
rm(datafull, data)


# EXPOSURE VECTOR - w(t)
# PORTFOLIO LOSS/PROFIT vector -  L(t)
# as 1) absolute price changes weighted with weights - lambda (assumed to be constant, as no adjustment of the portfolio)
# OR 2) approximated by difference of logs /Taylor approx./ to show percentage change, 
#       so have to be weighted by exposure vector -  w(t)
# Here: calculate from option 2)


DF$Lt <-0
w <- list()

for (i in 1:nrow(DF))
{
  # Define exposure vectors in t-dimensional lists  # Esposure vector w(t)
  w[[i]] <- t(as.vector(WEIGHTS  # lambdas for each stock d
                        * DF[i,2:(1+d)])) #current price from t for each stock d
  
  # PORTFOLIO LOSS/PROFIT vector -  L(t)
  DF$Lt[i] <- t(w[[i]])%*%t(as.matrix(DF[i,(d+2):(2*d+1)])) #diff of logs from t
}



# Estimate SIGMA RMA

DF$sigmaRMA <- as.numeric(NA)
DF$covRMA <- as.numeric(NA)

y <- list()
covRMA <- list()

for (i in H:nrow(DF))
{
  y[[i]] <- as.matrix(DF[(i-H+1):i,(1+d+1):(2*d+1)])
  
  covRMA[[i]] <- (t(y[[i]])%*%y[[i]])/H
  
        DF$covRMA[i] = (t(y[[i]])%*%y[[i]])/H
  
  DF$sigmaRMA[i] = sqrt(t(w[[i]])%*%covRMA[[i]]%*%w[[i]])
}



# Estimate SIGMA EMA

DF$sigmaEMA <- as.numeric(NA)
DF$covEMA <- as.numeric(NA)


y <- list()
yEMA <- list()
covEMA <- list()
GAMMA <- 0.94

for (i in H:nrow(DF))
{
  y[[i]] <- as.matrix(DF[(i-H+1):i,(1+d+1):(2*d+1)])
  
  yEMA[[i]] <- y[[i]]*sqrt(GAMMA^(H-(row(y[[i]])[,1])))
  #yEMA[[i]]  <- (as.matrix(sapply (1:H , function(x) {y[[i]][x,]*(GAMMA^(H-x))})))
  
  covEMA[[i]] <- (t(yEMA[[i]])%*%yEMA[[i]])*(1-GAMMA)
  
      DF$covEMA[i]<- (t(yEMA[[i]])%*%yEMA[[i]])*(1-GAMMA)
  
  DF$sigmaEMA[i] = sqrt(t(w[[i]])%*%covEMA[[i]]%*%w[[i]])
}



# Calculate VaR

DF$VaRRMA <- DF$sigmaRMA * tCrit
DF$VaREMA <- DF$sigmaEMA * tCrit

DF$VaRRMA_COV <- sqrt(DF$covRMA) * tCrit
DF$VaREMA_COV <- sqrt(DF$covEMA) * tCrit

DF$OutRMA <- ifelse((DF$Lt > DF$VaRRMA) | (DF$Lt < - DF$VaRRMA),DF$Lt,NA)
DF$OutEMA <- ifelse((DF$Lt > DF$VaREMA) | (DF$Lt < - DF$VaREMA),DF$Lt,NA)



# Plot Results

ggplot(DF[!is.na(DF$VaRRMA),],aes(x=Date)) +
  geom_point(aes(y=Lt)) +
  #geom_point(aes(y=(OutRMA), group=1), color="red") +
  #geom_point(aes(y=(OutEMA), group=1), color="yellow")+
  geom_line(aes(y=VaRRMA, group=1), color="green", size=0.8)+
  geom_line(aes(y=-VaRRMA, group=1), color="green", size=0.8)+
  geom_line(aes(y=VaREMA, group=1), color="blue", size=0.8)+
  geom_line(aes(y=-VaREMA, group=1), color="blue", size=0.8)+
  labs(x = "Date", y = "Portfolio Returns", 
       title = "VaR and observed changes") +
  scale_x_date(labels = date_format("%m-%Y"))

