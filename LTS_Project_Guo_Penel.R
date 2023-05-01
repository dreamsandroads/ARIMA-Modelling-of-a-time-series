rm(list=ls())
# ========== import and install required libraries =========
require(zoo)
# install.packages("tseries")
require(tseries) 
library(tseries)
# set path
path <- "/Users/edouardpenel/Desktop"
setwd(path) 
getwd() # working directory
list.files() # list files in wd
# CHOSEN SERIES : https://www.insee.fr/fr/statistiques/serie/010537315#Telechargement



# =========== QUESTION 1 ======================
datafile <- "valeurs_mensuelles.csv" 
data <- read.csv(datafile,sep=";")
dates_char <- as.character(data$dates)
dates_char[1] # first date
tail(dates_char,1) # last date
dates <- as.yearmon(seq(from=1990+0/12, to=2016+10/12, by=1/12)) 
spread <- zoo(data$spread, order.by=dates)
plot(spread, xlab = "dates", ylab = "values") # plot initial series

# decomposition of the time series (not necessary)
decomp<-decompose(spread)  
plot(decomp)

# ANSWER: The chosen series represents the Industrial Production Index associated
# to the manufacturing of tobacco-based products from 1990 until november 2016
# We observe a decreasing linear trend. The series is not stationary.

# ========== QUESTION 2 ========================
dspread <- diff(spread,1) # differentiation at order 1
plot(dspread, xlab="dates", ylab = "dvalues")
# ANSWER: the differentiated series seems stationary.

# Stationarity tests
# 1. Augmented Dickey-Fuller Test
adf <- adf.test(dspread) 
adf 
# p-value < 0.01
# Thus we can reject the null hypothesis of non-stationarity at level 1%. 

# 2. Phillips-Perron Unit Root Test
pp <- pp.test(dspread)
pp 
# p-value < 0.01
# We have the same conclusion.

# 3. KPSS Stationary Test
kpss <- kpss.test(dspread)
kpss 
# p-value > 0.1
# We can't reject the null hypothesis of stationarity at any known level (1%, 5% and 10%).

# ANSWER: we can suppose from now on that our differentiated series is stationary.


# ========== QUESTION 3 ========================
plot(cbind(spread,dspread)) # plot both graphs


# ========== QUESTION 4 & 5 ========================
# Choosing ARMA(p*, q*) best model for prediction

par(mfrow=c(1,2))
acf(dspread) ; # traces the complete autocorrelation functions 
pacf(dspread) # traces the partial autocorrelation functions

# The ACF is statistically significant in the 13th order maximum, 
# we will therefore choose q* = 13. 
# The PACF is also statistically significant in the third order maximum, 
# we will pick p* = 3.


# Student Test
signif <- function(estim){
  coef <- estim$coef
  se <- sqrt(diag(estim$var.coef))
  t <- coef/se
  pval <- (1-pnorm(abs(t)))???2
  return(rbind(coef,se,pval))
}

# Ljung-Box test
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}

# Combining both tests
arimafit <- function(estim){
  adjust <- round(signif(estim),3)
  pvals <- Qtests(estim$residuals,24,length(estim$coef)-1)
  pvals <- matrix(apply(matrix(1:24,nrow=6),2,function(c) round(pvals[c,],3)),nrow=6)
  colnames(pvals) <- rep(c("lag", "pval"),4)
  cat("coefficients nullity tests :\n")
  print(adjust)
  cat("\n tests of autocorrelation of the residuals : \n")
  print(pvals) }

# Examples of models which are not good 
# (because not well-adjusted or not valid)
arima111 <- arima(spread,c(1,1,1),include.mean=F)
arima215 <- arima(spread,c(2,1,5),include.mean=F)
arimafit(arima111)
arimafit(arima215)


# So we estimate ARIMA sub-models with 0 <= p <= 3 and 0 <= q <= 13
# We compute AIC and BIC for each of these models

pmax <- 3
qmax <- 13

mat <- matrix(NA,nrow=pmax+1,ncol=qmax+1) #empty matrix to fill
rownames(mat) <- paste0("p=",0:pmax) #renames lines
colnames(mat) <- paste0("q=",0:qmax) #renames columns
AICs <- mat #AIC matrix not filled 
BICs <- mat #BIC matrix not filled 
pqs <- expand.grid(0:pmax,0:qmax) #all possible combinations of p and q
for (row in 1:dim(pqs)[1]){ #loop for each (p,q)
  p <- pqs[row,1] #gets p
  q <- pqs[row,2] #gets q
  estim <- try(arima(dspread,c(p,0,q),include.mean = F)) #tries ARIMA estimation
  AICs[p+1,q+1] <- if (class(estim)=="try-error") NA else estim$aic #assigns the AIC
  BICs[p+1,q+1] <- if (class(estim)=="try-error") NA else BIC(estim) #assigns the BIC
}

AICs #prints the AICs
AICs==min(AICs) #prints the model minimizing the AIC  

arima310 <- arima(spread,c(3,1,0),include.mean=F)
BICs #prints the BICs  

BICs==min(BICs) #prints the model minimizing the BIC
# It's still arima310

# Student and Ljung-Box Tests on chosen model
arima310
arimafit(arima310)
# The AR(3) coefficient is statistically significant
# (the ratio between the estimated coefficient and the standard
# error is higher in absolute value than 1.96)
# so the ARIMA(3,1,0) is well adjusted.

# The model ARIMA(3,1,0) is also valid as we never reject the absence of 
# residual autocorrelation.

# ANSWER: the model ARIMA(3,1,0) is the best model for prediction.
# It minimizes AIC and BIC criteria, is well-adjusted and valid.


# ========== QUESTION 6 & 7 ========================

# See Project report.


# ========== QUESTION 8 ========================

# Library "forecast"
library(forecast)

# Transform with the "ts" function (ts for "time series")
ts_data <- ts(spread)

# Model found in the previous part
arima_model <- arima(ts_data, order = c(3,1,0))

# Prediction of the two futures values (h=2) with computation of confidence regions at level alpha
alpha <- 0.95
forecasted <- forecast(arima_model, h=2,level = alpha)

# Plot with new values and confidence regions zoomed in on latest values 
plot(forecasted, xlab = "Time", ylab = "Values",xlim = c(250,340), ylim = c(0,400))

# Plot with new values and confidence regions 
plot(forecasted, xlab = "Time", ylab = "Values")

# ========== QUESTION 9 ========================

# See Project report.
