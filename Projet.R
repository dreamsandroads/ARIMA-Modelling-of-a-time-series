# PRELIMINARY
require(zoo) #format de serie temporelle pratique et facile d'utilisation (mais plus volumineux)
# install.packages("tseries")
require(tseries) #diverses fonctions sur les series temporelles
library(tseries)
path <- "D:/ENSAE/2A S2/Linear Time Series/Projet"
setwd(path) #definit l'espace de travail (working directory ou "wd")
getwd() # working directory
list.files() # list files in wd

# CHOSEN SERIE : https://www.insee.fr/fr/statistiques/serie/010537315#Telechargement


# =========== QUESTION 1 ======================
datafile <- "valeurs_mensuelles.csv" 

data <- read.csv(datafile,sep=";") #import .csv file into data.frame class

dates_char <- as.character(data$dates)
dates_char[1] # first date
tail(dates_char,1) # last date
dates <- as.yearmon(seq(from=1990+0/12, to=2019+0/12, by=1/12)) 
spread <- zoo(data$spread, order.by=dates)
plot(spread) # plot initial series
# manufacturing of tabacco based products  
# decreasing linear trend ?

# decomposition of the time series (not necessary)
decomp<-decompose(spread)  
plot(decomp) 

# ========== QUESTION 2 ========================
dspread <- diff(spread,1) # differentiation at order 1

# JUSTIFICATION
# Augmented Dickey-Fuller Test
# Ho = unit root = non-stationarity hypothesis
adf <- adf.test(dspread) 
adf # p value < 0.01 so we reject Ho in favor of the alternative hypothesis
# lag order 7
# t-value -7.8183

# Phillips-Perron Unit Root Test
# Ho = unit root = non-stationarity hypothesis
pp <- pp.test(dspread)
pp # p value < 0.01 so we reject Ho in favor of the alternative hypothesis
# lag order 5
# t value -391.67

# KPSS Stationary Test
# Ho = stationarity hypothesis
kpss <- kpss.test(dspread)
kpss # p value > 0.1 so we can't reject Ho 
# lag order 5
# t value 0.03925

# Conclusion : one differentiation is enough, we can suppose that our series is
# stationary


# ========== QUESTION 3 ========================
plot(cbind(spread,dspread)) # plot both graphs

# ========== QUESTION 4 ========================
# Choosing ARMA(p*, q*) model

par(mfrow=c(1,2))
acf(dspread) ; # traces the complete autocorrelation functions 
pacf(dspread) # traces the partial autocorrelation functions

# The ACF is statistically significant in the second order maximum, 
# we well therefore choose q* = 2. 
# The PACF is also statistically significant in the third order maximum, 
# we will pick p* = 3.

# So we estimate ARIMA Models with 0 <= p <= 3 and 0 <= q <= 2
# => AIC and BIC

pmax <- 3
qmax <- 2

mat <- matrix(NA,nrow=pmax+1,ncol=qmax+1) #empty matrix to fill
rownames(mat) <- paste0("p=",0:pmax) #renames lines
colnames(mat) <- paste0("q=",0:qmax) #renames columns
AICs <- mat #AIC matrix not filled non remplie
BICs <- mat #BIC matrix not filled non remplie
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
# It's arima310

# ESTIMATE THE MODEL PARAMETERS

arima310
# The AR(3) coefficient is statistically significant
#  (the ratio between the estimated coefficient and the standard
# error is higher in absolute value than 1.96)
# so the ARIMA(3,1,0) is well adjusted


# TO DO / COEFFICIENTS OF THE CHOSEN ARMA MODEL

# ========== QUESTION 5 ========================


#Ljung Box test : autocorrelation of the residuals

Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}

Qtests(arima310$residuals,24,fitdf=1)

# We never reject the absence of residual autocorrelation.
# The ARIMA(3,1,0) is valid

# TO DO QQPLOT NORMALITY OF RESIDUALS
# JARQUE BERA TEST ???

# TO DO Write the arma model


