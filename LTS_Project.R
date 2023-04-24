### Question 1 ###

# CHOSEN SERIE : https://www.insee.fr/fr/statistiques/serie/010537315#Telechargement 
# The chosen series represent the industrial production related to products using tobacco until 2017 # 

library(stringr)
path <- "/Users/edouardpenel/Desktop"
setwd(path)
datafile <- "valeurs_mensuelles.csv"
data <- read.csv(datafile, sep=";")

### Question 2 ### 

require(zoo)
dates_char <- as.character(data$Libellé)
dates_char <- str_replace_all(dates_char, "-", "m")
dates_char[1] #
tail(dates_char,1) #
dates <- as.yearmon(seq(from=1990+0/12, to=2017+0/12, by=1/12)) #
spread <- zoo(data$spread, order.by=dates)

dspread <- diff(spread,1)

# Justification 

# install.packages("tseries")
require(tseries)
library (tseries)

# Augmented Dickey-Fuller test 
adf.test(dspread)
# p-value < 0.01
# Thus we can reject the null hypothesis of non-stationarity at level 1%. 

# Phillips-Perron (PP) 
pp.test(dspread)
# p-value < 0.01
# We have the same conclusion 

# KPSS test 
kpss.test(dspread)
# p-value > 0.1
# We can't reject the null hypothesis of stationary at any known level (1%, 5% and 10%).




### Question 3 ### 

plot(cbind(spread,dspread))

### Question 4 ### 

x <- dspread
par(mfrow =c(1,2))
acf(x);pacf(x)
# The maximum orders of the ARMA model are q = 13 and p = 3

# So, we have to look for an ARMA model with p <= 3 and q <= 13
pmax <- 3
qmax <- 13 

mat <- matrix(NA,nrow=pmax+1,ncol=qmax+1) #empty matrix to fill
rownames(mat) <- paste0("p=",0:pmax) #renames lines
colnames(mat) <- paste0("q=",0:qmax) #renames columns
AICs <- mat #AIC matrix 
BICs <- mat #BIC matrix 
pqs <- expand.grid(0:pmax,0:qmax) #all possible combinations of p and q
for (row in 1:dim(pqs)[1]){ #loop for each (p,q)
  p <- pqs[row,1] #gets p
  q <- pqs[row,2] #gets q
  estim <- try(arima(x,c(p,0,q),include.mean = F)) #tries ARIMA estimation 
  AICs[p+1,q+1] <- if (class(estim)=="try-error") NA else estim$aic #assigns the AIC 
  BICs[p+1,q+1] <- if (class(estim)=="try-error") NA else BIC(estim) #assigns the BIC
}
AICs

AICs == min(AICs)

BICs 
BICs == min(BICs)

# The two (AIC and BIC) criteria information give q = 0 and p = 3 

# Thus we estimate the series as an ARMA(3,0) model 
arima310 <- arima(spread,c(3,1,0),include.mean = F)

# We just have to perform a residual autocorrelation test 

Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}

Qtests(arima310$residuals,24, fitdf = 3)

# We clearly see that we never reject the absence of residual autocorrelation.

### Question 5 ###
# ARIMA (3,1,0)

arima310

# The alsolute value of the ratio between the coefficient and the standard error is larger than 1.96.
# We can conclude that all coefficients are significant at level 5%.

### Question 6 ### 

# We assume that residuals are gaussian 
# We have first to compute the standard errors of residuals 
mean <- mean(arima310$residuals)
var <- var(arima310$residuals)
sd <- sqrt(var)
alpha <- 0.05

qsup <- qnorm(1 - alpha/2, mean, sd)
qinf <- qnorm (alpha/2, mean, sd)

# Now we know that X_T+1 is in [predict(X_T+1) + qinf; precit(X_T+1) + qsup]
# Same for X_T+2 ? 

### Question 7 ### 

### Question 8 ### 

### Question 9 ### 
