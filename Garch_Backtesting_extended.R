#GARCH + Backtesting
library(zoo)
library(tidyverse)
library(ggthemes)
library(forecast)
library(tseries)
library(gridExtra)
library(rugarch)
citation("rugarch")

#############################################################################################

df = read.csv("data-avg-predict_extended.csv",  header = TRUE, sep=",", dec="." , check.names=TRUE)
df[df == 0 ] = NA
Sents = df[,6:9]
#calculate average sentiment
avg_sent = rowMeans(Sents, na.rm = TRUE)
#replace NaNs
avg_sent[is.nan(avg_sent)] = 0.000000001
#append avg_sent to df
df = cbind(df, avg_sent)

Exch_Sent_old = df %>% select(Date , EUR.USD.Close , avg_sent)
Exch_Sent = Exch_Sent_old[-c(1044:1637),] #delete weekends, since no exchangedata generated

acf(Exch_Sent$EUR.USD.Close)
ccf(Exch_Sent$EUR.USD.Close, Exch_Sent$avg_sent)

#############################################################################################

# plot 
r = Exch_Sent[,2]
summary(r)
plot.ts(r, main = "xchange rates")

#getting the returns
rets = diff(log(r)) * 100
plot.ts(rets, main = "Daily returns", xlab = "D
        ate", ylab = "Return in percent")
Sent_delta = diff(Exch_Sent$avg_sent)*100
#check for correlation
acf(rets)
ccf(rets, Sent_delta)
ccf(rets, Exch_Sent$avg_sent)

#Finding Best Mean Model Using ARIMA
fit1 = auto.arima(rets, trace=TRUE, test="kpss",  ic="aic")

####Augmented dickey-FUller-Test for staionarity
adf.test(fit1$residuals^2) #--> stationary

####Test for ARCH Effect

Box.test(fit1$residuals^2,lag=30, type="Ljung-Box") #p-valus<5% --> Garch!
#############################################################################################
                          
                          ######################################
###########################Fit Garch without external regressor##############################
                          ######################################

###Parameters: sGARCH, norm------------------------------------------------------------------

model.spec11 = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) , 
                        mean.model = list(armaOrder = c(0 , 1)), distribution.model = "norm")

model.fit11 = ugarchfit(spec = model.spec11 , data = rets)
model.fit11
model.fit11@fit$matcoef

###Parameters: eGARCH, norm------------------------------------------------------------------
model.spec12 = ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) , 
                         mean.model = list(armaOrder = c(0 , 1)), distribution.model = "norm")

model.fit12 = ugarchfit(spec = model.spec12 , data = rets)
model.fit12
model.fit12@fit$matcoef

###Parameters: sGARCH, std------------------------------------------------------------------

model.spec21 = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) , 
                         mean.model = list(armaOrder = c(0 , 1)), distribution.model = "std")

model.fit21 = ugarchfit(spec = model.spec21 , data = rets)
model.fit21
model.fit21@fit$matcoef


###Parameters: eGARCH, std------------------------------------------------------------------
model.spec22 = ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) , 
                         mean.model = list(armaOrder = c(0 , 1)), distribution.model = "std")

model.fit22 = ugarchfit(spec = model.spec22 , data = rets)
model.fit22
model.fit22@fit$matcoef

###Parameters: sGARCH, ged------------------------------------------------------------------
#generalized error equation
model.spec31 = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) , 
                          mean.model = list(armaOrder = c(0 , 1)), distribution.model = "ged")

model.fit31 = ugarchfit(spec = model.spec31 , data = rets)
model.fit31
model.fit31@fit$matcoef

###Parameters: eGARCH, ged------------------------------------------------------------------
model.spec32 = ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) , 
                          mean.model = list(armaOrder = c(0 , 1)), distribution.model = "ged")

model.fit32 = ugarchfit(spec = model.spec32 , data = rets)
model.fit32
model.fit32@fit$matcoef

                          ######################################
###########################   Fit GARCH with daily avg_Sent     ##############################
                          ######################################

###Parameters: sGARCH, norm------------------------------------------------------------------
model.spec_ex11 = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                   external.regressors = matrix(Exch_Sent$avg_sent)), #adding sentiment as external regressor based on our assumption, that it influences our volatility
  mean.model = list(armaOrder = c(0,1)), distribution.model = "norm")

fit_ex11 =ugarchfit(spec=model.spec_ex11,data= rets)
fit_ex11
fit_ex11@fit$matcoef 

###Parameters: eGARCH, norm------------------------------------------------------------------
model.spec_ex12 = ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                        external.regressors = matrix(Exch_Sent$avg_sent)), #adding sentiment as external regressor based on our assumption, that it influences our volatility
  mean.model = list(armaOrder = c(0,1)), distribution.model = "norm")

fit_ex12 =ugarchfit(spec=model.spec_ex12,data= rets)
fit_ex12
fit_ex12@fit$matcoef 

###Parameters: sGARCH, std------------------------------------------------------------------

model.spec_ex21 = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                        external.regressors = matrix(Exch_Sent$avg_sent)), #adding sentiment as external regressor based on our assumption, that it influences our volatility
  mean.model = list(armaOrder = c(0,1)), distribution.model = "std")

fit_ex21 = ugarchfit(spec=model.spec_ex21,data= rets)
fit_ex21
fit_ex21@fit$matcoef 

###Parameters: eGARCH, std------------------------------------------------------------------
model.spec_ex22= ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                        external.regressors = matrix(Exch_Sent$avg_sent)), #adding sentiment as external regressor based on our assumption, that it influences our volatility
  mean.model = list(armaOrder = c(0,1)), distribution.model = "std")

fit_ex22 =ugarchfit(spec=model.spec_ex22,data= rets)
fit_ex22
fit_ex22@fit$matcoef 

###Parameters: sGARCH, ged------------------------------------------------------------------
#generalized error equation
model.spec_ex31= ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                        external.regressors = matrix(Exch_Sent$avg_sent)), #adding sentiment as external regressor based on our assumption, that it influences our volatility
  mean.model = list(armaOrder = c(0,1)), distribution.model = "ged")

fit_ex31 =ugarchfit(spec=model.spec_ex31,data= rets)
fit_ex31
fit_ex31@fit$matcoef 

###Parameters: eGARCH, ged------------------------------------------------------------------
model.spec_ex32 = ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1), 
                                                   external.regressors = matrix(Exch_Sent$avg_sent)), 
                             mean.model = list(armaOrder = c(0 , 1)), distribution.model = "ged")

fit_ex32 = ugarchfit(spec = model.spec32 , data = rets)
fit_ex32
fit_ex32@fit$matcoef

                          ######################################
###########################  Fit GARCH with daily sent_delta   ##############################
                          ######################################

###Parameters: sGARCH, norm------------------------------------------------------------------
model.spec_ex11.2= ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                        external.regressors = matrix(Sent_delta)), #adding sentiment as external regressor based on our assumption, that it influences our volatility
  mean.model = list(armaOrder = c(0,1)), distribution.model = "norm")

fit_ex11.2 =ugarchfit(spec=model.spec_ex11.2,data= rets)
fit_ex11.2
fit_ex11.2@fit$matcoef 

###Parameters: eGARCH, norm------------------------------------------------------------------
model.spec_ex12.2= ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                        external.regressors = matrix(Sent_delta)), #adding sentiment as external regressor based on our assumption, that it influences our volatility
  mean.model = list(armaOrder = c(0,1)), distribution.model = "norm")

fit_ex12.2 =ugarchfit(spec=model.spec_ex12.2,data= rets)
fit_ex12.2
fit_ex12.2@fit$matcoef 

###Parameters: sGARCH, std------------------------------------------------------------------

model.spec_ex21.2= ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                        external.regressors = matrix(Sent_delta)), #adding sentiment as external regressor based on our assumption, that it influences our volatility
  mean.model = list(armaOrder = c(0,1)), distribution.model = "std")

fit_ex21.2 =ugarchfit(spec=model.spec_ex21.2,data= rets)
fit_ex21.2
fit_ex21.2@fit$matcoef 

###Parameters: eGARCH, std------------------------------------------------------------------
model.spec_ex22.2= ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                        external.regressors = matrix(Sent_delta)), #adding sentiment as external regressor based on our assumption, that it influences our volatility
  mean.model = list(armaOrder = c(0,1)), distribution.model = "std")

fit_ex22.2 =ugarchfit(spec=model.spec_ex22.2,data= rets)
fit_ex22.2
fit_ex22.2@fit$matcoef 

###Parameters: sGARCH, ged------------------------------------------------------------------
#generalized error equation
model.spec_ex31.2= ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                        external.regressors = matrix(Sent_delta)), #adding sentiment as external regressor based on our assumption, that it influences our volatility
  mean.model = list(armaOrder = c(0,1)), distribution.model = "ged")

fit_ex31.2 =ugarchfit(spec=model.spec_ex31.2,data= rets)
fit_ex31.2
fit_ex31.2@fit$matcoef 

###Parameters: eGARCH, ged------------------------------------------------------------------
model.spec_ex32.2 = ugarchspec(
  variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1), 
                        external.regressors = matrix(Sent_delta)), 
  mean.model = list(armaOrder = c(0 , 1)), distribution.model = "ged")

fit_ex32.2 = ugarchfit(spec = model.spec_ex32.2 , data = rets)
fit_ex32.2
fit_ex32.2@fit$matcoef

                          ############################################
########################### Backtest GARCH without external regressor ##############################
                          #############################################

ctrl = list(tol = 1e-7, delta = 1e-9)

###Backtest: sGARCH, norm------------------------------------------------------------------
model.fit_roll11 = ugarchroll(model.spec11, rets, n.start = 522, refit.every = 1, 
                             refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                             VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_roll11, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_roll11, type = "fpm")

#Plot VaR performance
model_VaR11 = zoo(model.fit_roll11@forecast$VaR[, 1])
model_rets11 = zoo(model.fit_roll11@forecast$VaR[, 2])

plot(model_rets11, type = "b", main = "99%VaR Backtesting/ without external regressor sGarch-norm", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR11, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: eGARCH, norm------------------------------------------------------------------
model.fit_roll12 = ugarchroll(model.spec12, rets, n.start = 522, refit.every = 1, 
                               refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                               VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_roll12, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_roll12, type = "fpm")

#Plot VaR performance
model_VaR12 = zoo(model.fit_roll12@forecast$VaR[, 1])
model_rets12 = zoo(model.fit_roll12@forecast$VaR[, 2])

plot(model_rets12, type = "b", main = "99%VaR Backtesting/ without external regressor eGarch-norm", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR12, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: sGARCH, std------------------------------------------------------------------
model.fit_roll21 = ugarchroll(model.spec21, rets, n.start = 522, refit.every = 1, 
                               refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                               VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_roll21, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_roll21, type = "fpm")

#Plot VaR performance
model_VaR21 = zoo(model.fit_roll21@forecast$VaR[, 1])
model_rets21 = zoo(model.fit_roll21@forecast$VaR[, 2])

plot(model_rets21, type = "b", main = "99%VaR Backtesting/ without external regressor sGarch-std", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR21, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: eGARCH, std------------------------------------------------------------------
model.fit_roll22 = ugarchroll(model.spec22, rets, n.start = 522, refit.every = 1, 
                               refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                               VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_roll22, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_roll22, type = "fpm")

#Plot VaR performance
model_VaR22 = zoo(model.fit_roll22@forecast$VaR[, 1])
model_rets22 = zoo(model.fit_roll22@forecast$VaR[, 2])

plot(model_rets22, type = "b", main = "99%VaR Backtesting/ without external regressor eGarch-std", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR22, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: sGARCH, ged------------------------------------------------------------------
model.fit_roll31 = ugarchroll(model.spec31, rets, n.start = 522, refit.every = 1, 
                               refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                               VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_roll31, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_roll31, type = "fpm")

#Plot VaR performance
model_VaR31 = zoo(model.fit_roll31@forecast$VaR[, 1])
model_rets31 = zoo(model.fit_roll31@forecast$VaR[, 2])

plot(model_rets31, type = "b", main = "99%VaR Backtesting/ without external regressor sGarch-ged", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR31, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: eGARCH, ged------------------------------------------------------------------
model.fit_roll32 = ugarchroll(model.spec32, rets, n.start = 522, refit.every = 1, 
                               refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                               VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_roll32, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_roll32, type = "fpm")

#Plot VaR performance
model_VaR32 = zoo(model.fit_roll32@forecast$VaR[, 1])
model_rets32 = zoo(model.fit_roll32@forecast$VaR[, 2])

plot(model_rets32, type = "b", main = "99%VaR Backtesting/ without external regressor eGarch-std", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR32, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))


                          ######################################
###########################Backtest Garch with daily avg_sent##############################
                          ######################################

###Backtest: sGARCH, norm------------------------------------------------------------------
model.fit_ex_roll11 = ugarchroll(model.spec_ex11, rets, n.start = 522, refit.every = 1, 
                                  refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                                  VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_ex_roll11, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_ex_roll11, type = "fpm")

#Plot VaR performance
model_VaR_ex_11 = zoo(model.fit_ex_roll11@forecast$VaR[, 1])
model_rets_ex_11 = zoo(model.fit_ex_roll11@forecast$VaR[, 2])

plot(model_rets_ex_11, type = "b", main = "99%VaR Backtesting/ with daily avg_sent sGarch-norm", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR_ex_11, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: eGARCH, norm------------------------------------------------------------------
model.fit_ex_roll12 = ugarchroll(model.spec_ex12, rets, n.start = 522, refit.every = 1, 
                                  refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                                  VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_ex_roll12, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_ex_roll12, type = "fpm")

#Plot VaR performance
model_VaR_ex_12 = zoo(model.fit_ex_roll12@forecast$VaR[, 1])
model_rets_ex_12 = zoo(model.fit_ex_roll12@forecast$VaR[, 2])

plot(model_rets_ex_12, type = "b", main = "99%VaR Backtesting/ with daily avg_sent eGarch-norm", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR_ex_12, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: sGARCH, std------------------------------------------------------------------
model.fit_ex_roll21 = ugarchroll(model.spec_ex21, rets, n.start = 522, refit.every = 1, 
                                  refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                                  VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_ex_roll21, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_ex_roll21, type = "fpm")

#Plot VaR performance
model_VaR_ex_21 = zoo(model.fit_ex_roll21@forecast$VaR[, 1])
model_rets_ex_21 = zoo(model.fit_ex_roll21@forecast$VaR[, 2])

plot(model_rets_ex_21, type = "b", main = "99%VaR Backtesting/ with daily avg_sent sGarch-std", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR_ex_21, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: eGARCH, std------------------------------------------------------------------
model.fit_ex_roll22 = ugarchroll(model.spec_ex22, rets, n.start = 522, refit.every = 1, 
                                  refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                                  VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_ex_roll22, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_ex_roll22, type = "fpm")

#Plot VaR performance
model_VaR_ex_22 = zoo(model.fit_ex_roll22@forecast$VaR[, 1])
model_rets_ex_22 = zoo(model.fit_ex_roll22@forecast$VaR[, 2])

plot(model_rets_ex_22, type = "b", main = "99%VaR Backtesting/ with daily avg_sent eGarch-std", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR_ex_22, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: sGARCH, ged------------------------------------------------------------------
model.fit_ex_roll31 = ugarchroll(model.spec_ex31, rets, n.start = 522, refit.every = 1, 
                                  refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                                  VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_ex_roll31, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_ex_roll31, type = "fpm")

#Plot VaR performance
model_VaR_ex_31 = zoo(model.fit_ex_roll31@forecast$VaR[, 1])
model_rets_ex_31 = zoo(model.fit_ex_roll31@forecast$VaR[, 2])

plot(model_rets_ex_31, type = "b", main = "99%VaR Backtesting/ with daily avg_sent sGarch-ged", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR_ex_31, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: eGARCH, ged-----------------------------------------------------------------
model.fit_ex_roll32 = ugarchroll(model.spec_ex32, rets, n.start = 522, refit.every = 1, 
                                  refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                                  VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_ex_roll32, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_ex_roll32, type = "fpm")

#Plot VaR performance
model_VaR_ex_32 = zoo(model.fit_ex_roll32@forecast$VaR[, 1])
model_rets_ex_32 = zoo(model.fit_ex_roll32@forecast$VaR[, 2])

plot(model_rets_ex_32, type = "b", main = "99%VaR Backtesting/ with daily avg_sent eGarch-ged", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR_ex_32, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

                          ######################################
###########################   Backtest Garch with Sent_delta  ##############################
                          ######################################

###Backtest: sGARCH, norm------------------------------------------------------------------
model.fit_ex_roll11.2 = ugarchroll(model.spec_ex11.2, rets, n.start = 522, refit.every = 1, 
                                    refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                                    VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_ex_roll11.2, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_ex_roll11.2, type = "fpm")

#Plot VaR performance
model_VaR_ex_11.2 = zoo(model.fit_ex_roll11.2@forecast$VaR[, 1])
model_rets_ex_11.2 = zoo(model.fit_ex_roll11.2@forecast$VaR[, 2])

plot(model_rets_ex_11.2, type = "b", main = "99%VaR Backtesting/ with sent_delta sGarch-norm", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR_ex_11.2, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: eGARCH, norm------------------------------------------------------------------
model.fit_ex_roll12.2 = ugarchroll(model.spec_ex12.2, rets, n.start = 522, refit.every = 1, 
                                    refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                                    VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_ex_roll12.2, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_ex_roll12.2, type = "fpm")

#Plot VaR performance
model_VaR_ex_12.2 = zoo(model.fit_ex_roll12.2@forecast$VaR[, 1])
model_rets_ex_12.2 = zoo(model.fit_ex_roll12.2@forecast$VaR[, 2])

plot(model_rets_ex_12.2, type = "b", main = "99%VaR Backtesting/ with sent_delta eGarch-norm", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR_ex_12.2, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: sGARCH, std------------------------------------------------------------------
model.fit_ex_roll21.2 = ugarchroll(model.spec_ex21.2, rets, n.start = 522, refit.every = 1, 
                                    refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                                    VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_ex_roll21.2, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_ex_roll21.2, type = "fpm")

#Plot VaR performance
model_VaR_ex_21.2 = zoo(model.fit_ex_roll21.2@forecast$VaR[, 1])
model_rets_ex_21.2 = zoo(model.fit_ex_roll21.2@forecast$VaR[, 2])

plot(model_rets_ex_21.2, type = "b", main = "99%VaR Backtesting/ with sent_delta sGarch-std", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR_ex_21.2, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: eGARCH, std------------------------------------------------------------------
model.fit_ex_roll22.2 = ugarchroll(model.spec_ex22.2, rets, n.start = 522, refit.every = 1, 
                                    refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                                    VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_ex_roll22.2, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_ex_roll22.2, type = "fpm")

#Plot VaR performance
model_VaR_ex_22.2 = zoo(model.fit_ex_roll22.2@forecast$VaR[, 1])
model_rets_ex_22.2 = zoo(model.fit_ex_roll22.2@forecast$VaR[, 2])

plot(model_rets_ex_22.2, type = "b", main = "99%VaR Backtesting/ with sent_delta eGarch-std", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR_ex_22.2, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: sGARCH, ged------------------------------------------------------------------
model.fit_ex_roll31.2 = ugarchroll(model.spec_ex31.2, rets, n.start = 522, refit.every = 1, 
                                    refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                                    VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_ex_roll31.2, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_ex_roll31.2, type = "fpm")

#Plot VaR performance
model_VaR_ex_31.2 = zoo(model.fit_ex_roll31.2@forecast$VaR[, 1])
model_rets_ex_31.2 = zoo(model.fit_ex_roll31.2@forecast$VaR[, 2])

plot(model_rets_ex_31.2, type = "b", main = "99%VaR Backtesting/ with sent_delta sGarch-ged", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR_ex_31.2, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

###Backtest: eGARCH, ged------------------------------------------------------------------
model.fit_ex_roll32.2 = ugarchroll(model.spec_ex32.2, rets, n.start = 522, refit.every = 1, 
                                    refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, 
                                    VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
#Kupiec's Unconditional Coverage and the Christoffersen Test
report(model.fit_ex_roll32.2, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(model.fit_ex_roll32.2, type = "fpm")

#Plot VaR performance
model_VaR_ex_32.2 = zoo(model.fit_ex_roll32.2@forecast$VaR[, 1])
model_rets_ex_32.2 = zoo(model.fit_ex_roll32.2@forecast$VaR[, 2])

plot(model_rets_ex_32.2, type = "b", main = "99%VaR Backtesting/ with sent_delta eGarch-ged", xlab = "Date", ylab = "Return/VaR in percent")
lines(model_VaR_ex_32.2, col = "red")
legend("topleft", inset=.05, c("FX return","VaR"), col = c("black","red"), lty = c(1,1))

######################################################################################################
######################################### end of code ################################################
######################################################################################################