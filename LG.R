library(readxl)
library(vars)
library(forecast)
library(corrplot)
library(mFilter)

(files <- list.files(path = 'C:/Users/edgaras/Desktop/LG', 
                     pattern = '*.xlsx', full.names = T))

PLANT_df <- read_excel(files[1])[-1]
MINERAL_df <- read_excel(files[2])[-1]
FOOD_df <- read_excel(files[3])[-1]
WOOD_df <- read_excel(files[4])[-1]
OIL_df <- read_excel(files[5])[-1]

########## 1. Exploratory analysis ##########

##### 1.1 PLANT - 'Augalines kilmes produktai.xlsx' (Plant) #####

### 1.1.1 PLANT as indicator ###

PLANT <- ts(PLANT_df[, 'Total'], start = c(2009, 1), frequency = 12)

plant <- window(PLANT, end = c(2018, 12))
ts.plot(plant, xlab = '', ylab = 'Products of plant origin in total',
        main = 'Dynamics of products of plant origin')

acf(plant)

### 1.1.2 PLANT as growth rate ###

d_plant <- diff(log(plant)) * 100
ts.plot(d_plant, xlab = '', ylab = 'Change in products of plant origin, %',
        main = 'Growth rate of products of plant origin')

acf(d_plant)

##### 1.2 MINERAL - 'Kietasis mineralinis kuras.xlsx' #####

### 1.2.1 MINERAL as indicator ###

MINERAL <- ts(MINERAL_df[, 'Total'], start = c(2009, 1), frequency = 12)

mineral <- window(MINERAL, end = c(2018, 12))
ts.plot(mineral, xlab = '', ylab = 'Solid mineral fuel in total',
        main = 'Dynamics of solid mineral fuel')

acf(mineral)

### 1.2.2 MINERAL as growth rate ###

d_mineral <- diff(log(mineral)) * 100
ts.plot(d_mineral, xlab = '', ylab = 'Change in solid mineral fuel, %',
        main = 'Growth rate of solid mineral fuel')

acf(d_mineral)

##### 1.3 FOOD - 'Maisto pramones produktai.xlsx' #####

### 1.3.1 FOOD as indicator ###

FOOD <- ts(FOOD_df[, 'Total'], start = c(2009, 1), frequency = 12)

food <- window(FOOD, end = c(2018, 12))
ts.plot(food, xlab = '', ylab = 'Food industry products in total',
        main = 'Dynamics of food industry products')

acf(food)

### 1.3.2 FOOD as growth rate ###

d_food <- diff(log(food)) * 100
ts.plot(d_food, xlab = '', ylab = 'Change in food industry products, %',
        main = 'Growth rate of food industry products')

acf(d_food)

##### 1.4 WOOD - 'Mediena, kamstiena.xlsx' #####

### 1.4.1 WOOD as indicator ###

WOOD <- ts(WOOD_df[, 'Total'], start = c(2009, 1), frequency = 12)

wood <- window(WOOD, end = c(2018, 12))
ts.plot(wood, xlab = '', ylab = 'Wood & cork in total',
        main = 'Dynamics of wood & cork')

acf(wood)

### 1.4.2 WOOD as growth rate ###

d_wood <- diff(log(wood)) * 100
ts.plot(d_wood, xlab = '', ylab = 'Change in wood & cork, %',
        main = 'Growth rate of wood & cork')

acf(d_wood)

##### 1.5 OIL - 'Nafta ir naftos produktai.xlsx' #####

### 1.5.1 OIL as indicator ###

OIL <- ts(OIL_df[, 'Total'], start = c(2009, 1), frequency = 12)

oil <- window(OIL, end = c(2018, 12))
ts.plot(oil, xlab = '', ylab = 'Oil and petroleum products in total',
        main = 'Dynamics of oil and petroleum products')

acf(oil)

### 1.5.2 OIL as growth rate ###

d_oil <- diff(log(oil)) * 100
ts.plot(d_oil, xlab = '', ylab = 'Change in oil and petroleum products, %',
        main = 'Growth rate of oil and petroleum products')

acf(d_oil)

########## 2. Unit root tests ##########

##### 2.1 plant - 'Augalines kilmes produktai.xlsx' #####

test_plant <- ur.df(plant, type = 'drift', lags = 12)
summary(test_plant)

# Since Dickey-Fuller test statistic is t = -1.4018 > -2.88 (critical value), we
# can not reject the null hypothesis.

test_d_plant <- ur.df(d_plant, type = 'drift', lags = 12)
summary(test_d_plant)

# Since Dickey-Fuller test statistic is t = -4.5238 < -2.88 (critical value), we
# can reject the null hypothesis. Thus, the process is I(1).

##### 2.2 mineral - 'Kietasis mineralinis kuras.xlsx' #####

test_mineral <- ur.df(mineral, type = 'drift', lags = 12)
summary(test_mineral)

# Since Dickey-Fuller test statistic is t = 0.1106 > -2.88 (critical value), we
# can not reject the null hypothesis.

test_d_mineral <- ur.df(d_mineral, type = 'drift', lags = 12)
summary(test_d_mineral)

# Since Dickey-Fuller test statistic is t = -3.1004 < -2.88 (critical value), we
# can reject the null hypothesis. Thus, the process is I(1).

##### 2.3 food - 'Maisto pramones produktai.xlsx' #####

test_food <- ur.df(food, type = 'drift', lags = 12)
summary(test_food)

# Since Dickey-Fuller test statistic is t = -2.4974 > -2.88 (critical value), we
# can not reject the null hypothesis.

test_d_food <- ur.df(d_food, type = 'drift', lags = 12)
summary(test_d_food)

# Since Dickey-Fuller test statistic is t = -3.6578 < -2.88 (critical value), we
# can reject the null hypothesis. Thus, the process is I(1).

##### 2.4 wood - 'Mediena, kamstiena.xlsx' #####

test_wood <- ur.df(wood, type = 'drift', lags = 12)
summary(test_wood)

# Since Dickey-Fuller test statistic is t = -1.1621 > -2.88 (critical value), we
# can not reject the null hypothesis.

test_d_wood <- ur.df(d_wood, type = 'drift', lags = 12)
summary(test_d_wood)

# Since Dickey-Fuller test statistic is t = -3.4473 < -2.88 (critical value), we
# can reject the null hypothesis. Thus, the process is I(1).

##### 2.5 oil - 'Nafta ir naftos produktai.xlsx' #####

test_oil <- ur.df(oil, type = 'drift', lags = 12)
summary(test_oil)

# Since Dickey-Fuller test statistic is t = -1.6578 > -2.88 (critical value), we
# can not reject the null hypothesis.

test_d_oil <- ur.df(d_oil, type = 'drift', lags = 12)
summary(test_d_oil)

# Since Dickey-Fuller test statistic is t = -3.7036 < -2.88 (critical value), we
# can reject the null hypothesis. Thus, the process is I(1).

# Since all of variables are integrated of order 1, they can be co-integrated.

########## 3. Modelling ##########

##### 3.1 Univariate models: ARIMA #####

### 3.1.1 ARIMA for PLANT ###

# 3.1.1.1 Model building

arma.plant <- arima(diff(log(plant)), 
                    order = c(12, 0, 6), 
                    fixed = c(NA, rep(0, 2), NA, rep(0, 7), NA,
                              rep(0, 5), NA,
                              NA))
coeftest(arma.plant)

# This model includes 1st, 4th & 12th lag of AR (autoregressive) and 6th lag of MA (moving average).

# 3.1.1.2 Residuals

acf(residuals(arma.plant))

# Autocorrelation function plot showed that model residuals are not autocorrelated.

# 3.1.1.3 Predictions

first.obs.plant <- c(head(plant, 1))
last.obs.plant <- c(tail(plant, 1))

fitted.arma.plant <- first.obs.plant * cumprod(exp(fitted(arma.plant)))
pred.rates.arma.plant <- predict(arma.plant, n.ahead = 12)$pred
pred.val.arma.plant <- last.obs.plant * cumprod(exp(pred.rates.arma.plant))

pred.val.arma.plant.ts <- ts(c(first.obs.plant, 
                               fitted.arma.plant, 
                               pred.val.arma.plant), 
                             start = c(2009, 1), frequency = 12)
fcst.val.arma.plant.ts <- ts(c(as.matrix(last.obs.plant), 
                               tail(pred.val.arma.plant, 12)),
                             start = c(2018, 12), frequency = 12)

# 3.1.1.4 Plots

# Plot of PLANT (observed, fitted & predicted). 

ts.plot(ts(plant, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Products of plant origin in total', 
        main = 'Products of plant origin',
        ylim = c(0, max(plant) + 0.33 * max(plant)),
        xlim = c(2009, 2020))
lines(pred.val.arma.plant.ts, col = 'red', lty = 2)
legend(2008.75, max(plant) + 0.33 * max(plant), 
       legend = c('Observed', 'ARMA(p = 1, 4, 12, q = 6)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# Plot of PLANT (observed & predicted). 

ts.plot(ts(plant, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Products of plant origin in total', 
        main = 'Products of plant origin',
        ylim = c(0, max(plant) + 0.33 * max(plant)),
        xlim = c(2009, 2020))
lines(fcst.val.arma.plant.ts, col = 'red', lty = 2)
legend(2008.75, max(plant) + 0.33 * max(plant), 
       legend = c('Observed', 'ARMA(p = 1, 4, 12, q = 6)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# 3.1.1.5 Accuracy

# ARMA model accuracy for 2018:

# Monthly

(acc.arma.plant <- accuracy(pred.val.arma.plant.ts[109:120], 
                            as.matrix(plant[109:120])))

# Quarterly

real.q.plant <- matrix(c(plant[109:120]),
                       ncol = 4, byrow = FALSE)
pred.q.plant.arma <- matrix(c(pred.val.arma.plant.ts[109:120]), 
                            ncol = 4, byrow = FALSE)
(acc.q.arma.plant <- accuracy(apply(pred.q.plant.arma, 2, sum), 
                              apply(real.q.plant, 2, sum)))

# Yearly

(acc.y.arma.plant <- accuracy(sum(pred.val.arma.plant.ts[109:120]), 
                              sum(plant[109:120])))

### 3.1.2 ARIMA for MINERAL ###

# 3.1.2.1 Model building

arma.mineral <- arima(diff(log(mineral)), 
                      order = c(6, 0, 0), 
                      fixed = c(NA, rep(0, 4), NA,
                                NA))
coeftest(arma.mineral)

# This model includes only 1st & 6th lag of AR (autoregressive).

# 3.1.2.2 Residuals

acf(residuals(arma.mineral))

# Autocorrelation function plot showed that model residuals are not autocorrelated.

# 3.1.2.3 Predictions

first.obs.mineral <- c(head(mineral, 1))
last.obs.mineral <- c(tail(mineral, 1))

fitted.arma.mineral <- first.obs.mineral * cumprod(exp(fitted(arma.mineral)))
pred.rates.arma.mineral <- predict(arma.mineral, n.ahead = 12)$pred
pred.val.arma.mineral <- last.obs.mineral * cumprod(exp(pred.rates.arma.mineral))

pred.val.arma.mineral.ts <- ts(c(first.obs.mineral, 
                                 fitted.arma.mineral, 
                                 pred.val.arma.mineral), 
                               start = c(2009, 1), frequency = 12)
fcst.val.arma.mineral.ts <- ts(c(as.matrix(last.obs.mineral), 
                                 tail(pred.val.arma.mineral, 12)),
                               start = c(2018, 12), frequency = 12)

# 3.1.2.4 Plots

# Plot of MINERAL (observed, fitted & predicted). 

ts.plot(ts(mineral, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Solid mineral fuel in total', 
        main = 'Solid mineral fuel',
        ylim = c(0, max(mineral) + 0.33 * max(mineral)),
        xlim = c(2009, 2020))
lines(pred.val.arma.mineral.ts, col = 'red', lty = 2)
legend(2008.75, max(mineral) + 0.33 * max(mineral), 
       legend = c('Observed', 'AR(p = 1, 6)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# Plot of MINERAL (observed & predicted). 

ts.plot(ts(mineral, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Solid mineral fuel in total', 
        main = 'Solid mineral fuel',
        ylim = c(0, max(mineral) + 0.33 * max(mineral)),
        xlim = c(2009, 2020))
lines(fcst.val.arma.mineral.ts, col = 'red', lty = 2)
legend(2008.75, max(mineral) + 0.33 * max(mineral), 
       legend = c('Observed', 'AR(p = 1, 6)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# 3.1.2.5 Accuracy

# ARMA model accuracy for 2018:

# Monthly

(acc.arma.mineral <- accuracy(pred.val.arma.mineral.ts[109:120], 
                              as.matrix(mineral[109:120])))

# Quarterly

real.q.mineral <- matrix(c(mineral[109:120]),
                         ncol = 4, byrow = FALSE)
pred.q.mineral.arma <- matrix(c(pred.val.arma.mineral.ts[109:120]), 
                              ncol = 4, byrow = FALSE)
(acc.q.arma.mineral <- accuracy(apply(pred.q.mineral.arma, 2, sum), 
                                apply(real.q.mineral, 2, sum)))

# Yearly

(acc.y.arma.mineral <- accuracy(sum(pred.val.arma.mineral.ts[109:120]), 
                                sum(mineral[109:120])))

### 3.1.3 ARIMA for FOOD ###

# 3.1.3.1 Model building

arma.food <- arima(diff(log(food)), 
                   order = c(12, 0, 4), 
                   fixed = c(NA, 0, NA, rep(0, 5), NA, rep(0, 2), NA,
                             rep(0, 3), NA,
                             NA))
coeftest(arma.food)

# This model includes 1st, 3rd, 9th & 12th lag of AR (autoregressive) and 4th lag of MA (moving average).

# 3.1.3.2 Residuals

acf(residuals(arma.food))

# Autocorrelation function plot showed that model residuals are not autocorrelated.

# 3.1.3.3 Predictions

first.obs.food <- c(head(food, 1))
last.obs.food <- c(tail(food, 1))

fitted.arma.food <- first.obs.food * cumprod(exp(fitted(arma.food)))
pred.rates.arma.food <- predict(arma.food, n.ahead = 12)$pred
pred.val.arma.food <- last.obs.food * cumprod(exp(pred.rates.arma.food))

pred.val.arma.food.ts <- ts(c(first.obs.food, 
                              fitted.arma.food, 
                              pred.val.arma.food), 
                            start = c(2009, 1), frequency = 12)
fcst.val.arma.food.ts <- ts(c(as.matrix(last.obs.food), 
                              tail(pred.val.arma.food, 12)),
                            start = c(2018, 12), frequency = 12)

# 3.1.3.4 Plots

# Plot of FOOD (observed, fitted & predicted). 

ts.plot(ts(food, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Food industry products in total', 
        main = 'Food industry products',
        ylim = c(0, max(food) + 0.33 * max(food)),
        xlim = c(2009, 2020))
lines(pred.val.arma.food.ts, col = 'red', lty = 2)
legend(2008.75, max(food) + 0.33 * max(food), 
       legend = c('Observed', 'AR(p = 1, 3, 9, 12, q = 4)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# Plot of MPP (observed & predicted). 

ts.plot(ts(food, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Food industry products in total', 
        main = 'Food industry products',
        ylim = c(0, max(food) + 0.33 * max(food)),
        xlim = c(2009, 2020))
lines(fcst.val.arma.food.ts, col = 'red', lty = 2)
legend(2008.75, max(food) + 0.33 * max(food), 
       legend = c('Observed', 'AR(p = 1, 3, 9, 12, q = 4)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# 3.1.3.5 Accuracy

# ARMA model accuracy for 2018:

# Monthly

(acc.arma.food <- accuracy(pred.val.arma.food.ts[109:120], 
                           as.matrix(food[109:120])))

# Quarterly

real.q.food <- matrix(c(food[109:120]),
                      ncol = 4, byrow = FALSE)
pred.q.food.arma <- matrix(c(pred.val.arma.food.ts[109:120]), 
                           ncol = 4, byrow = FALSE)
(acc.q.arma.food <- accuracy(apply(pred.q.food.arma, 2, sum), 
                             apply(real.q.food, 2, sum)))

# Yearly

(acc.y.arma.food <- accuracy(sum(pred.val.arma.food.ts[109:120]), 
                             sum(food[109:120])))

### 3.1.4 ARIMA for WOOD ###

# 3.1.4.1 Model building

arma.wood <- arima(diff(log(wood)), 
                   order = c(12, 0, 0), 
                   fixed = c(NA, rep(0, 2), NA, rep(0, 4), NA, rep(0, 2), NA,
                             NA))
coeftest(arma.wood)

# This model includes 1st, 4th, 9th & 12th lag of AR (autoregressive).

# 3.1.4.2 Residuals

acf(residuals(arma.wood))

# Autocorrelation function plot showed that model residuals are not autocorrelated.

# 3.1.4.3 Predictions

first.obs.wood <- c(head(wood, 1))
last.obs.wood <- c(tail(wood, 1))

fitted.arma.wood <- first.obs.wood * cumprod(exp(fitted(arma.wood)))
pred.rates.arma.wood <- predict(arma.wood, n.ahead = 12)$pred
pred.val.arma.wood <- last.obs.wood * cumprod(exp(pred.rates.arma.wood))

pred.val.arma.wood.ts <- ts(c(first.obs.wood, 
                              fitted.arma.wood, 
                              pred.val.arma.wood), 
                            start = c(2009, 1), frequency = 12)
fcst.val.arma.wood.ts <- ts(c(as.matrix(last.obs.wood), 
                              tail(pred.val.arma.wood, 12)),
                            start = c(2018, 12), frequency = 12)

# 3.1.4.4 Plots

# Plot of WOOD (observed, fitted & predicted). 

ts.plot(ts(wood, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Wood & cork in total', 
        main = 'Wood & cork',
        ylim = c(0, max(wood) + 0.33 * max(wood)),
        xlim = c(2009, 2020))
lines(pred.val.arma.wood.ts, col = 'red', lty = 2)
legend(2008.75, max(wood) + 0.33 * max(wood), 
       legend = c('Observed', 'AR(p = 1, 4, 9, 12)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# Plot of WOOD (observed & predicted). 

ts.plot(ts(wood, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Wood & cork in total', 
        main = 'Wood & cork',
        ylim = c(0, max(wood) + 0.33 * max(wood)),
        xlim = c(2009, 2020))
lines(fcst.val.arma.wood.ts, col = 'red', lty = 2)
legend(2008.75, max(wood) + 0.33 * max(wood), 
       legend = c('Observed', 'AR(p = 1, 4, 9, 12)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# 3.1.4.5 Accuracy

# ARMA model accuracy for 2018:

# Monthly

(acc.arma.wood <- accuracy(pred.val.arma.wood.ts[109:120], 
                           as.matrix(wood[109:120])))

# Quarterly

real.q.wood <- matrix(c(wood[109:120]),
                      ncol = 4, byrow = FALSE)
pred.q.wood.arma <- matrix(c(pred.val.arma.wood.ts[109:120]), 
                           ncol = 4, byrow = FALSE)
(acc.q.arma.wood <- accuracy(apply(pred.q.wood.arma, 2, sum), 
                             apply(real.q.wood, 2, sum)))

# Yearly

(acc.y.arma.wood <- accuracy(sum(pred.val.arma.wood.ts[109:120]), 
                             sum(wood[109:120])))

### 3.1.5 ARIMA for OIL ###

# 3.1.5.1 Model building

arma.oil <- arima(diff(log(oil)), 
                  order = c(12, 0, 1), 
                  fixed = c(rep(0, 8), NA, NA, 0, NA,
                            NA,
                            NA))
coeftest(arma.oil)

# This model includes 9th, 10th & 12th lag of AR (autoregressive) and 1st lag of MA (moving average).

# 3.1.5.2 Residuals

acf(residuals(arma.oil))

# Autocorrelation function plot showed that model residuals are not autocorrelated.

# 3.1.5.3 Predictions

first.obs.oil <- c(head(oil, 1))
last.obs.oil <- c(tail(oil, 1))

fitted.arma.oil <- first.obs.oil * cumprod(exp(fitted(arma.oil)))
pred.rates.arma.oil <- predict(arma.oil, n.ahead = 12)$pred
pred.val.arma.oil <- last.obs.oil * cumprod(exp(pred.rates.arma.oil))

pred.val.arma.oil.ts <- ts(c(first.obs.oil, 
                             fitted.arma.oil, 
                             pred.val.arma.oil), 
                           start = c(2009, 1), frequency = 12)
fcst.val.arma.oil.ts <- ts(c(as.matrix(last.obs.oil), 
                             tail(pred.val.arma.oil, 12)),
                           start = c(2018, 12), frequency = 12)

# 3.1.5.4 Plots

# Plot of OIL (observed, fitted & predicted). 

ts.plot(ts(oil, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Oil and petroleum products in total', 
        main = 'Oil and petroleum products',
        ylim = c(0, max(oil) + 0.33 * max(oil)),
        xlim = c(2009, 2020))
lines(pred.val.arma.oil.ts, col = 'red', lty = 2)
legend(2008.75, max(oil) + 0.33 * max(oil), 
       legend = c('Observed', 'AR(p = 9, 10, 12, q = 1)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# Plot of OIL (observed & predicted). 

ts.plot(ts(oil, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Oil and petroleum products in total', 
        main = 'Oil and petroleum products',
        ylim = c(0, max(oil) + 0.33 * max(oil)),
        xlim = c(2009, 2020))
lines(fcst.val.arma.oil.ts, col = 'red', lty = 2)
legend(2008.75, max(oil) + 0.33 * max(oil), 
       legend = c('Observed', 'AR(p = 9, 10, 12, q = 1)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# 3.1.5.5 Accuracy

# ARMA model accuracy for 2018:

# Monthly

(acc.arma.oil <- accuracy(pred.val.arma.oil.ts[109:120], 
                          as.matrix(oil[109:120])))

# Quarterly

real.q.oil <- matrix(c(oil[109:120]),
                     ncol = 4, byrow = FALSE)
pred.q.oil.arma <- matrix(c(pred.val.arma.oil.ts[109:120]), 
                          ncol = 4, byrow = FALSE)
(acc.q.arma.oil <- accuracy(apply(pred.q.oil.arma, 2, sum), 
                            apply(real.q.oil, 2, sum)))

# Yearly

(acc.y.arma.oil <- accuracy(sum(pred.val.arma.oil.ts[109:120]), 
                            sum(oil[109:120])))

##### 3.2 Multivariate models: VAR #####

# 3.2.1 Model building

data.var <- cbind(plant = diff(log(plant)), 
                  mineral = diff(log(mineral)),
                  food = diff(log(food)),
                  wood = diff(log(wood)),
                  oil = diff(log(oil)))
plot.ts(data.var)
acf(data.var)

# 2nd & 3th lags significant in case of akp and mpp, 1st lag significant in case of kmk, mk and nnp.
# Furthermore, in order to underline seasonality, 4 lags of variables were included in model equations.

var.model <- VAR(data.var, p = 4)
summary(var.model)

# 3.2.2 Residuals

acf(residuals(var.model))

# Autocorrelation function plot showed that model residuals are not autocorrelated.

# 3.2.3 Predictions

pred.rates.var <- predict(var.model, n.ahead = 12)
fitted.var <- fitted(var.model)

# 3.2.3.1 PLANT

first.obs.var.plant <- c(head(plant, 5))
fitted.var.plant <- first.obs.var.plant[5] * cumprod(exp(fitted.var[, 1]))

pred.rates.var.plant <- pred.rates.var$fcst$plant[, 1]
pred.val.var.plant <- last.obs.plant * cumprod(exp(pred.rates.var.plant))

pred.val.var.plant.ts <- ts(c(first.obs.var.plant, 
                              fitted.var.plant, 
                              pred.val.var.plant), 
                            start = c(2009, 1), frequency = 12)
fcst.val.var.plant.ts <- ts(c(as.matrix(last.obs.plant), 
                              tail(pred.val.var.plant, 12)),
                            start = c(2018, 12), frequency = 12)

# 3.2.3.2 MINERAL

first.obs.var.mineral <- c(head(mineral, 5))
fitted.var.mineral <- first.obs.var.mineral[5] * cumprod(exp(fitted.var[, 2]))

pred.rates.var.mineral <- pred.rates.var$fcst$mineral[, 1]
pred.val.var.mineral <- last.obs.mineral * cumprod(exp(pred.rates.var.mineral))

pred.val.var.mineral.ts <- ts(c(first.obs.var.mineral, 
                                fitted.var.mineral, 
                                pred.val.var.mineral), 
                              start = c(2009, 1), frequency = 12)
fcst.val.var.mineral.ts <- ts(c(as.matrix(last.obs.mineral), 
                                tail(pred.val.var.mineral, 12)),
                              start = c(2018, 12), frequency = 12)

# 3.2.3.3 FOOD

first.obs.var.food <- c(head(food, 5))
fitted.var.food <- first.obs.var.food[5] * cumprod(exp(fitted.var[, 3]))

pred.rates.var.food <- pred.rates.var$fcst$food[, 1]
pred.val.var.food <- last.obs.food * cumprod(exp(pred.rates.var.food))

pred.val.var.food.ts <- ts(c(first.obs.var.food, 
                             fitted.var.food, 
                             pred.val.var.food), 
                           start = c(2009, 1), frequency = 12)
fcst.val.var.food.ts <- ts(c(as.matrix(last.obs.food), 
                             tail(pred.val.var.food, 12)),
                           start = c(2018, 12), frequency = 12)

# 3.2.3.4 WOOD

first.obs.var.wood <- c(head(wood, 5))
fitted.var.wood <- first.obs.var.wood[5] * cumprod(exp(fitted.var[, 4]))

pred.rates.var.wood <- pred.rates.var$fcst$wood[, 1]
pred.val.var.wood <- last.obs.wood * cumprod(exp(pred.rates.var.wood))

pred.val.var.wood.ts <- ts(c(first.obs.var.wood, 
                             fitted.var.wood, 
                             pred.val.var.wood), 
                           start = c(2009, 1), frequency = 12)
fcst.val.var.wood.ts <- ts(c(as.matrix(last.obs.wood), 
                             tail(pred.val.var.wood, 12)),
                           start = c(2018, 12), frequency = 12)

# 3.2.3.5 OIL

first.obs.var.oil <- c(head(oil, 5))
fitted.var.oil <- first.obs.var.oil[5] * cumprod(exp(fitted.var[, 5]))

pred.rates.var.oil <- pred.rates.var$fcst$oil[, 1]
pred.val.var.oil <- last.obs.oil * cumprod(exp(pred.rates.var.oil))

pred.val.var.oil.ts <- ts(c(first.obs.var.oil, 
                            fitted.var.oil, 
                            pred.val.var.oil), 
                          start = c(2009, 1), frequency = 12)
fcst.val.var.oil.ts <- ts(c(as.matrix(last.obs.oil), 
                            tail(pred.val.var.oil, 12)),
                          start = c(2018, 12), frequency = 12)

# 3.2.4 Plots

# 3.2.4.1 PLANT

# Plot of PLANT (observed, fitted & predicted). 

ts.plot(ts(plant, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Products of plant origin in total', 
        main = 'Products of plant origin',
        ylim = c(0, max(plant) + 0.33 * max(plant)),
        xlim = c(2009, 2020))
lines(pred.val.var.plant.ts, col = 'red', lty = 2)
legend(2008.75, max(plant) + 0.33 * max(plant), 
       legend = c('Observed', 'VAR(p = 4)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# Plot of PLANT (observed & predicted). 

ts.plot(ts(plant, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Products of plant origin in total', 
        main = 'Products of plant origin',
        ylim = c(0, max(plant) + 0.33 * max(plant)),
        xlim = c(2009, 2020))
lines(fcst.val.var.plant.ts, col = 'red', lty = 2)
legend(2008.75, max(plant) + 0.33 * max(akp), 
       legend = c('Observed', 'VAR(p = 4)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# 3.2.4.2 MINERAL

# Plot of MINERAL (observed, fitted & predicted). 

ts.plot(ts(mineral, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Solid mineral fuel in total', 
        main = 'Solid mineral fuel',
        ylim = c(0, max(mineral) + 0.33 * max(mineral)),
        xlim = c(2009, 2020))
lines(pred.val.var.mineral.ts, col = 'red', lty = 2)
legend(2008.75, max(mineral) + 0.33 * max(mineral), 
       legend = c('Observed', 'VAR(p = 4)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# Plot of MINERAL (observed & predicted). 

ts.plot(ts(mineral, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Solid mineral fuel in total', 
        main = 'Solid mineral fuel',
        ylim = c(0, max(mineral) + 0.33 * max(mineral)),
        xlim = c(2009, 2020))
lines(fcst.val.var.mineral.ts, col = 'red', lty = 2)
legend(2008.75, max(mineral) + 0.33 * max(mineral), 
       legend = c('Observed', 'VAR(p = 4)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# 3.2.4.3 FOOD

# Plot of FOOD (observed, fitted & predicted). 

ts.plot(ts(food, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Food industry products in total', 
        main = 'Food industry products',
        ylim = c(0, max(food) + 0.33 * max(food)),
        xlim = c(2009, 2020))
lines(pred.val.var.food.ts, col = 'red', lty = 2)
legend(2008.75, max(food) + 0.33 * max(food), 
       legend = c('Observed', 'VAR(p = 4)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# Plot of FOOD (observed & predicted). 

ts.plot(ts(food, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Food industry products in total', 
        main = 'Food industry products',
        ylim = c(0, max(food) + 0.33 * max(food)),
        xlim = c(2009, 2020))
lines(fcst.val.var.food.ts, col = 'red', lty = 2)
legend(2008.75, max(food) + 0.33 * max(food), 
       legend = c('Observed', 'VAR(p = 4)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# 3.2.4.4 WOOD

# Plot of WOOD (observed, fitted & predicted). 

ts.plot(ts(wood, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Wood & cork in total', 
        main = 'Wood & cork',
        ylim = c(0, max(wood) + 0.33 * max(wood)),
        xlim = c(2009, 2020))
lines(pred.val.var.wood.ts, col = 'red', lty = 2)
legend(2008.75, max(wood) + 0.33 * max(wood), 
       legend = c('Observed', 'VAR(p = 4)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# Plot of WOOD (observed & predicted). 

ts.plot(ts(wood, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Wood & cork in total', 
        main = 'Wood & cork',
        ylim = c(0, max(wood) + 0.33 * max(wood)),
        xlim = c(2009, 2020))
lines(fcst.val.var.wood.ts, col = 'red', lty = 2)
legend(2008.75, max(wood) + 0.33 * max(wood), 
       legend = c('Observed', 'VAR(p = 4)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# 3.2.4.5 OIL

# Plot of OIL (observed, fitted & predicted). 

ts.plot(ts(oil, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Oil and petroleum products in total', 
        main = 'Oil and petroleum products',
        ylim = c(0, max(oil) + 0.33 * max(oil)),
        xlim = c(2009, 2020))
lines(pred.val.var.oil.ts, col = 'red', lty = 2)
legend(2008.75, max(oil) + 0.33 * max(oil), 
       legend = c('Observed', 'VAR(p = 4)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# Plot of OIL (observed & predicted). 

ts.plot(ts(oil, start = c(2009, 1), frequency = 12), 
        col = 'blue', 
        xlab = '', ylab = 'Oil and petroleum products in total', 
        main = 'Oil and petroleum products',
        ylim = c(0, max(oil) + 0.33 * max(oil)),
        xlim = c(2009, 2020))
lines(fcst.val.var.oil.ts, col = 'red', lty = 2)
legend(2008.75, max(oil) + 0.33 * max(oil), 
       legend = c('Observed', 'VAR(p = 4)'),
       col = c('blue', 'red'), lty = 1:2, box.lty = 0)

# 3.2.5 Accuracy

# 3.2.5.1 PLANT

# Monthly

(acc.var.plant <- accuracy(pred.val.var.plant.ts[109:120], 
                           as.matrix(plant[109:120])))

# Quarterly

real.q.plant <- matrix(c(plant[109:120]),
                       ncol = 4, byrow = FALSE)
pred.q.plant.var <- matrix(c(pred.val.var.plant.ts[109:120]), 
                           ncol = 4, byrow = FALSE)
(acc.q.var.plant <- accuracy(apply(pred.q.plant.var, 2, sum), 
                             apply(real.q.plant, 2, sum)))

# Yearly

(acc.y.var.plant <- accuracy(sum(pred.val.var.plant.ts[109:120]), 
                             sum(plant[109:120])))

# 3.2.5.2 MINERAL

# Monthly

(acc.var.mineral <- accuracy(pred.val.var.mineral.ts[109:120], 
                             as.matrix(mineral[109:120])))

# Quarterly

real.q.mineral <- matrix(c(mineral[109:120]),
                         ncol = 4, byrow = FALSE)
pred.q.mineral.var <- matrix(c(pred.val.var.mineral.ts[109:120]), 
                             ncol = 4, byrow = FALSE)
(acc.q.var.mineral <- accuracy(apply(pred.q.mineral.var, 2, sum), 
                               apply(real.q.mineral, 2, sum)))

# Yearly

(acc.y.var.mineral <- accuracy(sum(pred.val.var.mineral.ts[109:120]), 
                               sum(mineral[109:120])))

# 3.2.5.3 FOOD

# Monthly

(acc.var.food <- accuracy(pred.val.var.food.ts[109:120], 
                          as.matrix(food[109:120])))

# Quarterly

real.q.food <- matrix(c(food[109:120]),
                      ncol = 4, byrow = FALSE)
pred.q.food.var <- matrix(c(pred.val.var.food.ts[109:120]), 
                          ncol = 4, byrow = FALSE)
(acc.q.var.food <- accuracy(apply(pred.q.food.var, 2, sum), 
                            apply(real.q.food, 2, sum)))

# Yearly

(acc.y.var.food <- accuracy(sum(pred.val.var.food.ts[109:120]), 
                            sum(food[109:120])))

# 3.2.5.4 WOOD

# Monthly

(acc.var.wood <- accuracy(pred.val.var.wood.ts[109:120], 
                          as.matrix(wood[109:120])))

# Quarterly

real.q.wood <- matrix(c(wood[109:120]),
                      ncol = 4, byrow = FALSE)
pred.q.wood.var <- matrix(c(pred.val.var.wood.ts[109:120]), 
                          ncol = 4, byrow = FALSE)
(acc.q.var.wood <- accuracy(apply(pred.q.wood.var, 2, sum), 
                            apply(real.q.wood, 2, sum)))

# Yearly

(acc.y.var.wood <- accuracy(sum(pred.val.var.wood.ts[109:120]), 
                            sum(wood[109:120])))

# 3.2.5.5 OIL

# Monthly

(acc.var.oil <- accuracy(pred.val.var.oil.ts[109:120], 
                         as.matrix(oil[109:120])))

# Quarterly

real.q.oil <- matrix(c(oil[109:120]),
                     ncol = 4, byrow = FALSE)
pred.q.oil.var <- matrix(c(pred.val.var.oil.ts[109:120]), 
                         ncol = 4, byrow = FALSE)
(acc.q.var.oil <- accuracy(apply(pred.q.oil.var, 2, sum), 
                           apply(real.q.oil, 2, sum)))

# Yearly

(acc.y.var.oil <- accuracy(sum(pred.val.var.oil.ts[109:120]), 
                           sum(oil[109:120])))

# 3.2.6 IRF (impulse response function) plots

plot(irf(var.model))

# Impulse response from PLANT: there is no significant response in other variables when system gets PLANT shock;
# Impulse response from MINERAL: PLANT rises by 10% for 2 period when system gets MINERAL shock;
# Impulse response from FOOD: there is no significant response in other variables when system gets FOOD shock;
# Impulse response from OIL: PLANT probably falls by 5% for 2 period when system gets OIL shock;

# 3.2.6 FEVD (forecast error variance decomposition)

fevd(var.model)
plot(fevd(var.model))

# In all cases, the proportions of changes in the variable due to their own shocks are approx. 95% in the 1st period and at least 70% in the 10th period.

########## 4. Additional: correlation analysis ##########

##### 4.1 Correlation between the variables using in modelling #####

### 4.1.1 Pearson correlation between growth rates of the variables

plant.rate <- c(diff(as.matrix(log(plant)))) * 100
mineral.rate <- c(diff(as.matrix(log(mineral)))) * 100
food.rate <- c(diff(as.matrix(log(food)))) * 100
wood.rate <- c(diff(as.matrix(log(wood)))) * 100
oil.rate <- c(diff(as.matrix(log(oil)))) * 100

cor.rates <- data.frame(plant.rate, mineral.rate,
                        food.rate, wood.rate, oil.rate)
colnames(cor.rates) <- c('Plant', 'Mineral', 'Food', 'Wood', 'Oil')
corrplot(cor(cor.rates), method = 'number', tl.srt = 30)

### 4.2.1 Pearson correlation between cycle components of the variables

plant.cycle <- hpfilter(log(plant), freq = 14400)$cycle * 100
mineral.cycle <- hpfilter(log(mineral), freq = 14400)$cycle * 100
food.cycle <- hpfilter(log(food), freq = 14400)$cycle * 100
wood.cycle <- hpfilter(log(wood), freq = 14400)$cycle * 100
oil.cycle <- hpfilter(log(oil), freq = 14400)$cycle * 100

cor.cycle <- cbind(plant.cycle, mineral.cycle,
                   food.cycle, wood.cycle, oil.cycle)
colnames(cor.cycle) <- c('Plant', 'Mineral', 'Food', 'Wood', 'Oil')
corrplot(cor(cor.cycle), method = 'number', tl.srt = 30)

###### Table 1: ARIMA, quarterly

(acc.q.mat.arma <- cbind(Plant = c(round(acc.q.arma.plant[1], 1), 
                                   round(acc.q.arma.plant[2], 1), 
                                   round(acc.q.arma.plant[3], 1), 
                                   round(acc.q.arma.plant[4], 1), 
                                   round(acc.q.arma.plant[5], 1)),
                         Mineral = c(round(acc.q.arma.mineral[1], 1), 
                                     round(acc.q.arma.mineral[2], 1),
                                     round(acc.q.arma.mineral[3], 1), 
                                     round(acc.q.arma.mineral[4], 1),
                                     round(acc.q.arma.mineral[5], 1)),
                         Food = c(round(acc.q.arma.food[1], 1), 
                                  round(acc.q.arma.food[2], 1),
                                  round(acc.q.arma.food[3], 1), 
                                  round(acc.q.arma.food[4], 1),
                                  round(acc.q.arma.food[5], 1)),
                         Wood = c(round(acc.q.arma.wood[1], 1), 
                                  round(acc.q.arma.wood[2], 1),
                                  round(acc.q.arma.wood[3], 1), 
                                  round(acc.q.arma.wood[4], 1),
                                  round(acc.q.arma.wood[5], 1)),
                         Oil = c(round(acc.q.arma.oil[1], 1), 
                                 round(acc.q.arma.oil[2], 1),
                                 round(acc.q.arma.oil[3], 1), 
                                 round(acc.q.arma.oil[4], 1),
                                 round(acc.q.arma.oil[5], 1))))
row.names(acc.q.mat.arma) <- c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE')
acc.q.mat.arma

###### Table 1: VAR, quarterly

(acc.q.mat.var <- cbind(Plant = c(round(acc.q.var.plant[1], 1), 
                                  round(acc.q.var.plant[2], 1), 
                                  round(acc.q.var.plant[3], 1), 
                                  round(acc.q.var.plant[4], 1), 
                                  round(acc.q.var.plant[5], 1)),
                        Mineral = c(round(acc.q.var.mineral[1], 1), 
                                    round(acc.q.var.mineral[2], 1),
                                    round(acc.q.var.mineral[3], 1), 
                                    round(acc.q.var.mineral[4], 1),
                                    round(acc.q.var.mineral[5], 1)),
                        Food = c(round(acc.q.var.food[1], 1), 
                                 round(acc.q.var.food[2], 1),
                                 round(acc.q.var.food[3], 1), 
                                 round(acc.q.var.food[4], 1),
                                 round(acc.q.var.food[5], 1)),
                        Wood = c(round(acc.q.var.wood[1], 1), 
                                 round(acc.q.var.wood[2], 1),
                                 round(acc.q.var.wood[3], 1), 
                                 round(acc.q.var.wood[4], 1),
                                 round(acc.q.var.wood[5], 1)),
                        Oil = c(round(acc.q.var.oil[1], 1), 
                                round(acc.q.var.oil[2], 1),
                                round(acc.q.var.oil[3], 1), 
                                round(acc.q.var.oil[4], 1),
                                round(acc.q.var.oil[5], 1))))
row.names(acc.q.mat.var) <- c('ME', 'RMSE', 'MAE', 'MPE', 'MAPE')
acc.q.mat.var
