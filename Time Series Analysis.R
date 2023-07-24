setwd("C:/Users/david/OneDrive/Documents/USD/DSCI 725/Project")

data = read.csv( "AirPassengers.csv" )
head(data)
dim(data) #242 records
sum(is.na(data)) #0 missing values
data$date = paste(data$Year, data$Month, sep = "/") #combine the year&month into date
updated_data = data[c("date", "DOMESTIC", "INTERNATIONAL")] #update the initial dataset-date column
head(updated_data)
summary(updated_data)

#check for the time series components available in the dataset
#since the last year(2022) ends on Nov, the start year(2002 will start at Dec, to make an even complete 12 months cycle
#DOMESTIC AIR TRAVEL
updated_data_dom.ts = ts( updated_data$DOMESTIC, start = c(2002,12), end = c(2022, 11), freq = 12 )
summary(updated_data_dom.ts)
ylow_dom = round(summary(updated_data_dom.ts)[1],-4)
yhigh_dom = summary(updated_data_dom.ts)[6] + 1000000
paste(ylow_dom, yhigh_dom) 
options(scipen = 999) #prevent scientific notation from showing on the plot

windows()
par(mfrow = c(1,1))
plot(updated_data_dom.ts, ylim = c(ylow_dom, yhigh_dom),  ylab = "No. of Passengers", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c((2002.12), (2022.11)), main = "Domestic Air Travel")
axis(1, at = seq(2002, 2022, 1), labels = format(seq(2002, 2022, 1)))
#time series components available are level, trend, seasonality and noise
#In early 2020 a huge drop in the number of passengers is observed. This is due to the COVID-19 crisis


#INTERNATIONAL AIR TRAVEL
updated_data_int.ts = ts( updated_data$INTERNATIONAL, start = c(2002,12), end = c(2022, 11), freq = 12 )
summary(updated_data_int.ts)
ylow_int = round(summary(updated_data_int.ts)[1],-4)
yhigh_int = summary(updated_data_int.ts)[6] + 1000000
paste(ylow_int, yhigh_int) 
options(scipen = 999) #prevent scientific notation from showing on the plot

windows()
par(mfrow = c(1,1))
plot(updated_data_int.ts, ylim = c(ylow_int, yhigh_int),  ylab = "No. of Passengers", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c((2002.12), (2022.11)), main = "International Air Travel")
axis(1, at = seq(2002, 2022, 1), labels = format(seq(2002, 2022, 1)))
#time series components available are level, trend, seasonality and noise
#In early 2020 a huge drop in the number of passengers is observed. This is due to the COVID-19 crisis


#DOMESTIC & INTERNATIONAL VISUALIAZTION
windows()
par(mfrow = c(1,2))
plot(updated_data_dom.ts, ylim = c(ylow_dom, yhigh_dom),  ylab = "No. of Passengers", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c((2002.12), (2022.11)), main = "Domestic Air Travel")
axis(1, at = seq(2002, 2022, 1), labels = format(seq(2002, 2022, 1)))
plot(updated_data_int.ts, ylim = c(ylow_int, yhigh_int),  ylab = "No. of Passengers", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c((2002.12), (2022.11)), main = "International Air Travel")
axis(1, at = seq(2002, 2022, 1), labels = format(seq(2002, 2022, 1)))


#PART 1: DOMESTIC DATASET
#PARTITION the DOMESTIC dataset
nValid = 12
nTrain_dom = length(updated_data_dom.ts) - nValid
train_dom.ts = window(updated_data_dom.ts, start = c(2002, 12), end = c((2002.12), nTrain_dom + 10))
valid_dom.ts = window(updated_data_dom.ts, start = c((2002.12), nTrain_dom + 10), end = c(2002, nTrain_dom + 11 + nValid))
length(train_dom.ts)
length(valid_dom.ts)
length(updated_data_dom.ts)
train_dom.ts
valid_dom.ts


#1 LINEAR MODEL
library(forecast)
set.seed(101)
train_dom.lm.trend = tslm(train_dom.ts ~ trend)
train_dom.lm.season = tslm(train_dom.ts ~ season)
train_dom.lm.trend.season = tslm(train_dom.ts ~ trend + season)
train_dom.lm.trend.season.quad = tslm(train_dom.ts ~ trend + season + I(trend^2))

windows()
par(mfrow = c(2,2))
plot(train_dom.ts, xlab = "Time: Trend", ylab = "No. of Passengers", ylim = c(ylow_dom, yhigh_dom), bty ="l" )
lines(train_dom.lm.trend$fitted, lwd = 2, col = "red")

plot(train_dom.ts, xlab = "Time: Season", ylab = "No. of Passengers", ylim = c(ylow_dom, yhigh_dom), bty ="l" )
lines(train_dom.lm.season$fitted, lwd = 2, col = "blue")

plot(train_dom.ts, xlab = "Time: Trend+Season", ylab = "No. of Passengers", ylim = c(ylow_dom, yhigh_dom), bty ="l" )
lines(train_dom.lm.trend.season$fitted, lwd = 2, col = "darkgreen")

plot(train_dom.ts, xlab = "Time: Trend+Season+Trend^2", ylab = "No. of Passengers", ylim = c(ylow_dom, yhigh_dom), bty ="l" )
lines(train_dom.lm.trend.season.quad$fitted, lwd = 2, col = "purple")

summary(train_dom.lm.trend)$adj.r.squared #= -.00173 
summary(train_dom.lm.season)$adj.r.squared #= .1134
summary(train_dom.lm.trend.season)$adj.r.squared #= .1112
summary(train_dom.lm.trend.season.quad)$adj.r.squared #= .17173

pred_dom.lm.trend = forecast(train_dom.lm.trend, h = nValid, level = 0)
accuracy(pred_dom.lm.trend, valid_dom.ts)
#MASE = (1.235 for Training set), and (1.579 for Test set)
pred_dom.lm.season = forecast(train_dom.lm.season, h = nValid, level = 0)
accuracy(pred_dom.lm.season, valid_dom.ts)
#MASE = (0.981 for Training set), and (1.372 for Test set)
pred_dom.lm.trend.season = forecast(train_dom.lm.trend.season, h = nValid, level = 0)
accuracy(pred_dom.lm.trend.season, valid_dom.ts)
#MASE = (0.957 for Training set), and (1.250 for Test set)
pred_dom.lm = forecast(train_dom.lm.trend.season.quad, h = nValid, level = 0)
accuracy(pred_dom.lm, valid_dom.ts)
#MASE = (1.019 for Training set), and (2.335 for Test set)

#Plot the best Linear Model
windows()
par(mfrow = c(1,1))
plot(pred_dom.lm, ylim = c(ylow_dom, yhigh_dom),  ylab = "No. of Passengers", col="darkgreen",
     xlab = "Time", bty = "l", xaxt = "n", xlim = c((2002.12), (2024.11)), main = "Domestic Air Travel")
axis(1, at = seq(2002, 2024, 1), labels = format(seq(2002, 2024, 1)))
lines(train_dom.lm.trend.season.quad$fitted, lwd = 2, col = "red")
lines(valid_dom.ts, col = "purple", lwd = 2)


#2 NAIVE Methods
library(fpp)
set.seed(201)
n_dom1 = meanf(train_dom.ts, h = nValid)
n_dom2 = rwf(train_dom.ts, h = nValid)
n_dom3 = snaive(train_dom.ts, h = nValid)

windows()
par(mfrow = c(1,1))
plot(n_dom1, main="Forecasts Domestic Air Passengers")
lines(n_dom2$mean, col = "red", lwd = 2)
lines(n_dom3$mean, col = "purple", lwd = 1.5)
lines(updated_data_dom.ts)
legend("bottomleft", lty=1, col=c("black", "red","purple"),
       legend=c("Mean method","Random Walk Naive method","Seasonal naive method"))

accuracy(n_dom1, valid_dom.ts) #MASE = 1.256 Training set, 1.696 Test set
accuracy(n_dom2, valid_dom.ts) #MASE = .765 Training set, 1.727 Test set
accuracy(n_dom3, valid_dom.ts) #MASE = 1.000 Training set, 3.426 Test set


#3 EXPONENTIAL SMOOTHING FORECASTS
set.seed(301)
ets_dom = ets(train_dom.ts, model = "ZZZ")
summary(ets_dom)
ets_dom.pred = forecast(ets_dom, h = nValid, level = 0)
accuracy(ets_dom.pred, valid_dom.ts)
#MASE = (.282 for the Training set), and (.945 for the Test set)
ets_dom.pred$method #ETS(A,N,A)
ets_dom.pred$model$aic #8135.587

windows()
par(mfrow = c(1,1))
plot(ets_dom.pred, ylim = c(ylow_dom, yhigh_dom),  ylab = "No. of Passengers", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c((2002.12), (2022.11)), main = "Domestic Air Travel")
axis(1, at = seq(2002, 2022, 1), labels = format(seq(2002, 2022, 1)))
lines(valid_dom.ts, lwd = 2, col = "darkorange")
lines(ets_dom.pred$fitted, lwd = 1.5, col = "blue")

#Predict the next window using ets
ets_dom_new = ets(updated_data_dom.ts, model = "ZZZ")
summary(ets_dom_new)
ets_dom_new.pred = forecast(ets_dom_new, h = nValid, level = 0)
ets_dom_new.pred


#4 NEURAL NETWORK FORECASTS
set.seed(401)
nnetar_dom = nnetar(train_dom.ts, P = 12, scale.inputs = TRUE)

#nnetar_dom$fitted
nnetar_dom.pred = forecast(nnetar_dom, h=nValid)
nnetar_dom.pred$fitted
length(nnetar_dom.pred)

#Check Neural Nets accuracy
accuracy( nnetar_dom.pred, valid_dom.ts ) 
#MASE = (.0307 for the Training set) and (1.869 for the Test set)

windows()
par(mfrow = c(1,1))
plot(train_dom.ts, ylim = c(ylow_dom, yhigh_dom),  ylab = "No. of Passengers", col="darkgreen",
     xlab = "Time", bty = "l", xaxt = "n", xlim = c((2002.12), (2024.11)), main = "Domestic Air Travel")
axis(1, at = seq(2002, 2024, 1), labels = format(seq(2002, 2024, 1)))
lines(nnetar_dom.pred$fitted, lwd = 1.5, col = "blue", lty = 1) 
lines(nnetar_dom.pred$mean, lwd = 3, col = "darkorange", lty = 4) 
lines(valid_dom.ts, col="purple")

lines(c(2022, 2022), c(ylow_dom, yhigh_dom))
lines(c(2023, 2023), c(ylow_dom, yhigh_dom))
text(2021.5, yhigh_dom, "T") # Training
text(2022.5, yhigh_dom, "V") # Validation
text(2023.5, yhigh_dom, "F") # Future

legend("bottomleft", legend=c("Actual Training","Fitted Training", 
                           "Predicted Validation", "Actual Validation"),
       col=c("darkgreen", "blue", "darkorange", "purple"), lwd=2, cex=1.0)

#For the above Domestic air travel analysis, ets(A,N,A),i.e., Additive error, No Trend, Additive seasonality 
#...produced the best results among all the 4 models evaluated.
#This is the model used to predict the future air passengers values. 
#Check model #3 above for the predicted values. 



#PART 2: INTERNATIONAL DATASET
#PARTITION the INTERNATIONAL dataset
nValid = 12
nTrain_int = length(updated_data_int.ts) - nValid
train_int.ts = window(updated_data_int.ts, start = c(2002, 12), end = c((2002.12), nTrain_int + 10))
valid_int.ts = window(updated_data_int.ts, start = c((2002.12), nTrain_int + 10), end = c(2002, nTrain_int + 11 + nValid))
length(train_int.ts)
length(valid_int.ts)
length(updated_data_int.ts)
train_int.ts
valid_int.ts


#1 LINEAR MODEL
library(forecast)
set.seed(121)
train_int.lm.trend = tslm(train_int.ts ~ trend)
train_int.lm.season = tslm(train_int.ts ~ season)
train_int.lm.trend.season = tslm(train_int.ts ~ trend + season)
train_int.lm.trend.season.quad = tslm(train_int.ts ~ trend + season + I(trend^2))

windows()
par(mfrow = c(2,2))
plot(train_int.ts, xlab = "Time: Trend", ylab = "No. of Passengers", ylim = c(ylow_int, yhigh_int), bty ="l" )
lines(train_int.lm.trend$fitted, lwd = 2, col = "red")

plot(train_int.ts, xlab = "Time: Season", ylab = "No. of Passengers", ylim = c(ylow_int, yhigh_int), bty ="l" )
lines(train_int.lm.season$fitted, lwd = 2, col = "blue")

plot(train_int.ts, xlab = "Time: Trend+Season", ylab = "No. of Passengers", ylim = c(ylow_int, yhigh_int), bty ="l" )
lines(train_int.lm.trend.season$fitted, lwd = 2, col = "darkgreen")

plot(train_int.ts, xlab = "Time: Trend+Season+Trend^2", ylab = "No. of Passengers", ylim = c(ylow_int, yhigh_int), bty ="l" )
lines(train_int.lm.trend.season.quad$fitted, lwd = 2, col = "purple")

summary(train_int.lm.trend)$adj.r.squared #= .066
summary(train_int.lm.season)$adj.r.squared #= .049
summary(train_int.lm.trend.season)$adj.r.squared #= .114
summary(train_int.lm.trend.season.quad)$adj.r.squared #= .292

pred_int.lm.trend = forecast(train_int.lm.trend, h = nValid, level = 0)
accuracy(pred_int.lm.trend, valid_int.ts)
#MASE = (1.485 for Training set), and (1.856 for Test set)
pred_int.lm.lm.season = forecast(train_int.lm.season, h = nValid, level = 0)
accuracy(pred_int.lm.lm.season, valid_int.ts)
#MASE = (1.619 for Training set), and (1.396 for Test set)
pred_int.lm.trend.season = forecast(train_int.lm.trend.season, h = nValid, level = 0)
accuracy(pred_int.lm.trend.season, valid_int.ts)
#MASE = (1.307 for Training set), and (1.427 for Test set)
pred_int.lm = forecast(train_int.lm.trend.season.quad, h = nValid, level = 0)
accuracy(pred_int.lm, valid_int.ts)
#MASE = (1.298 for Training set), and (1.826 for Test set)

#Plot the best Linear Model
windows()
par(mfrow = c(1,1))
plot(pred_int.lm, xlab = "Time: Trend+Season+Trend^2", ylab = "No. of Passengers", ylim = c(ylow_int, yhigh_int), xlim = c(2002, 2022), bty ="l" )
lines(train_int.lm.trend.season.quad$fitted, lwd = 2, col = "red")
lines(valid_int.ts, col = "purple", lwd = 2)


#2 NAIVE Methods
library(fpp)
set.seed(221)
n_int1 = meanf(train_int.ts, h = nValid)
n_int2 = rwf(train_int.ts, h = nValid)
n_int3 = snaive(train_int.ts, h = nValid)

windows()
par(mfrow = c(1,1))
plot(n_int1, main="Forecasts International Air Passengers")
lines(n_int2$mean, col = "red", lwd = 2)
lines(n_int3$mean, col = "purple", lwd = 1.5)

lines(updated_data_int.ts)
legend("bottomleft", lty=1, col=c("black", "red","purple"),
       legend=c("Mean method","Random Walk Naive method","Seasonal naive method"))

accuracy(n_int1, valid_int.ts) #MASE = 1.744 Training set, 1.782 Test set
accuracy(n_int2, valid_int.ts) #MASE = .705 Training set, 3.099 Test set
accuracy(n_int3, valid_int.ts) #MASE = 1.000 Training set, 4.200 Test set

valid_int.ts

#3 EXPONENTIAL SMOOTHING FORECASTS
set.seed(321)
ets_int = ets(train_int.ts, model = "ZZZ")
summary(ets_int)
ets_int.pred = forecast(ets_int, h = nValid, level = 0)
accuracy(ets_int.pred, valid_int.ts)
#MASE = (.268 for the Training set), and (2.313 for the Test set)
ets_int.pred$method #ETS(A,Ad,A)
ets_int.pred$model$aic #7546.802

windows()
par(mfrow = c(1,1))
plot(ets_int.pred, ylim = c(ylow_int, yhigh_int),  ylab = "No. of Passengers", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c((2002.12), (2022.11)), main = "International Air Travel")
axis(1, at = seq(2002, 2022, 1), labels = format(seq(2002, 2022, 1)))
lines(valid_int.ts, lwd = 2, col = "darkorange")
lines(ets_int.pred$fitted, lwd = 1.5, col = "blue")


#4 NEURAL NETWORK FORECASTS
set.seed(421)
nnetar_int = nnetar(train_int.ts, P = 12, scale.inputs = TRUE)

nnetar_int$fitted
nnetar_int.pred = forecast(nnetar_int, h=nValid)
nnetar_int.pred
nnetar_int.pred$fitted
length(nnetar_int.pred)

#Check Neural Nets accuracy
accuracy(nnetar_int.pred, valid_int.ts ) 
#MASE = (.031 for the Training set) and (6.105 for the Test set)

windows()
par(mfrow = c(1,1))
plot(train_int.ts, ylim = c(ylow_int, yhigh_int),  ylab = "No. of Passengers", col="darkgreen",
     xlab = "Time", bty = "l", xaxt = "n", xlim = c((2002.12), (2024.11)), main = "International Air Travel")
axis(1, at = seq(2002, 2024, 1), labels = format(seq(2002, 2024, 1)))
lines(nnetar_int.pred$fitted, lwd = 1.5, col = "blue", lty = 1) 
lines(nnetar_int.pred$mean, lwd = 3, col = "darkorange", lty = 4) 
lines(valid_int.ts, col="purple")

lines(c(2022, 2022), c(ylow_int, yhigh_int))
lines(c(2023, 2023), c(ylow_int, yhigh_int))
text(2021.5, yhigh_int, "T") # Training
text(2022.5, yhigh_int, "V") # Validation
text(2023.5, yhigh_int, "F") # Future

legend("bottomleft", legend=c("Actual Training","Fitted Training", 
                              "Predicted Validation", "Actual Validation"),
       col=c("darkgreen", "blue", "darkorange", "purple"), lwd=2, cex=1.0)

#For International air travel, none of the 4 models explored above performed better than naive
#This means for the prediction purposes we are going to use naive method, with the 12-month window. 

#PRECOVID 
#PART 2: INTERNATIONAL DATASET
#PARTITION the INTERNATIONAL dataset
updated_data_int_precovid.ts = ts( updated_data$INTERNATIONAL, start = c(2002,12), end = c(2020, 11), freq = 12 )
#INTERNATIONAL AIR TRAVEL
updated_data_int_precovid.ts = ts( updated_data$INTERNATIONAL, start = c(2002,12), end = c(2020, 11), freq = 12 )
summary(updated_data_int_precovid.ts)
nValid = 12
nTrain_int_precovid = length(updated_data_int_precovid.ts) - nValid
train_int.ts_precovid = window(updated_data_int_precovid.ts, start = c(2002, 12), end = c((2002.12), nTrain_int_precovid + 10))
valid_int.ts_precovid = window(updated_data_int_precovid.ts, start = c((2002.12), nTrain_int_precovid + 10), end = c(2002, nTrain_int_precovid + 11 + nValid))
length(train_int.ts_precovid)
length(valid_int.ts_precovid)
length(updated_data_int_precovid.ts)
train_int.ts_precovid
valid_int.ts_precovid
#EXPONENTIAL SMOOTHING FORECASTS
set.seed(701)
ets_int_precovid = ets(train_int.ts_precovid, model = "ZZZ")
summary(ets_int_precovid)
ets_int_precovid.pred = forecast(ets_int_precovid, h = nValid, level = 0)
accuracy(ets_int_precovid.pred, valid_int.ts_precovid)
#MASE = (.3135 for the Training set), and (15.0028 for the Test set)
ets_int_precovid.pred$method #ETS(M,A,M)
ets_int_precovid.pred$model$aic #6310.27




#Suppressing seasonality
#Check the data on a monthly basis
d = 2022 - 2002 
d = d * 12
d
d/12
dec = seq(1, d, 12) # December months
jan = seq(2, d, 12) # January months
feb = seq(3, d, 12) # February months
mar = seq(4, d, 12) # March months
apr = seq(5, d, 12) # April months
may = seq(6, d, 12) # May months
jun = seq(7, d, 12) # June months
jul = seq(8, d, 12) # July months
aug = seq(9, d, 12) # August months
sep = seq(10, d, 12) # September months
oct = seq(11, d, 12) # October months
nov = seq(12, d, 12) # November months

dec

#DOMESTIC DATA
#January
set.seed(131)
dom.jan = updated_data$DOMESTIC[jan]
head(dom.jan)
tail(dom.jan)
dom.jan.ts = ts(dom.jan, start = c(2002,1), end = c(2023,1), freq = 1)
dom.jan.ts
dom.jan.lm = tslm(dom.jan.ts ~ trend )
summary(dom.jan.lm ) # Adjusted R-squared = -.0394
plot(dom.jan.ts, xlab = "Time", ylab = "Domestic Air Travels", bty = "l" )
lines( dom.jan.lm$fitted, lwd=2 )

# see if quadratic has a better fit for the air data
dom.janB.lm <- tslm( dom.jan.ts ~ trend + I(trend^2) )
summary( dom.janB.lm ) # Adjusted R-squared: .0127
plot( dom.jan.ts, xlab = "Time", ylab = "Domestic Air Travels", main = "Domestic Air Travel - Jan", bty = "l" )
lines( dom.janB.lm$fitted, lwd=2 )

#February
dom.feb = updated_data$DOMESTIC[feb]
head(dom.feb)
tail(dom.feb)
dom.feb.ts = ts(dom.feb, start = c(2002,1), end = c(2023,1), freq = 1)
dom.feb.ts
dom.feb.lm = tslm(dom.feb.ts ~ trend )
summary(dom.feb.lm ) # Adjusted R-squared = -.0330
plot(dom.feb.ts, xlab = "Time", ylab = "Domestic Air Travels", main = "Domestic Air Travel - Feb", bty = "l" )
lines(dom.feb.lm$fitted, lwd=2 )

#March
dom.mar = updated_data$DOMESTIC[mar]
head(dom.mar)
tail(dom.mar)
dom.mar.ts = ts(dom.mar, start = c(2002,1), end = c(2023,1), freq = 1)
dom.mar.ts
dom.mar.lm = tslm(dom.mar.ts ~ trend )
summary(dom.mar.lm ) # Adjusted R-squared = .0364
plot(dom.mar.ts, xlab = "Time", ylab = "Domestic Air Travels", main = "Domestic Air Travel - Mar", bty = "l" )
lines(dom.mar.lm$fitted, lwd=2)

#April
dom.apr = updated_data$DOMESTIC[apr]
head(dom.apr)
tail(dom.apr)
dom.apr.ts = ts(dom.apr, start = c(2002,1), end = c(2023,1), freq = 1)
dom.apr.ts
dom.apr.lm = tslm(dom.apr.ts ~ trend )
summary(dom.apr.lm ) # Adjusted R-squared = -.0461
plot(dom.apr.ts, xlab = "Time", ylab = "Domestic Air Travels", main = "Domestic Air Travel - apr", bty = "l" )
lines(dom.apr.lm$fitted, lwd=2)

#May
dom.may = updated_data$DOMESTIC[may]
head(dom.may)
tail(dom.may)
dom.may.ts = ts(dom.may, start = c(2002,1), end = c(2023,1), freq = 1)
dom.may.ts
dom.may.lm = tslm(dom.may.ts ~ trend )
summary(dom.may.lm ) # Adjusted R-squared = -.0461
plot(dom.may.ts, xlab = "Time", ylab = "Domestic Air Travels", main = "Domestic Air Travel - may", bty = "l" )
lines(dom.may.lm$fitted, lwd=2)

#June
dom.jun = updated_data$DOMESTIC[jun]
head(dom.jun)
tail(dom.jun)
dom.jun.ts = ts(dom.jun, start = c(2002,1), end = c(2023,1), freq = 1)
dom.jun.ts
dom.jun.lm = tslm(dom.jun.ts ~ trend )
summary(dom.jun.lm ) # Adjusted R-squared = -.0361
plot(dom.jun.ts, xlab = "Time", ylab = "Domestic Air Travels", main = "Domestic Air Travel - jun", bty = "l" )
lines(dom.jun.lm$fitted, lwd=2)

#July
dom.jul = updated_data$DOMESTIC[jul]
head(dom.jul)
tail(dom.jul)
dom.jul.ts = ts(dom.jul, start = c(2002,1), end = c(2023,1), freq = 1)
dom.jul.ts
dom.jul.lm = tslm(dom.jul.ts ~ trend )
summary(dom.jul.lm ) # Adjusted R-squared = -.0475
plot(dom.jul.ts, xlab = "Time", ylab = "Domestic Air Travels", main = "Domestic Air Travel - jul", bty = "l" )
lines(dom.jul.lm$fitted, lwd=2)

#August
dom.aug = updated_data$DOMESTIC[aug]
head(dom.aug)
tail(dom.aug)
dom.aug.ts = ts(dom.aug, start = c(2002,1), end = c(2023,1), freq = 1)
dom.aug.ts
dom.aug.lm = tslm(dom.aug.ts ~ trend )
summary(dom.aug.lm ) # Adjusted R-squared = -.0497
plot(dom.aug.ts, xlab = "Time", ylab = "Domestic Air Travels", main = "Domestic Air Travel - aug", bty = "l" )
lines(dom.aug.lm$fitted, lwd=2)

#September
dom.sep = updated_data$DOMESTIC[sep]
head(dom.sep)
tail(dom.sep)
dom.sep.ts = ts(dom.sep, start = c(2002,1), end = c(2023,1), freq = 1)
dom.sep.ts
dom.sep.lm = tslm(dom.sep.ts ~ trend )
summary(dom.sep.lm ) # Adjusted R-squared = -.0492
plot(dom.sep.ts, xlab = "Time", ylab = "Domestic Air Travels", main = "Domestic Air Travel - sep", bty = "l" )
lines(dom.sep.lm$fitted, lwd=2)

#October
dom.oct = updated_data$DOMESTIC[oct]
head(dom.oct)
tail(dom.oct)
dom.oct.ts = ts(dom.oct, start = c(2002,1), end = c(2023,1), freq = 1)
dom.oct.ts
dom.oct.lm = tslm(dom.oct.ts ~ trend )
summary(dom.oct.lm ) # Adjusted R-squared = -.0494
plot(dom.oct.ts, xlab = "Time", ylab = "Domestic Air Travels", main = "Domestic Air Travel - oct", bty = "l" )
lines(dom.oct.lm$fitted, lwd=2)

#November
dom.nov = updated_data$DOMESTIC[nov]
head(dom.nov)
tail(dom.nov)
dom.nov.ts = ts(dom.nov, start = c(2002,1), end = c(2023,1), freq = 1)
dom.nov.ts
dom.nov.lm = tslm(dom.nov.ts ~ trend )
summary(dom.nov.lm ) # Adjusted R-squared = -.0305
plot(dom.nov.ts, xlab = "Time", ylab = "Domestic Air Travels", main = "Domestic Air Travel - nov", bty = "l" )
lines(dom.nov.lm$fitted, lwd=2)

#December
dom.dec = updated_data$DOMESTIC[dec]
head(dom.dec)
tail(dom.dec)
dom.dec.ts = ts(dom.dec, start = c(2002,1), end = c(2023,1), freq = 1)
dom.dec.ts
dom.dec.lm = tslm(dom.dec.ts ~ trend )
summary(dom.dec.lm ) # Adjusted R-squared = -.0384
plot(dom.dec.ts, xlab = "Time", ylab = "Domestic Air Travels", main = "Domestic Air Travel - dec", bty = "l" )
lines(dom.dec.lm$fitted, lwd=2)


#Monthly Summary statistics
#1. Seasonality
#Select a small range to view the seasonality in the data
stat_data_dom.ts = ts( updated_data$DOMESTIC, start = c(2010,1), end = c(2015,12), freq = 12 )
summary(stat_data_dom.ts)
ylow_stat_dom = round(summary(stat_data_dom.ts)[1],-4)
yhigh_stat_dom = summary(stat_data_dom.ts)[6] + 1000000
paste(ylow_stat_dom, yhigh_stat_dom) 
options(scipen = 999) #prevent scientific notation from showing on the plot

windows()
par(mfrow = c(1,1))
plot(stat_data_dom.ts, ylim = c(ylow_stat_dom, yhigh_stat_dom),  ylab = "No. of Passengers", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(2010, 2015), main = "Domestic Air Travel")
axis(1, at = seq(2010, 2015, 1), labels = format(seq(2010, 2015, 1)))

#2. Median
median_dom=c(summary(dom.jan.ts)[3],
             summary(dom.feb.ts)[3],
             summary(dom.mar.ts)[3],
             summary(dom.apr.ts)[3],
             summary(dom.may.ts)[3],
             summary(dom.jun.ts)[3],
             summary(dom.jul.ts)[3],
             summary(dom.aug.ts)[3],
             summary(dom.sep.ts)[3],
             summary(dom.oct.ts)[3],
             summary(dom.nov.ts)[3],
             summary(dom.dec.ts)[3])
windows()
barplot(median_dom, 
        main = "Median Summary Statistics for each month", 
        names.arg = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
        ylab = "Median Value")



#INTERNATIONAL DATA
#January
set.seed(141)
int.jan = updated_data$INTERNATIONAL[jan]
head(int.jan)
tail(int.jan)
int.jan.ts = ts(int.jan, start = c(2002,1), end = c(2023,1), freq = 1)
int.jan.ts
int.jan.lm = tslm(int.jan.ts ~ trend )
summary(int.jan.lm ) # Adjusted R-squared = -.0241
plot(int.jan.ts, xlab = "Time", ylab = "International Air Travels", main = "International Air Travel - jan", bty = "l" )
lines(int.jan.lm$fitted, lwd=2 )

#February
int.feb = updated_data$INTERNATIONAL[feb]
head(int.feb)
tail(int.feb)
int.feb.ts = ts(int.feb, start = c(2002,1), end = c(2023,1), freq = 1)
int.feb.ts
int.feb.lm = tslm(int.feb.ts ~ trend )
summary(int.feb.lm ) # Adjusted R-squared = -.0066
plot(int.feb.ts, xlab = "Time", ylab = "International Air Travels", main = "International Air Travel - feb", bty = "l" )
lines(int.feb.lm$fitted, lwd=2 )

#March
int.mar = updated_data$INTERNATIONAL[mar]
head(int.mar)
tail(int.mar)
int.mar.ts = ts(int.mar, start = c(2002,1), end = c(2023,1), freq = 1)
int.mar.ts
int.mar.lm = tslm(int.mar.ts ~ trend )
summary(int.mar.lm ) # Adjusted R-squared = -.0271
plot(int.mar.ts, xlab = "Time", ylab = "INTERNATIONAL Air Travels", main = "INTERNATIONAL Air Travel - Mar", bty = "l" )
lines(int.mar.lm$fitted, lwd=2)

#April
int.apr = updated_data$INTERNATIONAL[apr]
head(int.apr)
tail(int.apr)
int.apr.ts = ts(int.apr, start = c(2002,1), end = c(2023,1), freq = 1)
int.apr.ts
int.apr.lm = tslm(int.apr.ts ~ trend )
summary(int.apr.lm ) # Adjusted R-squared = -.0347
plot(int.apr.ts, xlab = "Time", ylab = "INTERNATIONAL Air Travels", main = "INTERNATIONAL Air Travel - apr", bty = "l" )
lines(int.apr.lm$fitted, lwd=2)

#May
int.may = updated_data$INTERNATIONAL[may]
head(int.may)
tail(int.may)
int.may.ts = ts(int.may, start = c(2002,1), end = c(2023,1), freq = 1)
int.may.ts
int.may.lm = tslm(int.may.ts ~ trend )
summary(int.may.lm ) # Adjusted R-squared = -.0491
plot(int.may.ts, xlab = "Time", ylab = "INTERNATIONAL Air Travels", main = "INTERNATIONAL Air Travel - may", bty = "l" )
lines(int.may.lm$fitted, lwd=2)

#June
int.jun = updated_data$INTERNATIONAL[jun]
head(int.jun)
tail(int.jun)
int.jun.ts = ts(int.jun, start = c(2002,1), end = c(2023,1), freq = 1)
int.jun.ts
int.jun.lm = tslm(int.jun.ts ~ trend )
summary(int.jun.lm ) # Adjusted R-squared = -.0499
plot(int.jun.ts, xlab = "Time", ylab = "INTERNATIONAL Air Travels", main = "INTERNATIONAL Air Travel - jun", bty = "l" )
lines(int.jun.lm$fitted, lwd=2)

#July
int.jul = updated_data$INTERNATIONAL[jul]
head(int.jul)
tail(int.jul)
int.jul.ts = ts(int.jul, start = c(2002,1), end = c(2023,1), freq = 1)
int.jul.ts
int.jul.lm = tslm(int.jul.ts ~ trend )
summary(int.jul.lm ) # Adjusted R-squared = -.0487
plot(int.jul.ts, xlab = "Time", ylab = "INTERNATIONAL Air Travels", main = "INTERNATIONAL Air Travel - jul", bty = "l" )
lines(int.jul.lm$fitted, lwd=2)

#August
int.aug = updated_data$INTERNATIONAL[aug]
head(int.aug)
tail(int.aug)
int.aug.ts = ts(int.aug, start = c(2002,1), end = c(2023,1), freq = 1)
int.aug.ts
int.aug.lm = tslm(int.aug.ts ~ trend )
summary(int.aug.lm ) # Adjusted R-squared = -.0449
plot(int.aug.ts, xlab = "Time", ylab = "INTERNATIONAL Air Travels", main = "INTERNATIONAL Air Travel - aug", bty = "l" )
lines(int.aug.lm$fitted, lwd=2)

#September
int.sep = updated_data$INTERNATIONAL[sep]
head(int.sep)
tail(int.sep)
int.sep.ts = ts(int.sep, start = c(2002,1), end = c(2023,1), freq = 1)
int.sep.ts
int.sep.lm = tslm(int.sep.ts ~ trend )
summary(int.sep.lm ) # Adjusted R-squared = -.0426
plot(int.sep.ts, xlab = "Time", ylab = "INTERNATIONAL Air Travels", main = "INTERNATIONAL Air Travel - sep", bty = "l" )
lines(int.sep.lm$fitted, lwd=2)

#October
int.oct = updated_data$INTERNATIONAL[oct]
head(int.oct)
tail(int.oct)
int.oct.ts = ts(int.oct, start = c(2002,1), end = c(2023,1), freq = 1)
int.oct.ts
int.oct.lm = tslm(int.oct.ts ~ trend )
summary(int.oct.lm ) # Adjusted R-squared = -.0424
plot(int.oct.ts, xlab = "Time", ylab = "INTERNATIONAL Air Travels", main = "INTERNATIONAL Air Travel - oct", bty = "l" )
lines(int.oct.lm$fitted, lwd=2)

#November
int.nov = updated_data$INTERNATIONAL[nov]
head(int.nov)
tail(int.nov)
int.nov.ts = ts(int.nov, start = c(2002,1), end = c(2023,1), freq = 1)
int.nov.ts
int.nov.lm = tslm(int.nov.ts ~ trend )
summary(int.nov.lm ) # Adjusted R-squared = -.0415
plot(int.nov.ts, xlab = "Time", ylab = "INTERNATIONAL Air Travels", main = "INTERNATIONAL Air Travel - nov", bty = "l" )
lines(int.nov.lm$fitted, lwd=2)

#December
int.dec = updated_data$INTERNATIONAL[dec]
head(int.dec)
tail(int.dec)
int.dec.ts = ts(int.dec, start = c(2002,1), end = c(2023,1), freq = 1)
int.dec.ts
int.dec.lm = tslm(int.dec.ts ~ trend )
summary(int.dec.lm ) # Adjusted R-squared = -.03467
plot(int.dec.ts, xlab = "Time", ylab = "INTERNATIONAL Air Travels", main = "INTERNATIONAL Air Travel - dec", bty = "l" )
lines(int.dec.lm$fitted, lwd=2)


#Monthly Summary statistics
#1. Seasonality
#Select a small range to view the seasonality in the data
stat_data_int.ts = ts( updated_data$INTERNATIONAL, start = c(2010,1), end = c(2015,12), freq = 12 )
summary(stat_data_int.ts)
ylow_stat_int = round(summary(stat_data_int.ts)[1],-4)
yhigh_stat_int = summary(stat_data_int.ts)[6] + 1000000
paste(ylow_stat_int, yhigh_stat_int) 
options(scipen = 999) #prevent scientific notation from showing on the plot

windows()
par(mfrow = c(1,1))
plot(stat_data_int.ts, ylim = c(ylow_stat_int, yhigh_stat_int),  ylab = "No. of Passengers", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(2010, 2015), main = "International Air Travel")
axis(1, at = seq(2010, 2015, 1), labels = format(seq(2010, 2015, 1)))


#2. Median
median_int = c(summary(int.jan.ts)[3],
               summary(int.feb.ts)[3],
               summary(int.mar.ts)[3],
               summary(int.apr.ts)[3],
               summary(int.may.ts)[3],
               summary(int.jun.ts)[3],
               summary(int.jul.ts)[3],
               summary(int.aug.ts)[3],
               summary(int.sep.ts)[3],
               summary(int.oct.ts)[3],
               summary(int.nov.ts)[3],
               summary(int.dec.ts)[3])
windows()
barplot(median_int, 
        main = "Median Summary Statistics for each month", 
        names.arg = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
        ylab = "Median Value")


