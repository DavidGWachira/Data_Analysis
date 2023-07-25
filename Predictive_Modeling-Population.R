setwd("***")

data = read.csv("SiouxFallsPopulation.csv")
head(data)
tail(data)
dim(data) #35 records from 1987 to 2021
sum(is.na(data)) #0 missing values
summary(data)

#Pearson product-moment correlation matrix
cor(data[, c('Year', 'Population')])  

windows()
plot(data, main="Population over Time")
lines(data)

#Approach 1 Statistical regression
#Split data into Train and Test datasets
set.seed(1)
sample = sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))  
trainset  = data[sample, ]
testset   = data[!sample, ]
dim(trainset) 
dim(testset)

#1 LINEAR REGRESSION
#Train dataset
set.seed(2)
#Fitting the model on the train dataset
datamodelTrain = lm(Population ~ Year, data = trainset)  
datamodelTrain
#Output statistics of the train model
summarytrain = summary(datamodelTrain)  
summarytrain #R-Squared = 98.68%

#Predict on the test dataset using the fitted Linear Model
predicted_values = predict(datamodelTrain, newdata = testset)
predicted_values

#Calculate the MSE
MSE = mean((testset$Population - predicted_values)^2)
MSE

#Overall model
model = lm(Population ~ Year, data = data)
model
summarymodel = summary(model)  
summarymodel #R-Squared = 98.7%

#Make prediction for the future (next 30 years)
df = data.frame(Year = seq(from = 2022, to = 2052, by = 1))
projection = predict(model, newdata = df, interval = "confidence")
df$Population = projection[,1]
df$lwr = projection[,2]
df$upr = projection[,3]
df

#Plot the Population projection with confidence intervals
library(ggplot2)
windows()
#Plot actual population data
ggplot(df, aes(x=Year, y=Population)) + 
  geom_point() +
  labs(x="Year", y="Population", title="Population Projection") +
  
  # add confidence intervals to the plot
  geom_ribbon(aes(ymin=lwr, ymax=upr), data=df, alpha=0.3, fill="blue") +
  geom_line(aes(x=Year, y=Population), data=df, color="blue")

#Export the PopulationProjections to csv file
write.csv(df, "PopulationProjection.csv", row.names = FALSE)



#Approach 2 - TS
data.ts = ts(data$Population, start = 1987, end = 2021, freq = 1)
data.ts
summary(data.ts)
updated = read.csv("postcovid.csv")
post_data.ts = ts(updated$Population, start = 2010, end = 2021, freq = 1)
post_data.ts

#Partition the data
nValid = 12
nTrain = length(data.ts) - nValid
train.ts = window(data.ts, start = c(1987,1), end = c(1987,nTrain))
valid.ts = window(data.ts, start = c(1987,nTrain+1), end = c(1987,nTrain+nValid))
length(train.ts)
length(valid.ts)
length(data.ts)
train.ts
valid.ts

#1 LINEAR MODEL
library(forecast)
set.seed(101)
train.lm.trend = tslm(train.ts ~ trend)
summary(train.lm.trend)$adj.r.squared #= 0.981 
pred.lm.trend = forecast(train.lm.trend, h = nValid, level = 0.95)
pred.lm.trend
accuracy(pred.lm.trend, valid.ts)

lm.pred = forecast(data.ts, h = 30, level = 0.95)
lm.pred


#2 EXPONENTIAL SMOOTHING FORECASTS
set.seed(201)
ets = ets(train.ts, model = "ZZZ")
summary(ets)
ets.pred = forecast(ets, h = nValid, level = 0.95)
accuracy(ets.pred, valid.ts)

#Predict the next 30years using ets
ets_new = ets(post_data.ts, model = "ZZZ")
ets_new.pred = forecast(ets_new, h = 30, level = 0.95)
ets_new.pred
windows()
plot(ets_new.pred, main = "Population Projections with 95% Prediction Interval", 
     xlab = "Year", ylab = "Population")
