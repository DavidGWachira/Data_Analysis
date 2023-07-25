setwd("***")

Stock=read.csv("Stock.csv")
dim(Stock)  #Checks the dimensions of the data set
sapply(Stock, function(x) sum(is.na(x)))  #Check the missing values
Stock[1:4,]  #Display the first 4 rows of the data set
tail(Stock)  #Display the last 6 rows of the data set
names(Stock) #variable names
summary(Stock) #Display the data statistics
boxplot(Stock$Sales)  #Box plot for five number summaries
boxplot(Stock$Inventory)
boxplot(Stock$Price)

#Pearson product-moment correlation matrix
cor(Stock[, c('Sales', 'Inventory', 'Price')])  

#Check the descriptive statistics
library(fBasics)
basicStats(Stock$Sales)  
basicStats(Stock$Inventory)
basicStats(Stock$Price)
#Notice the Inventory and Sales data is skewed

#Plot histograms to show the skewness of the original data
hist(Stock$Sales, col = 'blue', main = 'Original_Sales') 
hist(Stock$Inventory, col = 'red', main = 'Original_Inventory')
hist(Stock$Price, col = 'green', main = 'Original_Price')

#Transform Inventory and Sales data using Square Root
sqrt_sales = sqrt(Stock$Sales)
sqrt_inventory = sqrt(Stock$Inventory)

#Plot histograms for the Square Root-transformed data
hist(sqrt_sales, col = 'blue', main = 'Square Root-transformed Sales') 
hist(sqrt_inventory, col = 'red', main = 'Square Root-transformed Inventory')

#Create a new dataframe with the new variables
NewStock = data.frame(Stock, sqrt_sales, sqrt_inventory)
head(NewStock)

#Check the skewness of the Square Root-transformed data
basicStats(NewStock$sqrt_sales)
basicStats(NewStock$sqrt_inventory)
basicStats(NewStock$Price)

#Drop the columns that won't be used for calculations
NewStock1 = subset(NewStock, select = -c(Date, Sales, Inventory))
head(NewStock1)

#Pearson product-moment correlation matrix
cor(NewStock1[, c('sqrt_sales', 'sqrt_inventory', 'Price')]) 


#Split data into Train and Test datasets
set.seed(1)
sample = sample(c(TRUE, FALSE), nrow(NewStock1), replace=TRUE, prob=c(0.7,0.3))  
trainset  = NewStock1[sample, ]
testset   = NewStock1[!sample, ]
dim(trainset)
dim(testset)


#METHOD 1: LINEAR REGRESSION
#Train dataset
set.seed(2)
#Fitting the model on the train dataset
NewstockmodelTrain = lm(sqrt_inventory ~ sqrt_sales + Price, data = trainset)  
NewstockmodelTrain
#Output statistics of the train model
summarytrain = summary(NewstockmodelTrain)  
summarytrain
#Calculate the MSE
mean(summarytrain$residuals^2)  
#The MSE is 259.0256
#RSquared 9.976%
#The coefficients are 30.063 + 1.156sqrt_sales - 2.011Price

#Test dataset
set.seed(3)
#Fitting the model on the test dataset
NewstockmodelTest = lm(sqrt_inventory ~ sqrt_sales + Price, data = testset)  
NewstockmodelTest
summarytest = summary(NewstockmodelTest)  
#Output statistics of the model/summary
summarytest 
mean(summarytest$residuals^2)  
#The MSE is 240.9124
#RSquared 16.42%
#The coefficients are 30.063 + 1.3858sqrt_sales + 0.7329Price



#METHOD 2: REGRESSION TREE
library(rpart)
library(rpart.plot)

#Train dataset 
set.seed(4)
#build the initial decision tree
tree = rpart(sqrt_inventory ~ sqrt_sales + Price, data=trainset, control=rpart.control(cp=.0001))

#identify best cp value to use
best = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

#produce a pruned tree based on the best cp value
pruned_tree = prune(tree, cp=best)

#plot the pruned tree
prp(pruned_tree)


#Evaluate the regression tree model
eval_results = function(true, predicted, df) 
{
  SSE = sum((predicted - true)^2)
  SST = sum((true - mean(true))^2)
  R_square = 1 - (SSE / SST)
  RMSE = sqrt(SSE/nrow(df))
  MSE = (RMSE^2)
  
  #Model performance metrics
  data.frame(RMSE = RMSE, Rsquare = R_square, MSE = MSE)
}

#Predicting and evaluating the model on train data
predictions_train_cart = predict(pruned_tree, data = trainset)
eval_results(trainset$sqrt_inventory, predictions_train_cart, trainset)
#The MSE is 150.7634
#RSquared 47.60%

#Predicting and evaluating the model on test data
predictions_test_cart = predict(pruned_tree, newdata = testset)
eval_results(testset$sqrt_inventory, predictions_test_cart, testset)
#The MSE is 169.492
#RSquared 41.2%


#Interraction terms
summary(lm(sqrt_inventory ~ sqrt_sales + Price, data = NewStock1))
summary(lm(sqrt_inventory ~ sqrt_sales * Price, data = NewStock1))


#Plot the diagnostic plots 
par(mfrow = c(2,2)) 
plot(NewstockmodelTest)

par(mfrow = c(1,1))
plot(NewstockmodelTest, 1)  #Plot Residuals vs Fitted
plot(NewstockmodelTest, 3)  #Plot Scale-Location
plot(NewstockmodelTest, 2)  #Plot Normal Q-Q
plot(NewstockmodelTest, 5)  #Plot Residuals vs Leverage

library(car)
vif(NewstockmodelTest)  #Calculate the VIF
