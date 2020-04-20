# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Module 4: Introduction to Machine Learning - Assignment: Linear Regression:

# Analyze the information given in the Video_games dataset and predict the values using linear regression model.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# SET WORKING DIRECTORY

getwd()
setwd('C:/Users/Asus/Desktop/Data Science Master Program/Edureka_Studies/R Certification/004 INTRO TO MACHINE LEARNING')

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# IMPORT DATASET

vid_gam <- read.csv(file.choose())                 # Dataset Copy 01
vid_gam_01 <- read.csv(file.choose())              # Dataset Copy 02

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# DATA EXPLORATION

str(vid_gam)                                 # Gives the STRUCTURE OF DATA
names(vid_gam)                               # Gives the NAME OF COLUMNS
summary(vid_gam)                             # Gives the SUMMARY OF DATA (Columns)
class(vid_gam)                               # Gives the CLASS OF DATA
nrow(vid_gam) # (No. of Rows - 16,719)       # Shows number of ROWS
ncol(vid_gam) # (No. of Columns - 17)        # Shows number of COLUMNS

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# MISSING VALUE TREATMENT

# Check for NA Values

table(is.na(vid_gam))                       # Check for Total of NA Values
sapply(vid_gam, function(x) sum(is.na(x)))  # Check for NA Values as per Column

# NA Values are as follow... Critic_Score = 8582, Critic_Count = 8582 and User_Count = 9129
# We will Treat them individually one column at a time

#----------------------------------

# OPTION 01 (with vid_gam dataset)

install.packages("Hmisc")
library(Hmisc)
table(is.na(vid_gam))

vid_gam$Critic_Count <- impute(vid_gam$Critic_Count, median)
table(is.na(vid_gam))

vid_gam$Critic_Score <- impute(vid_gam$Critic_Score, median)
table(is.na(vid_gam))

vid_gam$User_Count <- impute(vid_gam$User_Count, median)
table(is.na(vid_gam))

sapply(vid_gam, function(x) sum(is.na(x)))  # Check for NA Values as per Column

#----------------------------------

# OPTION 02 (with vid_gam_01 Datset)

install.packages("Hmisc")
library(Hmisc)
table(is.na(vid_gam_01))

vid_gam_01$Critic_Count <- impute(vid_gam_01$Critic_Count, median)
table(is.na(vid_gam_01))

vid_gam_01$Critic_Score <- impute(vid_gam_01$Critic_Score, median)
table(is.na(vid_gam_01))

vid_gam_01$User_Count <- impute(vid_gam_01$User_Count, median)
table(is.na(vid_gam_01))

sapply(vid_gam_01, function(x) sum(is.na(x)))  # Check for NA Values as per Column
sapply(vid_gam, function(x) sum(is.na(x)))  # Check for NA Values as per Column

#----------------------------------

# OPTION 03 - KNN Imputation (with vid_gam_02 Datset)

vid_gam_002 <- read.csv(file.choose())
table(is.na(vid_gam_002))
vid_gam_002 <- knnImputation(vid_gam_002[, !names(vid_gam_002) %in% "medv"])  # perform knn imputation.
anyNA(vid_gam_002)
table(is.na(vid_gam_002))
sapply(vid_gam_002, function(x) sum(is.na(x)))

# Now all of our 3 Dataset are NA Value Free

# We will use vid_gam for further process


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# LINEAR REGRESSION

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Check ABLINE between Dependent and Independent Variables
require(stats)
plot(vid_gam$EU_Sales, vid_gam$Other_Sales)
abline(rec.lm, lwd=2, col='red')
plot(vid_gam$JP_Sales, vid_gam$Other_Sales)
abline(rec.lm, lwd=2, col='red')
plot(vid_gam$NA_Sales, vid_gam$Other_Sales)
abline(rec.lm, lwd=2, col='red')

# Draw a Corelation Plot
library(dplyr)
z <- select(vid_gam, EU_Sales, JP_Sales, NA_Sales, Other_Sales)
str(z)
cr3 <- cor(z)
install.packages("corrplot")
library(corrplot)
corrplot(cr3, type = "lower")
# From our plot we can see that EU & NA sales are highly corelated to Other Sales
# We will reassign z
z <- select(vid_gam, EU_Sales, NA_Sales, Other_Sales)
cr3 <- cor(z)
corrplot(cr3, type = "lower")
rec.lm <- lm(z, data=vid_gam)
summary(rec.lm)

# ****************************************

# We will select ONE Highly Corelated Variable for predicting OTHER SALES by...
cor(vid_gam$EU_Sales, vid_gam$Other_Sales)             # = 0.7227955
cor(vid_gam$NA_Sales, vid_gam$Other_Sales)             # = 0.6386539
# As seen above EU Sale is highly corelated to Other Sale with 72.27%
# So we will use EU Sales for predicting OTHER SALES

# ***************************************

# Assign X & y variable for Linear Model
X <- select(vid_gam, EU_Sales, NA_Sales, Other_Sales)
y <- select(vid_gam, Other_Sales)

# Create Training and Test data

set.seed(100)  # setting seed to reproduce results of random sampling
X_train_Row_Index <- sample(1:nrow(X), 0.7 * nrow(X))  # row indices for training data
X_train_Data <- X[X_train_Row_Index, ]  # model training data
X_test_Data  <- X[-X_train_Row_Index, ]   # test data


# Build the model on training data

lmMod_01 <- lm(EU_Sales ~ Other_Sales, data=X_train_Data) 
summary(lmMod_01)

lmMod_02 <- lm(NA_Sales ~ Other_Sales, data=X_train_Data) 
summary(lmMod_02)

# build the model
othersale_Pred_01 <- predict(lmMod_01, X_test_Data)  # predict distance
othersale_Pred_02 <- predict(lmMod_02, X_test_Data)  # predict distance

# Prediction accuracy and error rates

# EU Sales
actuals_preds_EU <- data.frame(cbind(actuals=X_test_Data$EU_Sales, predicteds=othersale_Pred_01))  # make actuals_predicteds dataframe.
correlation_accuracy_EU <- cor(actuals_preds_EU)
correlation_accuracy_EU

# NA Sales
actuals_preds_NA <- data.frame(cbind(actuals=X_test_Data$NA_Sales, predicteds=othersale_Pred_02))  # make actuals_predicteds dataframe.
correlation_accuracy_NA <- cor(actuals_preds_NA)
correlation_accuracy_NA

#To compare Predicted value with actual value
EU_Sales
plot(X_test_Data$EU_Sales, type="o", lty=1.8, col="green")
lines(othersale_Pred_01, type = "o", col="blue")

NA_Sales
plot(X_test_Data$NA_Sales, type="l", lty=1.8, col="blue")
lines(othersale_Pred_02, type = "l", col="green")

# - - - - - - - - - - - - - -

str(vid_gam)
table(is.na(vid_gam))                       # Check for Total of NA Values
sapply(vid_gam, function(x) sum(is.na(x)))  # Check for NA Values as per Column

vid_gam_03 <- vid_gam

vid_gam_03$Critic_Score <- as.numeric(as.character(vid_gam_03$Critic_Score))   # Convert AGE COlumn to NUMERIC
vid_gam_03$Critic_Count <- as.numeric(as.character(vid_gam_03$Critic_Count))
vid_gam_03$User_Score <- as.numeric(as.character(vid_gam_03$User_Score))
vid_gam_03$User_Count <- as.numeric(as.character(vid_gam_03$User_Count))

library(Hmisc)
vid_gam_03$User_Score <- impute(vid_gam_03$User_Score, median)
str(vid_gam_03)
table(is.na(vid_gam_03$User_Count))

vid_gam_03_new <- vid_gam_03[,-1]
vid_gam_03_new <- vid_gam_03_new[,-5]
str(vid_gam_03_new)

cr <- cor(vid_gam_03_new)
corrplot(cr, type = "lower")
cor(vid_gam_03_new)

set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(vid_gam_03_new), 0.7*nrow(vid_gam_03_new))  # row indices for training data
trainingData <- vid_gam_03_new[trainingRowIndex, ]  # model training data
testData  <- vid_gam_03_new[-trainingRowIndex, ]   # test data

model <- lm(trainingData$Other_Sales~., data=trainingData)
summary(model)
str(trainingData)
trainingData_1 <- trainingData[,-3]
testData_1 <- testData[,-3]
model_2 <- lm(trainingData_1$Other_Sales~., data=trainingData_1)
summary(model_2)
str(testData_1)

vif(mod)

library(car)
vif(model_2)

str(trainingData)
trainingData <- trainingData[,-3]
testData <- testData[,-3]
model_2 <- lm(trainingData$Other_Sales~., data=trainingData)
summary(model_2)
vif(model_2)

predic <- predict(model_2, testData)
predic

plot(testData$Other_Sales, type = "l", lty =1.8, col = "red")
lines(predic,type = "l",col = "blue")


# END OF LINEAR REGRESSION

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -






