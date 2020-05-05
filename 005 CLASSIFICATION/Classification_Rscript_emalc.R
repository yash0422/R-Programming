#####################################
#   Logistics Regression
#####################################

# Y can take values between 0 and 1 whereas betax can take values between -infi to +infi
# Hence logit function becomes - log(p/1-p) = beta0 + beta1*x + error - log odds
#if x changes by one unit how odds changes
#It does not uses OLS (Ordinary Least Square) for parameter estimation. Instead, it uses maximum likelihood estimation (MLE).

diabetic<-read.csv("Diabetes.csv")

summary(diabetic)
str(diabetic)

diabetic$Is_Diabetic<-as.factor(diabetic$Is_Diabetic)
#cdplot(diabetic$Is_Diabetic~diabetic$BMI, data=diabetic)

#Build a training and testing set
set.seed(4)
id<-sample(2,nrow(diabetic),prob = c(0.7,0.3),replace = T)
trainset<-diabetic[id==1,]
testset<-diabetic[id==2,]
class(trainset)

#Build logistic regression model
model<-glm(trainset$Is_Diabetic~. ,data = trainset,family = "binomial")
summary(model)
exp(0.085879)
(exp(0.085879)-1)*100
(exp(-0.004797)-1)*100

#diabetic_re <- diabetic[,-c(3,4,5,8)]

#Build a training and testing set
#set.seed(4)
#id<-sample(2,nrow(diabetic_re),prob = c(0.7,0.3),replace = T)
#trainset<-diabetic_re[id==1,]
#testset<-diabetic_re[id==2,]
#class(trainset)

#model_1<-glm(trainset$Is_Diabetic~. ,data = trainset,family = "binomial")
#summary(model_1)

#hist(diabetic$Age)
#hist(log(diabetic$Age))

#Null deviance - how well response variable is predicted by model that includes only intercept
#Residual deviance - how well response variable is predicted by model with inclusion of independent variables
#The analogous metric of adjusted R² in logistic regression is AIC. AIC is the measure of fit which penalizes model for the number of model coefficients. Therefore, we always prefer model with minimum AIC value.

predvalues<-predict(model,newdata = testset,type = "response")
predvalues

table(Actualvalues = testset$Is_Diabetic, Predictedvalues = predvalues>0.5)

Accuracy <- (132+47)/(132+20+35+47)
# Accuracy is 76.4%

table(Actualvalues = testset$Is_Diabetic, Predictedvalues = predvalues>0.3)

Accuracy <- (111+64)/(111+64+41+18)
# Accuracy is 74.7%

table(Actualvalues = testset$Is_Diabetic, Predictedvalues = predvalues>0.7)

Accuracy <- (141+33)/(141+33+11+49)
# Accuracy is 74.3%

install.packages("ROCR")
library(ROCR)
ROCpred <-prediction(predvalues, testset$Is_Diabetic)
ROCperf <- performance(ROCpred, "tpr", "fpr")
plot(ROCperf, col = "blue", print.cutoffs.at = seq(0.1, by = 0.1),text.adj = c(0.2,1.7), cex = 0.7, colorize=TRUE)

table(Actualvalues = testset$Is_Diabetic, Predictedvalues = predvalues>0.4)

Accuracy <- (121+52)/(121+52+31+30)
# Accuracy is 73.9%

#####################################
#   Decision Tree
#####################################

getwd()
setwd('C:/Users/Asus/Desktop/Data Science Master Program/Edureka_Studies/R Certification/005 CLASSIFICATION')
diabetic<-read.csv("Diabetes.csv")

summary(diabetic)

diabetic$Is_Diabetic<-as.factor(diabetic$Is_Diabetic)
view(diabetic)
install.packages("rpart")
library(rpart)

#Build a training and testing set
set.seed(4)
id<-sample(2,nrow(diabetic),prob = c(0.7,0.3),replace = T)
trainset<-diabetic[id==1,]
testset<-diabetic[id==2,]
class(trainset)
view(trainset)

#Build decision tree model
model = rpart(trainset$Is_Diabetic~. ,data = trainset)
summary(model)
plot(model, margin=0.1)
text(model, use.n = TRUE, pretty = TRUE, cex=0.8)

temp <- trainset[trainset$glucose_conc<154.5 & trainset$BMI<26.35,]
table(temp$Is_Diabetic)

predvalues<-predict(model,newdata = testset,type = "class")
predvalues

class(predvalues)

diabetic$predvalues <- predvalues

install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)
confusionMatrix(table(predvalues, testset$Is_Diabetic))

#####################################
#   Random Forest
#####################################

diabetic<-read.csv("Diabetes.csv")

summary(diabetic)

diabetic$Is_Diabetic<-as.factor(diabetic$Is_Diabetic)

install.packages("randomForest")
library(randomForest)

#Build a training and testing set
set.seed(4)
id<-sample(2,nrow(diabetic),prob = c(0.7,0.3),replace = T)
trainset<-diabetic[id==1,]
testset<-diabetic[id==2,]
class(trainset)

#Build random forest model
model<-randomForest(trainset$Is_Diabetic~. ,data = trainset, ntree=200)
model
plot(model)

library(data.table)
library(ggplot2)

# Get OOB data from plot and coerce to data.table
oobData = as.data.table(plot(model))

# Define trees as 1:ntree
oobData[, trees := .I]

# Cast to long format
oobData2 = melt(oobData, id.vars = "trees")
setnames(oobData2, "value", "error")

# Plot using ggplot
ggplot(data = oobData2, aes(x = trees, y = error, color = variable)) + geom_line()

predvalues<-predict(model,newdata = testset,type = "class")
predvalues

install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)
confusionMatrix(table(predvalues, testset$Is_Diabetic))

hist(treesize(model))

varImp(model)
varImpPlot(model,sort=TRUE, type=2)
importance(model)
varUsed(model)

# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(trainset$Is_Diabetic~. ,data = trainset, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, testset, type = "class")
  a[i-2] = mean(predValid == testset$Is_Diabetic)
}

a

plot(3:8,a)


#Build naive bayes model

diabetic<-read.csv("Diabetes.csv")

summary(diabetic)

diabetic$Is_Diabetic<-as.factor(diabetic$Is_Diabetic)

#Build a training and testing set
set.seed(4)
id<-sample(2,nrow(diabetic),prob = c(0.7,0.3),replace = T)
trainset<-diabetic[id==1,]
testset<-diabetic[id==2,]
class(trainset)

model<-naiveBayes(trainset$Is_Diabetic~. ,data = trainset)
model

predvalues<-predict(model,newdata = testset,type = "class")
predvalues

confusionMatrix(table(predvalues, testset$Is_Diabetic))

#Build support vector machines

library(e1071)
data(iris)

summary(iris)
str(iris)

plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species)
plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species)

#Build a training and testing set
set.seed(4)
id<-sample(2,nrow(iris),prob = c(0.7,0.3),replace = T)
trainset<-iris[id==1,]
testset<-iris[id==2,]
class(trainset)

#kernel can be linear/polynomial/radial or sigmoid
#cost called as c value - determine the width of the margin, larger the c value, smaller the width
#scale for normalization to avoid bias

model<-svm(Species~. ,data = trainset, kernel = "linear", cost = 0.1)
summary(model)
plot(model, trainset,Petal.Width ~ Petal.Length)
plot(model, trainset,Sepal.Width ~ Sepal.Length)

predvalues<-predict(model,newdata = testset,type = "class")
predvalues

library(caret)
confusionMatrix(table(predvalues, testset$Species))

#Tune for best cost function

set.seed(1)
obj <- tune(svm, Species~., data = trainset, 
            ranges = list(gamma = 2^(-1:1), cost= c(0.001,0.01,0.1,1,5,10,100)),
            tunecontrol = tune.control(sampling = "fix"), kernel='linear')
summary(obj)
best.model <- obj$best.model
summary(best.model)

predvalues_best <-predict(best.model,newdata = testset,type = "class")
predvalues
confusionMatrix(table(predvalues_best, testset$Species))

#Let's change the kernel scenarios

model_poly<-svm(Species~. ,data = trainset, kernel = "polynomial", cost = 0.1)
summary(model_poly)

plot(model_poly, trainset,Petal.Width ~ Petal.Length)

predvalues<-predict(model_poly,newdata = testset,type = "class")
predvalues
confusionMatrix(table(predvalues, testset$Species))

set.seed(1)
obj <- tune(svm, Species~., data = trainset, 
            ranges = list(gamma = 2^(-1:1), cost= c(0.001,0.01,0.1,1,5,10,100)),
            tunecontrol = tune.control(sampling = "fix"), kernel='polynomial')
summary(obj)
best.model <- obj$best.model
summary(best.model)

predvalues_best <-predict(best.model,newdata = testset,type = "class")
predvalues
confusionMatrix(table(predvalues_best, testset$Species))
