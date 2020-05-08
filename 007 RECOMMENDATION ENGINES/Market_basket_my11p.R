
install.packages("arules")
library(arules)

datagr <- read.transactions("C:/Personal/Module 7/groceries.csv", sep = ",")
class(datagr)
summary(datagr)

#Inspect function is used to view the association rules
#inspect(datagr)
inspect(datagr[1:3])

#Item frequency helps in getting support characteristics
itemFrequency(datagr[,1])
itemFrequency(datagr[,1:6])

itemFrequencyPlot(datagr, support=0.08)
itemFrequencyPlot(datagr, topN =5)

#generating association rules

m1 <- apriori(datagr)
summary(m1)

m2 <- apriori(datagr, parameter = list(support=0.007,confidence=0.5))
summary(m2)
inspect(m2[1:10])
inspect(sort(m2,by="lift")[1:5])

m3 <- apriori(datagr, parameter = list(support=0.007,confidence=0.2),
              appearance = list(default="rhs",lhs=c("whole milk","soda")))
summary(m3)
inspect(m3[1:5])

install.packages("arulesViz")
library(arulesViz)
plot(m2)

#User Based Recommendation Engine

install.packages("recommenderlab")
library(recommenderlab)

data("Jester5k")
nratings(Jester5k)
class(Jester5k)

size1 <- object.size(Jester5k)
size2 <- object.size(as(Jester5k,"matrix"))
ratio <- size2/size1
ratio

head(as(Jester5k,"data.frame"))
head(as(Jester5k,"matrix"))[,1:10]
hist(getRatings(Jester5k),breaks = 100)
hist(getRatings(normalize(Jester5k)),breaks = 100)

recommendermodels <- recommenderRegistry$get_entries(datatype = "realRatingMatrix")
names(recommendermodels)

set.seed(3)
id <- sample(x=c(TRUE,FALSE),nrow(Jester5k),prob=c(0.8,0.2),replace=T)
jester_train <- Jester5k[id,]
jester_test <- Jester5k[!id,]

recc_model <- Recommender(jester_train,method="UBCF")
recc_model

recc_predict <- predict(recc_model, newdata = jester_test, n=10)
recc_predict

rec_list <- sapply(recc_predict@items,function(x){colnames(Jester5k)[x]})
rec_list[14:16]

#To find how many recommendations are generated for all the test users
#281 users - zero recommendations
#693 users - 10 recommendations

number_of_items <- sort(unlist(lapply(rec_list, length)),decreasing = TRUE)
table(number_of_items)

#Let's examine the dataset before IBCF clearly
table(rowCounts(Jester5k))
model_dataex <- Jester5k[rowCounts(Jester5k)]
dim(model_dataex)
boxplot(rowMeans(model_dataex))
summary(rowMeans(model_dataex))

#Filtering datasets based on above data driven issues

model_data <- Jester5k[rowCounts(Jester5k)<80]
dim(model_data)
boxplot(rowMeans(model_data))
summary(rowMeans(model_data))
boxplot(rowMeans(model_data[rowMeans(model_data)<=7 & rowMeans(model_data)>-5]))

#examine the rating distributions for first 100 users now
submodeldata <- model_data[1:100]
image(submodeldata)

#Building the model again
set.seed(3)
idd <- sample(x=c(TRUE,FALSE),nrow(model_data),prob=c(0.8,0.2),replace=T)
model_train <- model_data[idd,]
model_test <- model_data[!idd,]

#k is the number of neighbors to be considered while calculating the similarity values
ibcf_model <- Recommender(model_train,method="IBCF", parameter=list(k=30))
ibcf_details <- getModel(ibcf_model)
str(ibcf_details)

#n is the parameter for number of recommendations to be generated for each users
ibcf_predict <- predict(ibcf_model, model_test, n=5)
ibcf_predict

#predictions for the first user
recc_user_1 <- ibcf_predict@items[[1]]
recc_user_1

jokes_user_1 <- ibcf_predict@itemLabels[recc_user_1]
jokes_user_1








