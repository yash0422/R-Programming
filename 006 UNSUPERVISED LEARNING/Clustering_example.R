
##Clustering another example

CustomerData<- read.csv(file.choose())

Customerdata <- data.frame(CustomerData)

summary(Customerdata)

sum(is.na(Customerdata))

mycustomer <- Customerdata[,-c(1,2)]
set.seed(123)
k.max <- 10
wss <- sapply(1:k.max, function(k){kmeans(mycustomer,k, nstart = 1)$tot.withinss})
plot(1:k.max,wss, type="b", frame=FALSE, xlab = "Number of clusters k" , ylab = 'total within clusters sum of squares')
abline(v=5, lty=4)

km_customer <- kmeans(mycustomer, 5) #apply k means clustering
km_customer

###Wine dataset clustering

library(HDclassif)
data(wine)

colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash', 
                    'Alcalinity', 'Magnesium', 'Phenols', 
                    'Flavanoids', 'Nonflavanoids',
                    'Proanthocyanins', 'Color', 'Hue', 
                    'Dilution', 'Proline')

mywine <- wine[,-c(1)]
set.seed(123)
k.max <- 10
wss <- sapply(1:k.max, function(k){kmeans(mywine,k, nstart = 1)$tot.withinss})
plot(1:k.max,wss, type="b", frame=FALSE, xlab = "Number of clusters k" , ylab = 'total within clusters sum of squares')
abline(v=4, lty=4)

km_wine <- kmeans(mywine, 3) #apply k means clustering
km_wine








