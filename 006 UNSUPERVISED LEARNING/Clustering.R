
##K-means clustering

#Group the movies to clusters on basis of facebook likes

getwd()
setwd("C:/Users/Asus/Desktop/Data Science Master Program/Edureka_Studies/R Certification/006 UNSUPERVISED LEARNING")

mov <- read.csv("movie_metadata.csv", header=TRUE)
colSums(is.na(InsuranceData))
mov <- na.omit(mov)
mov$movie_title <- gsub(pattern = " ", replacement = "", mov$movie_title)
colnames(mov)
facebook <- (mov[,c(5,6,8,14,25)]) #take facebook like column

#facebook_normalized <- scale(facebook)

km <- kmeans(facebook, 5) #apply k means clustering
km

km$cluster
clust <- km$cluster
mov <- cbind(mov, clust)
mov1 <- mov[,c("movie_title", "clust")]
mov1

#to order the movie based on cluster values
grpmov <- mov1[order(mov1$clust),]
grpmov

mydata <- facebook
set.seed(123)
k.max <- 10
wss <- sapply(1:k.max, function(k){kmeans(mydata,k, nstart = 1)$tot.withinss})
plot(1:k.max,wss, type="b", frame=FALSE, xlab = "Number of clusters k" , ylab = 'total within clusters sum of squares')
abline(v=3, lty=4)

km <- kmeans(facebook, 4) #apply k means clustering
km

install.packages("animation")
library(animation)

km_ani <- kmeans.ani(facebook, 3) #apply k means clustering


##C-means clustering

cl <- cmeans(director1, 5)
options(scipen=10)

format(director1, scientific=FALSE)
plot(director1, col=cl$cluster,color=TRUE,las=1, xlab="gross", ylab="budget")
points(cl$centers, col=1:4, pch=1, cex=20)

#Hierarachical clustering
# Facebook likes

mov <- read.csv("movie_metadata.csv", header=TRUE)
mov <- na.omit(mov)
mov$movie_title <- sub(pattern = "Â", replacement = "", mov$movie_title)
colnames(mov)
facebook <- mov[,c(5,6,8,14)] #take facebook like column
submov <- facebook[1:1000,]
rownames(submov) <- mov$movie_title[1:1000]

d <- dist(as.matrix(submov))
hc <- hclust(d)
plot(hc,cex=0.8,las=1)
hc





