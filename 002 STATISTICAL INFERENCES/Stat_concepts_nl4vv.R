
#Let's see now iris dataset
data(iris)
summary(iris)
str(iris)

hist(iris$Sepal.Width)
summary(iris$Sepal.Length)

var(iris$Sepal.Length)
sd(iris$Sepal.Length)
mean(iris$Sepal.Length)

hist(iris$Sepal.Width)
write.csv(iris, "example.csv")

iris$Sepal.Width
sort(iris$Sepal.Width)

sd(iris$Sepal.Length)

iris[1,1]
iris[,1]
iris[1,]
iris[c(2,4,6),]
iris[,c(2,4,5)]
iris[iris$Species=="setosa",]
iris[iris$Sepal.Length>4.0,]


