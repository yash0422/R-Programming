
#####Missing data Imputations and Handling

data ("BostonHousing", package="mlbench")  # initialize the data  # load the data
original <- BostonHousing  # backup original data
summary(original)
table(is.na(original))

# Introduce missing values

BostonHousing[sample(1:nrow(BostonHousing), 40), "rad"] <- NA
BostonHousing[sample(1:nrow(BostonHousing), 40), "ptratio"] <- NA
head(BostonHousing)
summary(BostonHousing)
table(is.na(BostonHousing))

#Four ways to handle missing values

#Deleting the observations
boston_na <- na.omit(BostonHousing)
summary(boston_na)

#Imputation with mean, median and mode
install.packages("Hmisc")
library(Hmisc)
impute(BostonHousing$ptratio, mean)  # replace with mean
impute(BostonHousing$ptratio, median)  # median
impute(BostonHousing$ptratio, mode)  # mode
impute(BostonHousing$ptratio, 20)  # replace specific number

hist(original$ptratio)
hist(impute(BostonHousing$ptratio, mean))



