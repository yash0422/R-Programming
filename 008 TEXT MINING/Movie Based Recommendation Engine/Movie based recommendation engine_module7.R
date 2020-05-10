
#Movie based recommendation system

#In this script, let's develop a collaborative filtering recommender (CFR) system for recommending movies.

#The basic idea of CFR systems is that, if two users share the same interests in the past, e.g. they liked the same book or the same movie, they will also have similar tastes in the future. 
#If, for example, user A and user B have a similar purchase history and user A recently bought a book that user B has not yet seen, the basic idea is to propose this book to user B.
#The dataset used was from MovieLens, and is publicly available at http://grouplens.org/datasets/movielens/latest. 
#In order to keep the recommender simple, we used the smallest dataset available (ml-latest-small.zip), which at the time of download contained 100,000 ratings and 3,600 tag applications applied to 9,000 movies by 600 users. Last updated 9/2018. 

#The data are contained in four files: links.csv, movies.csv, ratings.csv and tags.csv. I only use the files movies.csv and ratings.csv to build a recommendation system.

movies <- read.csv("movies.csv",stringsAsFactors=FALSE)
ratings <- read.csv("ratings.csv")
table(ratings$userId)

library(reshape2)

#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds

library(recommenderlab)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

# Exploring values of ratings:
vector_ratings <- as.vector(ratingmat@data)
unique(vector_ratings) # what are unique values of ratings

table_ratings <- table(vector_ratings) # what is the count of each rating value
table_ratings

# Visualize the rating:
vector_ratings <- vector_ratings[vector_ratings != 0] # rating == 0 are NA values
vector_ratings <- factor(vector_ratings)

qplot(vector_ratings) + 
  ggtitle("Distribution of the ratings")

# Exploring viewings of movies:
views_per_movie <- colCounts(ratingmat) # count views for each movie

table_views <- data.frame(movie = names(views_per_movie),
                          views = views_per_movie) # create dataframe of views
table_views <- table_views[order(table_views$views, 
                                 decreasing = TRUE), ] # sort by number of views
table_views <- as.data.frame(table_views)

movies2 <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]
rownames(movies2) <- NULL
movies3 <- merge(table_views, movies2, by.x = "movie", by.y = "movieId")

movies3 <- movies3[order(-movies3$views),]

# Method: UBCF
# Similarity Calculation Method: Cosine Similarity

# Determine how similar the first four users are with each other
# create similarity matrix
similarity_users <- similarity(ratingmat[1:4, ], 
                               method = "cosine", 
                               which = "users")
as.matrix(similarity_users)

# compute similarity between
# the first four movies
similarity_items <- similarity(ratingmat[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(similarity_items)


#Create UBFC Recommender Model. UBCF stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat, 
                                 method = "UBCF", 
                                 param=list(method="Cosine"))

model_details <- getModel(recommender_model)
model_details$data

recom <- predict(recommender_model, 
                 ratingmat[1], 
                 n=10) #Obtain top 10 recommendations for 1st user in dataset

recom


recom_list <- as(recom, 
                 "list") #convert recommenderlab object to readable list
recom_list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in 1:10){
  recom_result[i] <- as.character(subset(movies, 
                                         movies$movieId == as.integer(recom_list[[1]][i]))$title)
}
recom_result
# Let's evaluate prediction for 2 user

recom <- predict(recommender_model, 
                 ratingmat[2], 
                 n=10) #Obtain top 10 recommendations for 2nd user in dataset
recom
recom_list <- as(recom, 
                 "list") #convert recommenderlab object to readable list
#Obtain recommendations
recom_result <- matrix(0,10)
for (i in 1:10){
  recom_result[i] <- as.character(subset(movies, 
                                         movies$movieId == as.integer(recom_list[[1]][i]))$title)
}
recom_result








