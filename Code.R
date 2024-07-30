# Load required libraries
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)
library(dplyr)

# Set working directory and retrieve data
setwd("D:/Amrita/OneDrive - Amrita university/Amrita/Projects/R_Lab/Code/Dataset")
movie_data <- read.csv("movies.csv", stringsAsFactors = FALSE)
rating_data <- read.csv("ratings.csv")

# Overview the summary and structure of the data
summary(movie_data)
head(movie_data)
summary(rating_data)
head(rating_data)

# Data pre-processing
# Create a one-hot encoding matrix for movie genres
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors = FALSE)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[, 1], '[|]', type.convert = TRUE), stringsAsFactors = FALSE)
colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")

genre_mat1 <- matrix(0, 10330, 18)
genre_mat1[1, ] <- list_genre
colnames(genre_mat1) <- list_genre

for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1, ] == movie_genre2[index, col])
    genre_mat1[index + 1, gen_col] <- 1
  }
}

genre_mat2 <- as.data.frame(genre_mat1[-1, ], stringsAsFactors = FALSE)
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[, col] <- as.integer(genre_mat2[, col])
}

# Create a search matrix for searching movies by genre
SearchMatrix <- cbind(movie_data[, 1:2], genre_mat2[])

# Create a rating matrix and convert it to a realRatingMatrix
ratingMatrix <- dcast(rating_data, userId ~ movieId, value.var = "rating", na.rm = FALSE)
ratingMatrix <- as.matrix(ratingMatrix[, -1])
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")

# Explore recommendation models
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description")

# Implement Item-Based Collaborative Filtering (IBCF)
recommendation_model$IBCF_realRatingMatrix$parameters

# Compute similarities between users
similarity_mat <- similarity(ratingMatrix[1:4, ], method = "cosine", which = "users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User Similarities")

# Compute similarities between movies
movie_similarity <- similarity(ratingMatrix[, 1:4], method = "cosine", which = "items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity), main = "Movie Similarities")

# Analyze rating values and movie views
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values)
Table_of_Ratings <- table(rating_values)

movie_views <- colCounts(ratingMatrix)
table_views <- data.frame(movie = names(movie_views), views = movie_views)
table_views <- table_views[order(table_views$views, decreasing = TRUE), ]
table_views$title <- NA
for (index in 1:nrow(table_views)) {
  table_views[index, 3] <- as.character(subset(movie_data, movie_data$movieId == table_views[index, 1])$title)
}

# Visualize top movies by views
ggplot(table_views[1:6, ], aes(x = reorder(title, -views), y = views)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  geom_text(aes(label = views), vjust = -0.3, size = 3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total Views of the Top Films") +
  xlab("Movie Title") +
  ylab("Number of Views")

# Visualize heatmap of movie ratings
image(ratingMatrix[1:20, 1:25], axes = FALSE, main = "Heatmap of the first 25 rows and 25 columns")

# Data preparation
movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50, colCounts(ratingMatrix) > 50]

minimum_movies <- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies, colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of the top users and movies")

# Visualize distribution of average ratings per user
average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill = I("steelblue"), col = I("red")) +
  ggtitle("Distribution of the average rating per user")

# Data normalization
normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)
image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies, colCounts(normalized_ratings) > minimum_users],
      main = "Normalized Ratings of the Top Users")

# Data binarization
binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)
good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies, colCounts(movie_ratings) > binary_minimum_users],
      main = "Heatmap of the top users and movies")

# Split the dataset into training and test sets
sampled_data <- sample(x = c(TRUE, FALSE), size = nrow(movie_ratings), replace = TRUE, prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

# Build the recommendation system using IBCF
recommendation_system <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters
recommen_model <- Recommender(data = training_data, method = "IBCF", parameter = list(k = 30))

# Explore the recommendation model
model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items], main = "Heatmap of the first rows and columns")
sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)
sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill = I("steelblue"), col = I("red")) + ggtitle("Distribution of the column count")

# Generate top recommendations
top_recommendations <- 10
predicted_recommendations <- predict(object = recommen_model, newdata = testing_data, n = top_recommendations)

# Recommendation for the first user
user1 <- predicted_recommendations@items[[1]]
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1

for (index in 1:length(movies_user1)) {
  movies_user2[index] <- as.character(subset(movie_data, movie_data$movieId == movies_user1[index])$title)
}
print(movies_user2)

# Matrix with recommendations for each user
recommendation_matrix <- sapply(predicted_recommendations@items, function(x) { as.integer(colnames(movie_ratings)[x]) })
recommendation_matrix[, 1:4]

# Distribution of the number of items for IBCF
number_of_items <- factor(table(recommendation_matrix))
chart_title <- "Distribution of the Number of Items for IBCF"
qplot(number_of_items, fill = I("steelblue"), col = I("red")) + ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(as.integer(names(number_of_items_top)), number_of_items_top)
for (i in 1:4) {
  table_top[i, 1] <- as.character(subset(movie_data, movie_data$movieId == table_top[i, 1])$title)
}

colnames(table_top) <- c("Movie Title", "No. of Items")
head(table_top)

# Function to recommend top movies based on genre
recommend_movies_by_genre <- function(genre, top_n = 10) {
  
  # Filter movies by genre
  filtered_movies <- movie_data %>% 
    filter(grepl(genre, genres))
  
  # Join with ratings to get ratings for filtered movies
  filtered_ratings <- rating_data %>% 
    inner_join(filtered_movies, by = "movieId")
  
  # Calculate average rating for each movie
  movie_avg_ratings <- filtered_ratings %>% 
    group_by(movieId, title) %>% 
    summarize(average_rating = mean(rating, na.rm = TRUE)) %>% 
    ungroup()
  
  # Sort movies by average rating
  top_movies <- movie_avg_ratings %>% 
    arrange(desc(average_rating)) %>% 
    slice(1:top_n)
  
  return(top_movies)
}

# Function to plot top recommended movies
plot_recommended_movies <- function(recommendations) {
  ggplot(recommendations, aes(x = reorder(title, -average_rating), y = average_rating)) +
    geom_bar(stat = "identity", fill = 'steelblue') +
    geom_text(aes(label = round(average_rating, 2)), vjust = -0.3, size = 3.5) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Top Recommended Movies") +
    xlab("Movie Title") +
    ylab("Average Rating")
}

# Example usage
genre_input <- "Comedy"
top_n <- 10

recommended_movies <- recommend_movies_by_genre(genre_input, top_n)
print(recommended_movies)

# Plot the recommendations
plot_recommended_movies(recommended_movies)

