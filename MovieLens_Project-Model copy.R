# script of code chunks and inline code by section of MovieLens_Project.Rmd

################################################################################
# METHODS SECTION CODE
################################################################################
# Data Exploration/Cleaning/Analysis
####################################

############
# code chunk to create train (edx) and validation (validation) sets
# supplied by edX Data Science: Capstone Course 

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# remove unneeded objects leving just edx and validation dfs
rm(dl, ratings, movies, test_index, temp, movielens, removed)

#end code chunk
###############

#############
# inline code to report number of rows in edx in text
noquote(format(nrow(edx), big.mark = ","))

#############
# inline code to report number of rows in validation in text
noquote(format(nrow(validation), big.mark = ","))

############
# inline code to produce Table 1 - Unique numbers of Users, movies,
#   Ratings, Timestamps, Titles and Genres
noquote(format(n_distinct(edx$userId), big.mark = ","))
noquote(format(n_distinct(edx$movieId), big.mark = ","))
noquote(format(n_distinct(edx$rating), big.mark = ","))
noquote(format(n_distinct(edx$timestamp), big.mark = ","))
noquote(format(n_distinct(edx$title), big.mark = ","))
noquote(format(n_distinct(edx$genres), big.mark = ","))

#############
# inline code to supply number of unique genres in text
noquote(format(n_distinct(edx$genres), big.mark = ","))

############
# code chunk to create df by separating genres in edx,
#   determine number of ratings in each genre, and
#   arrange genres in descending order
genres_edx <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# end code chunk
################

###########
# code chunk to determine percentage of rows in edx
#   with pipe_separated genres
number_of_solo_genre_movies <- edx %>% 
  group_by(genres) %>% 
  filter(!grepl(".*\\|.*", genres)) %>% 
  ungroup() %>% 
  count()
pecent_multiple_genre_movies <- 100 - (100 * number_of_solo_genre_movies$n/nrow(edx))

#end code chunk
###############

#############
# inline code reporting percentage of rows in edx
#   with pipe-separated genres in text
round(pecent_multiple_genre_movies)

#############
# inline code to produce part of Table 2 - list of genres
#   in descending order of number of ratings

# list of genres
noquote(genres_edx$genres[1])
noquote(genres_edx$genres[2])
noquote(genres_edx$genres[3])
noquote(genres_edx$genres[4])
noquote(genres_edx$genres[5])
noquote(genres_edx$genres[6])
noquote(genres_edx$genres[7])
noquote(genres_edx$genres[8])
noquote(genres_edx$genres[9])
noquote(genres_edx$genres[10])
noquote(genres_edx$genres[11])
noquote(genres_edx$genres[12])
noquote(genres_edx$genres[13])
noquote(genres_edx$genres[14])
noquote(genres_edx$genres[15])
noquote(genres_edx$genres[16])
noquote(genres_edx$genres[17])
noquote(genres_edx$genres[18])
noquote(genres_edx$genres[19])
noquote(genres_edx$genres[20])

# list of number of ratings for each genre
noquote(format(genres_edx$n[1], big.mark = ","))
noquote(format(genres_edx$n[2], big.mark = ","))
noquote(format(genres_edx$n[3], big.mark = ","))
noquote(format(genres_edx$n[4], big.mark = ","))
noquote(format(genres_edx$n[5], big.mark = ","))
noquote(format(genres_edx$n[6], big.mark = ","))
noquote(format(genres_edx$n[7], big.mark = ","))
noquote(format(genres_edx$n[8], big.mark = ","))
noquote(format(genres_edx$n[9], big.mark = ","))
noquote(format(genres_edx$n[10], big.mark = ","))
noquote(format(genres_edx$n[11], big.mark = ","))
noquote(format(genres_edx$n[12], big.mark = ","))
noquote(format(genres_edx$n[13], big.mark = ","))
noquote(format(genres_edx$n[14], big.mark = ","))
noquote(format(genres_edx$n[15], big.mark = ","))
noquote(format(genres_edx$n[16], big.mark = ","))
noquote(format(genres_edx$n[17], big.mark = ","))
noquote(format(genres_edx$n[18], big.mark = ","))
noquote(format(genres_edx$n[19], big.mark = ","))
noquote(format(genres_edx$n[20], big.mark = ","))

############
# code chunk to create tidy train (edx_tidy) and
#   validation (validation_tidy - final hold-out test set) sets
# MovieLens data "genres" variable is separated into one observation
#   for each genre and new variable renamed "single_genres"
# MovieLens data "timestamp" variable is reformatted to
#     y-m-d format and new variable renamed "date_rated"
if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t",
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")),
                          "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% 
  mutate(movieId = as.numeric(movieId),
         title = as.character(title),
         genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# format timestamp to y-m-d and change column name to date_rated
library(lubridate)
movielens <- movielens %>% 
  mutate(timestamp = format(as.Date(as_datetime(timestamp)),
                            "%Y-%m-%d"))

setnames(movielens, "timestamp", "date_rated")

# make a separate row for each genre and change column name to single_genres
tidy_movielens <- movielens %>% 
  separate_rows(genres, sep = "\\|")

setnames(tidy_movielens, "genres", "single_genres")

# Validation_tidy set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = tidy_movielens$rating,
                                  times = 1,
                                  p = 0.1,
                                  list = FALSE)
edx_tidy <- tidy_movielens[-test_index,]
temp <- tidy_movielens[test_index,]

# Make sure userId and movieId in validation_tidy set are
#   also in edx_tidy set
validation_tidy <- temp %>% 
  semi_join(edx_tidy, by = "movieId") %>%
  semi_join(edx_tidy, by = "userId")

# Add rows removed from validation_tidy set back into edx_tidy set
removed <- anti_join(temp, validation_tidy)
edx_tidy <- rbind(edx_tidy, removed)

rm(dl, ratings, movies, test_index, temp,
   movielens, tidy_movielens, removed)

#end code chunk
###############

#############
# inline code to report number of rows in edx_tidy in text
noquote(format(nrow(edx_tidy), big.mark = ","))

#############
# inline code to report number of rows in validation_tidy in text
noquote(format(nrow(validation_tidy), big.mark = ","))

#############
# inline code to produce Table 3 - Unique numbers of Users, movies,
#   Ratings, Dates_Rated, Titles and Single_Genres
noquote(format(n_distinct(edx_tidy$userId), big.mark = ","))
noquote(format(n_distinct(edx_tidy$movieId), big.mark = ","))
noquote(format(n_distinct(edx_tidy$rating), big.mark = ","))
noquote(format(n_distinct(edx_tidy$date_rated), big.mark = ","))
noquote(format(n_distinct(edx_tidy$title), big.mark = ","))
noquote(format(n_distinct(edx_tidy$single_genres), big.mark = ","))

#############
# inline code to report unique number of dates_rated in text
noquote(format(n_distinct(edx_tidy$date_rated), big.mark = ","))

#############               
# inline code to report unique number of single_genres in text         
noquote(format(n_distinct(edx_tidy$single_genres), big.mark = ","))             


################################################################################
# METHODS SECTION CODE
################################################################################
# Visualization
###############

#######################
# Figure 1 - code chunk
# histogram plot of number of ratings each movie received
# log 10 scale used on y axis to show detail across x axis
edx_tidy %>% 
  group_by(movieId) %>% 
  count() %>% 
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 3000) +
  labs(x = "Number of Ratings a Movie Received",
       y = "Number of Movies (log10 scale)",
       title = "Figure 1 - Number of Ratings for Movies") +
  scale_y_log10(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(0, 100000))

#end code chunk
###############

#######################
# Figure 2 - code chunk
# histogram plot of number of ratings given by each user
# log 10 scale used on y axis to show detail across x axis
edx_tidy %>% 
  group_by(userId) %>% 
  count() %>% 
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 300) +
  labs(x = "Number of Ratings Submitted by Each User",
       y = "Number of Users (log10 scale)",
       title = "Figure 2 - Number of Ratings Sumitted by Users") +
  scale_y_log10(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(0, 10000))

#end code chunk
###############

#######################
# Figure 3 - code chunk
# histogram plot of number of ratings for date_rated
# log 10 scale used on y axis to show detail across x axis
edx_tidy %>% 
  group_by(date_rated) %>% 
  count() %>% 
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1000) +
  labs(x = "Number of Ratings on Date Rated",
       y = "Number of Dates Rated (log10 scale)",
       title = "Figure 3 - Number of Ratings on Dates Rated") +
  scale_y_log10(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(0, 50000))

#end code chunk
###############

#######################
# Figure 4 - code chunk
# bar plot of number of ratings in each rating category with vertical line
#   plot of mean rating in edx_tidy 
edx_tidy %>% 
  group_by(rating) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = rating, y = n)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5),
                     labels = seq(0.5, 5, 0.5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Ratings",
       y = "Number of Ratings",
       title = "Figure 4 - Number of Ratings in Each Rating Category")  +
  geom_vline(xintercept = mean(edx_tidy$rating), linetype = "dashed",
             color = "red") +
  geom_text(aes(x = 2.9,
                label = "mean rating for edx_tidy", y = 5.9*10^6),
            color = "red", size = 3)

#end code chunk
###############

#######################
# Figure 5 - code chunk
# plot the number of ratings in each genre category
#  create smaller data frame of just single_genres, rating and title
single_genres_edx_tidy <- edx_tidy %>% 
  select(single_genres, rating, title)

# arrange single_genres_edx_tidy alphabetically
alpha_single_genres <- single_genres_edx_tidy[order(single_genres_edx_tidy$single_genres),] 

# create vector of single_genre names starting with (no genres listed) and then
#   alphabetical thereafter
single_genres_by_name <-  c("(no genres listed)", "Action", "Adventure",
                            "Animation", "Children","Comedy", "Crime",
                            "Documentary", "Drama", "Fantasy", "Film-Noir",
                            "Horror", "IMAX", "Musical", "Mystery",
                            "Romance", "Sci-Fi", "Thriller", "War", "Western")

# create vector of number of ratings in each single_genre
rating_count_for_single_genres <- sapply(single_genres_by_name, function(g) {
  sum(str_detect(alpha_single_genres$single_genres, g))
})

#create tibble data frame of single_genres and number of single_genres
tab_single_genres_num_ratings <- tibble(single_genres = single_genres_by_name,
                                        num_ratings = rating_count_for_single_genres)

# bar plot of number of ratings for each single_genres category 
tab_single_genres_num_ratings %>% 
  ggplot(aes(single_genres, num_ratings)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Single_Genres",
       y = "Number of Ratings",
       title = "Figure 5 - Number of Ratings in Each Single_Genres Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#end code chunk
###############

#######################
# Figure 6 - code chunk
# histogram plot of the number of average ratings for movies with
# vertical blue line plot for the mean of the average ratings for movies
# and vertical red line plot for the mean rating for edx_tidy
avg_ratings_movies <- edx_tidy %>% group_by(movieId) %>%
  summarise(movieId, n = n(),
            avg_rating = mean(rating)) %>% 
  slice_head(n = 1)

avg_ratings_movies %>% 
  ggplot(aes(avg_rating)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5),
                     labels = seq(0.5, 5, 0.5)) +
  labs(x = "Average Rating for Each Movie",
       y = "Number of Ratings",
       title = "Figure 6 - Number of Average Ratings for Movies") +
  geom_vline(xintercept = mean(edx_tidy$rating), linetype = "dashed",
             color = "red") +
  geom_text(aes(x = 4.2, y = 775,
                label = "mean rating for edx_tidy"), color = "red",
            size = 3) +
  geom_vline(xintercept = mean(avg_ratings_movies$avg_rating),
             linetype = "longdash", color = "blue") +
  geom_text(aes(x = 2.25, y = 725,
                label = "mean of average ratings for movies"), color = "blue",
            size = 3)

#end code chunk
###############

#######################
# Figure 7 - code chunk
# histogram plot of number of average ratings for users with
# vertical blue line plot for the mean of the average ratings for users
# and vertical red line plot for the mean rating for edx_tidy
avg_ratings_users <- edx_tidy %>% group_by(userId) %>%
  summarise(userId, n = n(),
            avg_rating = mean(rating)) %>% 
  slice_head(n = 1)

avg_ratings_users %>% 
  ggplot(aes(avg_rating)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5),
                     labels = seq(0.5, 5, 0.5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Average Rating for Each User",
       y = "Number of Ratings",
       title = "Figure 7 - Number of Average Ratings for Users") +
  geom_vline(xintercept = mean(edx_tidy$rating), linetype = "dashed",
             color = "red") +
  geom_text(aes(x = 2.95, y = 6200,
                label = "mean rating for edx_tidy"), color = "red",
            size = 2.75) +
  geom_vline(xintercept = mean(avg_ratings_users$avg_rating),
             linetype = "longdash", color = "blue") +
  geom_text(aes(x = 4.4, y = 6500,
                label = "mean of average ratings for users"), color = "blue",
            size = 2.75)

#end code chunk
###############

#######################
# Figure 8 - code chunk
# histogram plot of number of average ratings for dates rated with
# vertical blue line plot for the mean of the average ratings for date_rated
# and vertical red line plot for the mean rating for edx_tidy
avg_ratings_date_rated <- edx_tidy %>% group_by(date_rated) %>%
  summarise(date_rated, n = n(),
            avg_rating = mean(rating)) %>% 
  slice_head(n = 1)

avg_ratings_date_rated %>% 
  ggplot(aes(avg_rating)) +
  geom_histogram(bins = 80) +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5),
                     labels = seq(0.5, 5, 0.5),
                     limits = c(0.5, 5)) +
  labs(x = "Average Rating for Each Date_Rated",
       y = "Number of Ratings",
       title = "Figure 8 - Number of Average Ratings for Dates Rated") +
  geom_vline(xintercept = mean(edx_tidy$rating), linetype = "dashed",
             color = "red") +
  geom_text(aes(x = 3, y = 750,
                label = "mean rating for edx_tidy"), color = "red",
            size = 2.75) +
  geom_vline(xintercept = mean(avg_ratings_date_rated$avg_rating),
             linetype = "longdash", color = "blue") +
  geom_text(aes(x = 4.4, y = 770,
                label = "mean of average ratings for dates rated"),
            color = "blue", size = 2.75)

#end code chunk
###############

#############
# inline code to produce Table 4 - Means and Variances of
#   Average Ratings Distributions by Movie, User and Date_Rated

# means
round(mean(avg_ratings_movies$avg_rating),2)
round(mean(avg_ratings_users$avg_rating), 2)
round(mean(avg_ratings_date_rated$avg_rating), 2)

# variances
round(var(avg_ratings_movies$avg_rating),3)
round(var(avg_ratings_users$avg_rating), 3)
round(var(avg_ratings_date_rated$avg_rating), 3)

#############
# inline code used to report Variances of Average Ratings
#   Distributions by Movie, User and Date_Rated in text
round(var(avg_ratings_movies$avg_rating),3)
round(var(avg_ratings_users$avg_rating), 3)
round(var(avg_ratings_date_rated$avg_rating), 3)

#############
# inline code used to report average rating for edx_tidy
#   dataset in text
round(mean(edx_tidy$rating), 2)


################################################################################
# METHODS SECTION CODE
################################################################################
# Model Approach and Design
###########################

############
# code chunk to create training and test sets for edx_tidy to train model
set.seed(1, sample.kind="Rounding")
edx_tidy_test_index <- createDataPartition(edx_tidy$rating,
                                           times = 1,
                                           p = 0.2,
                                           list = FALSE)
edx_tidy_train <- edx_tidy[-edx_tidy_test_index,]
edx_tidy_test <- edx_tidy[edx_tidy_test_index,]

# remove users and movies from edx_tidy_test that are not
#   in edx_tidy_training
edx_tidy_test <- edx_tidy_test %>% 
  semi_join(edx_tidy_train, by = "movieId") %>% 
  semi_join(edx_tidy_train, by = "userId")

# end code chunk
################

############
# code chunk to calculate the RMSE of rating predictions
#   compared to actual ratings
RMSE <- function(predicted_ratings, actual_ratings){
  sqrt(mean((predicted_ratings - actual_ratings)^2))
}

# end code chunk
################

############
# code chunk to train prediction model with average movie ratings
#   by movieId and the individual user effect which is
#   approximated by taking the average of the residuals
#   obtained by removing the average movie rating from the
#   actual rating
options(digits = 5)
movie_avgs_train <- edx_tidy_train %>% 
  group_by(movieId) %>% 
  summarise(mu_i = mean(rating))

user_avgs_train <- edx_tidy_train %>% 
  left_join(movie_avgs_train, by = "movieId") %>% 
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mu_i))

predictions_movie_avgs_and_user_effect_train <- edx_tidy_test %>% 
  left_join(movie_avgs_train, by = "movieId") %>% 
  left_join(user_avgs_train, by = "userId") %>% 
  mutate(predictions = mu_i + b_u) %>% 
  pull(predictions)

RMSE_mu_i_and_b_u_train <- RMSE(predictions_movie_avgs_and_user_effect_train,
                                edx_tidy_test$rating)

# end code chunk
################

################################################################################
# RESULTS
################################################################################

############
# code chunk to run model,
#   make movie rating predictions with validation_tidy data, and
#   calculate the RMSE of the predicted movie ratings relative to
#   the actual movie ratings in validation_tidy
options(digits = 5)
movie_avgs_model <- edx_tidy %>% 
  group_by(movieId) %>% 
  summarise(mu_i = mean(rating))

user_avgs_model <- edx_tidy %>% 
  left_join(movie_avgs_model, by = "movieId") %>% 
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mu_i))

model_predictions_validation <- validation_tidy %>% 
  left_join(movie_avgs_model, by = "movieId") %>% 
  left_join(user_avgs_model, by = "userId") %>% 
  mutate(predictions = mu_i + b_u) %>% 
  pull(predictions)

model_RMSE_validation <- RMSE(model_predictions_validation,
                              validation_tidy$rating)

# end code chunk
################

#############
# inline code to report the RMSE of the trained model in the text
RMSE_mu_i_and_b_u_train

#############
# inline code to report the RMSE of the model tested on
#   validation_tidy date in the text
model_RMSE_validation


# end code script
################################################################################
