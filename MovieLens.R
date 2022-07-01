##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################
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

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))
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
# rm(): Remove Objects from Memory in R Programming
rm(dl, ratings, movies, test_index, temp, movielens, removed)

##########################################################
if(!require(rafalib)) install.packages("rafalib", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("rafalib", repos = "http://cran.us.r-project.org")
library(rafalib)
library(lubridate)

# 1. Data Exploration

# dimension of the datasets
dim(edx)
dim(validation)
# review head and data type in the datasets
edx %>% as_tibble()
validation %>% as_tibble() 

# number of unique users and unique movies
edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

# check missing values
# (insight) no missing value. No action needed.
sapply(edx, function(x) sum(is.na(x)))
sapply(validation, function(x) sum(is.na(x)))

# (insight) Imagine if we set each row as each unique users, each column as a unique movie. Then the whole question we are solving is to fill in the blanks in this matrix
# Below visualization shows how sparse this matrix is.
users <- sample(unique(edx$userId), 500)
rafalib::mypar()
edx %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% 
  select(sample(ncol(.), 500)) %>% 
  as.matrix() %>% 
  t(.) %>%
  image(1:500, 1:500,. , xlab="Movies", ylab="Users")

# Ratings - histogram
# (insight) In general, half star ratings are less common than whole star ratings.
edx %>% 
  ggplot(aes(rating)) + 
  geom_histogram(bins = 10, color = "black") + 
  ggtitle("Ratings Histogram")
# Ratings - table view - sort by number of ratings
edx %>% 
  count(rating) %>% 
  arrange(desc(n))

# Movie Distribution by Number of Ratings
# (insight) some movies get rated more than others
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  xlab("Number of Ratings") + ylab("Number of Movies") +
  scale_x_log10() + 
  ggtitle("Movie Distribution by Number of Ratings")
# Movie Distribution by Average Ratings
# (insight) some movies get rated higher than others
edx %>% 
  group_by(movieId) %>% 
  summarize (avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating)) + 
  geom_histogram(bins = 30, color = "black") + 
  xlab("Average Rating") + ylab("Number of Movies") + 
  ggtitle("Movie Distribution by Average Ratings")
# (insight) above shows in linear model, we should consider movie effects

# User Distribution by Number of Ratings
# (insight) some users are more active than others at rating movies
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  xlab("Number of Ratings") + ylab("Number of Users") +
  scale_x_log10() + 
  ggtitle("User Distribution by Number of Ratings")
# Users Distribution by Average Ratings
# (insight) some users give higher ratings than other users
edx %>% 
  group_by(userId) %>% 
  summarize (avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating)) + 
  geom_histogram(bins = 30, color = "black") + 
  xlab("Average Rating") + ylab("Number of Users") + 
  ggtitle("Users Distribution by Average Ratings")
# (insight) above shows in linear model, we should consider user effects

# Genre Distribution by Number of Ratings
# (insight) some genres receive more ratings than others
edx %>% 
  count(genres) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  xlab("Number of Ratings") + ylab("Number of Genre") +
  scale_x_log10() + 
  ggtitle("Genre Distribution by Number of Ratings")
# Genre Distribution by Average Ratings
# (insight) some genres receive higher ratings than others
edx %>% 
  group_by(genres) %>% 
  summarize (avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating)) + 
  geom_histogram(bins = 30, color = "black") + 
  xlab("Average Rating") + ylab("Number of Genres") + 
  ggtitle("Genres Distribution by Average Ratings")
# (insight) above shows in linear model, we should consider genre effects. 

# Number of Ratings by Genre (splitted) 
edx_generes_collaps <- edx %>%
  separate_rows(genres, sep = "\\|", convert = TRUE)
edx_generes_collaps %>% 
  ggplot(aes(genres)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) + 
  xlab("Genre") + ylab("Number of Ratings") +
  ggtitle("Number of Ratings by Genre (splitted)")
# Average Rating by Genre (splitted) 
edx_generes_collaps %>%
  group_by(genres) %>%
  summarize (avg_rating = mean(rating)) %>%
  ggplot(aes(genres,avg_rating)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) + 
  xlab("Genre") + ylab("Average Rating") +
  ggtitle("Average Rating by Genre (splitted)")
# (insight) above reenforce the idea that in linear model, we should consider genre effects. 
# (insight) Also note due to less variability on Average Rating by splitted Genre (each individual Genre), we might better use the original combined genere column without splitting it

# Data Cleaning
# So far our data exploration covered user, movie, and genres. In order to further explore the effects of rating timestamps, we need to clean the data.
# convert timestamp to Date type. Extract movie release year from movie title. And calculate the year between movie release and rating time stamp.
edx <- edx %>%
  mutate(title_temp = str_trim(title), date = as.Date(as_datetime(timestamp))) %>%
  # year_after_release: the year between movie release and rating time stamp
  extract(title_temp, c("title_temp", "releaseyear"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = FALSE) %>%
  mutate(releaseyear = if_else(str_length(releaseyear) > 4,
                               as.integer(str_split(releaseyear, "-", simplify = TRUE)[1]),
                               as.integer(releaseyear))) %>%
  mutate(year_after_release = isoyear(date)-releaseyear) %>%
  select(-title_temp, -timestamp)
edx %>% as_tibble()
# do the same to validation dataset (I'm not adding/deleting any rows.)
validation <- validation %>%
  mutate(title_temp = str_trim(title), date = as.Date(as_datetime(timestamp))) %>%
  extract(title_temp, c("title_temp", "releaseyear"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = FALSE) %>%
  mutate(releaseyear = if_else(str_length(releaseyear) > 4,
                               as.integer(str_split(releaseyear, "-", simplify = TRUE)[1]),
                               as.integer(releaseyear))) %>%
  mutate(year_after_release = isoyear(date)-releaseyear) %>%
  select(-title_temp, -timestamp)
validation %>% as_tibble()

# Weekly Number of Ratings
# (insight) not much insight.
edx %>% 
  mutate(date = round_date(date, unit="week")) %>% 
  group_by(date) %>%
  summarize(wk_no_rating = n()) %>%
  ggplot(aes(date, wk_no_rating)) +
  geom_line() +
  geom_smooth() +
  xlab("Date") + ylab("Weekly Number of Ratings") +
  ggtitle("Weekly Number of Ratings")
# Weekly Rating Average
# (insight) not much insight.
edx %>% 
  mutate(date = round_date(date, unit="week")) %>% 
  group_by(date) %>%
  summarize(wk_avg = mean(rating)) %>%
  ggplot(aes(date, wk_avg)) +
  geom_line() +
  geom_smooth() +
  xlab("Date") + ylab("Weekly Rating Average") +
  ggtitle("Weekly Rating Average")
# Number of Ratings by Years After Movie Release
# (insight) the number of ratings for a new released movie topped in the first 5 years, and then dramatically reduce over the time.
edx %>% 
  group_by(year_after_release) %>%
  summarize(wk_no_rating = n()) %>%
  ggplot(aes(year_after_release, wk_no_rating)) +
  geom_line() +
  xlab("Years After Movie Release") + ylab("Number of Ratings") +
  ggtitle("Number of Ratings by Years After Movie Release")
# Average Rating by Years After Movie Release
# (insight) Since the whole movie rating data came with Web 2.0 and is something less than 30 years old, I'm focusing on the trend within 30 years after movie release.
# (insight) During that period of time, the average rating tends to go up as time passes. Therefore we should consider rating time effects.
edx %>% 
  group_by(year_after_release) %>%
  summarize(wk_avg = mean(rating)) %>%
  ggplot(aes(year_after_release, wk_avg)) +
  geom_line() +
  geom_smooth() +
  xlab("Years After Movie Release") + ylab("Average Rating") +
  ggtitle("Average Rating by Years After Movie Release")

# 2. Model Building

# Split data sets and prepare for model building
# set.seed(755) # if using R 3.5 or earlier
set.seed(755, sample.kind = "Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]
# To make sure we don't include movies, users, genres, year_after_release in the test set that do not appear in the training set, we remove these entries using the semi_join function:
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set, by = "genres") %>%
  semi_join(train_set, by = "year_after_release")
# Add rows removed from test_set back into train_set
test_set_removed <- anti_join(edx[test_index,], test_set)
train_set <- rbind(train_set, test_set_removed)

# we will use RMSE as the loss function, here we write a custom function for that
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Model#1: Just the average
mu_hat <- mean(train_set$rating)
# (insight) If we predict all unknown ratings with mu_hat, we obtain the following RMSE=1.06, meaning our typical error is larger than one star, which is not good prediction.
model_1_rmse <- RMSE(test_set$rating, mu_hat)
# create a results table.
rmse_results <- tibble(method = "Just the average", RMSE = model_1_rmse)
rmse_results %>% knitr::kable()

# Model#2.1: Movie Effect Model (b_i)
# Since lm() is too slow and probably crash your laptop as complexity grows, we will calculate using code below.
mu <- mean(train_set$rating)
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
# by plotting a chart, we see it proves each movie's b_i (bias) varies substantially
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))
# Add the result to the results table
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_2_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, tibble(method="Movie Effect Model", RMSE = model_2_1_rmse ))
rmse_results %>% knitr::kable()

# Model#2.2: Movie + User Effects Model (b_i + b_u)
# by plotting a chart of the average rating for user u for those that have rated 100 or more movies. It proves that there is substantial variability across users as well.
train_set %>% 
  group_by(userId) %>% 
  filter(n()>=100) %>%
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
# Add the result to the results table
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
model_2_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie + User Effects Model", RMSE = model_2_2_rmse))
rmse_results %>% knitr::kable()

# Model#2.3: Movie + User + Genre Effects Model (b_i + b_u + b_g)
# by plotting a chart of the average rating for genre g for those that have 1000 or more movies. It proves that there is substantial variability across genres as well.
train_set %>% 
  group_by(genres) %>% 
  filter(n()>=1000) %>%
  summarize(b_g = mean(rating)) %>% 
  ggplot(aes(b_g)) + 
  geom_histogram(bins = 30, color = "black")
genre_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))
# Add the result to the results table
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)
model_2_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie + User + Genre Effects Model", RMSE = model_2_3_rmse))
rmse_results %>% knitr::kable()

# Model#2.4: Movie + User + Genre + Rate Time Effects Model (b_i + b_u + b_g + b_t)
rate_time_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  group_by(year_after_release) %>%
  summarize(b_t = mean(rating - mu - b_i - b_u - b_g))
# Add the result to the results table
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(rate_time_avgs, by='year_after_release') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_t) %>%
  pull(pred)
model_2_4_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie + User + Genre + Rate Time Effects Model", RMSE = model_2_4_rmse))
rmse_results %>% knitr::kable()

# Model#3.1: Regularized Movie Effect Model (b_i)
lambdas <- seq(0, 10, 0.25)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
# lambda_b_i=2.25
qplot(lambdas, rmses)  
lambda_b_i <- lambdas[which.min(rmses)]
model_3_1_rmse <- min(rmses)
rmse_results <- bind_rows(rmse_results, data_frame(method="Regularized Movie Effect Model", RMSE = model_3_1_rmse))
rmse_results %>% knitr::kable()

# Model#3.2: Regularized Movie + User Effect Model (b_i + b_u)
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
# lambda_b_u=4.75
qplot(lambdas, rmses)  
lambda_b_u <- lambdas[which.min(rmses)]
model_3_2_rmse <- min(rmses)
rmse_results <- bind_rows(rmse_results, data_frame(method="Regularized Movie + User Effect Model", RMSE = model_3_2_rmse))
rmse_results %>% knitr::kable()

# Model#3.3: Regularized Movie + User + Genre Effect Model (b_i + b_u + b_g)
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  b_g <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
# lambda_b_g=4.75
qplot(lambdas, rmses)  
lambda_b_g <- lambdas[which.min(rmses)]
model_3_3_rmse <- min(rmses)
rmse_results <- bind_rows(rmse_results, data_frame(method="Regularized Movie + User + Genre Effect Model", RMSE = model_3_3_rmse))
rmse_results %>% knitr::kable()

# Model#3.4: Regularized Movie + User + Genre + Rate Time Effect Model (b_i + b_u + b_g + b_t)
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  b_g <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  b_t <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(year_after_release) %>%
    summarize(b_t = sum(rating - mu - b_i - b_u - b_g)/(n()+l))
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_t, by = "year_after_release") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_t) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
# lambda_b_t=5
qplot(lambdas, rmses)  
lambda_b_t <- lambdas[which.min(rmses)]
model_3_4_rmse <- min(rmses)
rmse_results <- bind_rows(rmse_results, data_frame(method="Regularized Movie + User + Genre + Rate Time Effect Model", RMSE = model_3_4_rmse))
rmse_results %>% knitr::kable()
# We will pick the model with the lowest RMSE, which is Regularized Movie + User + Genre + Rate Time Effect Model

# 3. Results

# quick recap on all the parameters we picked
lambda_b_i
lambda_b_u
lambda_b_g
lambda_b_t
# use the final model developed based on edx data set (I did not use validation set so far at all.)
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda_b_i))
user_reg_avgs <- train_set %>%
  left_join(movie_reg_avgs, by="movieId") %>%
  group_by(userId) %>% 
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda_b_u)) 
genre_reg_avgs <- train_set %>%
  left_join(movie_reg_avgs, by="movieId") %>%
  left_join(user_reg_avgs, by="userId") %>%
  group_by(genres) %>% 
  summarize(b_g = sum(rating - b_i - mu - b_u)/(n()+lambda_b_g)) 
ratetime_reg_avgs <- train_set %>%
  left_join(movie_reg_avgs, by="movieId") %>%
  left_join(user_reg_avgs, by="userId") %>%
  left_join(genre_reg_avgs, by="genres") %>%
  group_by(year_after_release) %>% 
  summarize(b_t = sum(rating - b_i - mu - b_u - b_g)/(n()+lambda_b_t)) 
# implement the model on validation set, and see result
# RMSE=0.8648385, which has achieved highest target: RMSE < 0.86490 
predicted_ratings <- validation %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(user_reg_avgs, by = "userId") %>%
  left_join(genre_reg_avgs, by = "genres") %>%
  left_join(ratetime_reg_avgs, by = "year_after_release") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_t) %>%
  pull(pred)
RMSE(predicted_ratings, validation$rating)

