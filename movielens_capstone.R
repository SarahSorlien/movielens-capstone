library(dplyr)
library(lubridate)
library(stringr)
library(tidyverse)

# function to apply all transformations to the dataframe for the model.
preprocess_data <- function(input_df) {
  # new file df_to_process is created from input file
  df_to_process <- data.frame(input_df)
  # pull release date out of title new file df_to_process is created
  df_to_process$release_date <-
    gsub(".*\\((\\d{4})\\)", "\\1", df_to_process$title)
  df_to_process <- df_to_process %>%
    mutate(release_date = as.integer(release_date))
  # Extract year from timestamp column and store it in the review_date column

  df_to_process$timestamp <- as.POSIXct(df_to_process$timestamp,
    origin = "1970-01-01"
  )
  df_to_process$review_date <- as.integer(year(df_to_process$timestamp))

  # Create a new column called review_release_ interval
  # storing the difference between the review and release years as a factor
  df_to_process <- df_to_process %>%
    mutate(review_release_difference = review_date - release_date)

  #  Because of limited results > 50 years between review and release
  #  the intervals > than 50 years will be aggregated with 50.
  df_to_process <- df_to_process %>%
    mutate(
      review_release_difference =
        ifelse(review_release_difference > 50, 50, review_release_difference)
    )
  # Extract month, time of day,  and day of the week from timestamp
  # yielding new columns in df_to_process
  df_to_process <- df_to_process %>%
    mutate(
      review_month = month(timestamp), # extract month
      review_day_of_week = wday(timestamp, label = TRUE), # extract day of  week
      review_part_of_day =
        if_else(hour(timestamp) < 6 | hour(timestamp) >= 18, "Night", "Day")
    ) # define day (6am-6pm) and night (6pm-6am)

  # User rating frequency transformations
  # count number of reviews per user
  ratings_per_user <- df_to_process %>%
    count(userId, name = "n_ratings")
  # group users into subgroups based on number of reviews
  # Find the value at the 75% quantile shown in exploratory data analysis to
  # stratify users with lower mean ratings
  quantile_75 <- quantile(ratings_per_user$n_ratings, probs = 0.75, na.rm = TRUE)

  # Categorize users into subgroups based on the number of reviews
  ratings_per_user <- ratings_per_user %>%
    mutate(
      user_experience =
        ifelse(n_ratings >= quantile_75, "fanatic", "not fanatic")
    )

  # associate user_experience with user in df_to_process
  df_to_process <- df_to_process %>%
    inner_join(ratings_per_user, by = "userId")
  # remove the n_ratings column resulting from the join
  df_to_process <- df_to_process %>%
    select(-n_ratings)
  # convert the user_experience column to an ordered factor
  df_to_process$user_experience <-
    factor(df_to_process$user_experience,
      ordered = TRUE,
      levels = c("not fanatic", "fanatic")
    )

  # Summary of user_experience feature:
  # exploratory data analysis showed that top 25% of reviewers are significantly
  # different from bottom 75%
  # These quantiles have been used to create a user_experience feature
  # of two factors: not fanatic and fanatic
  # the -n_ratings column is removed from the data set

  # add a column of median rating per user

  df_to_process <- df_to_process %>%
    group_by(userId) %>%
    mutate(user_median_rating = median(rating)) %>%
    ungroup()

  # Movie rating transformations

  # associate movieId and the number of ratings received
  ratings_per_movie <- df_to_process %>%
    count(movieId, name = "n_ratings")

  # Find the value at the 50% and 75% quantiles
  quantile_50 <- quantile(ratings_per_movie$n_ratings, probs = 0.5, na.rm = TRUE)
  quantile_75 <- quantile(ratings_per_movie$n_ratings, probs = 0.75, na.rm = TRUE)

  # Categorize movies into subgroups based on the number of reviews
  ratings_per_movie <- ratings_per_movie %>%
    mutate(movie_experience = ifelse(n_ratings < quantile_50, "some reviews",
      ifelse(n_ratings < quantile_75, "frequently reviewed", "top reviewed")
    ))

  # Exploratory data analysis showed that the bottom two quartiles
  # are not significantly different with regards to mean and variability
  # The code creates 3 categories of rating "experience."
  # These are joined to the data frame
  # associate user_experience with user in df_to_process
  df_to_process <- df_to_process %>%
    inner_join(ratings_per_movie, by = "movieId")
  # remove the n_ratings column resulting from the join
  df_to_process <- df_to_process %>%
    select(-n_ratings)

  # convert the movie_experience column to an ordered factor
  df_to_process$movie_experience <-
    factor(df_to_process$movie_experience,
      ordered = TRUE,
      levels = c("some reviews", "frequently reviewed", "top reviewed")
    )

  # FINAL RESULT movie_experience is a factor with three levels:
  # some reviews, frequently reviewed, top reviewed

  # GENRES transformations to one hot encoding

  # create column for number of genres per film.
  df_to_process <- df_to_process %>%
    mutate(number_of_genres = str_count(genres, "\\|") + 1)


  # create new columns containing logical variables indicating
  # if $genres contains that specific genre
  # Define the genres to analyze from unique genres in edx data

  genres_list <- c(
    "Action", "Adventure", "Animation", "Children", "Comedy", "Crime",
    "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "IMAX", "Musical",
    "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western",
    "(no genres listed)"
  )


  # Create new columns for each genre
  for (genre in genres_list) {
    df_to_process <- df_to_process %>%
      mutate("{genre}" := str_detect(genres, genre))
  }
  # FINAL result is a column for each of 20 possible genres.
  # Logical values indicate if the film is in that genre.

  # Initialize an empty named vector for storing genre mean ratings
  genre_mean_ratings <- numeric(length(genres_list))
  names(genre_mean_ratings) <- genres_list

  # Loop over the genres to calculate their mean ratings
  for (genre in genres_list) {
    genre_mean <- df_to_process %>%
      filter(get(genre) == 1) %>%
      summarise(mean(rating)) %>%
      pull()
    # If a genre is not found in the sample
    # Use the average of all ratings from training set (3.512) rather than NA
    if (is.na(genre_mean)) {
      genre_mean_ratings[genre] <- 3.512
    } else {
      genre_mean_ratings[genre] <- genre_mean
    }
  }

  # The average of all applicable genre mean ratings is used
  df_to_process <- df_to_process %>%
    mutate(
      mean_rating_by_genres =
        rowSums(sapply(
          genres_list,
          function(x) df_to_process[[x]] * genre_mean_ratings[x]
        )) /
          rowSums(sapply(genres_list, function(x) df_to_process[[x]]))
    )

  # Remove unneeded columns, these are not used in the model
  df_to_process <- df_to_process |>
    select(-c(timestamp, title, genres))
  return(df_to_process)
}
# Run the function and store the result
# A copy of the df will be processed and returned
# Originals  remains unchanged

preprocessed_data <- preprocess_data(edx)

preprocessed_final_holdout_test <- preprocess_data(final_holdout_test)

# Preprocessed data from edx set is then used to train the xgboost model

# Load necessary libraries
library(dplyr)
library(caret)
library(xgboost)

# Split data into training and testing sets
trainIndex <- createDataPartition(preprocessed_data$rating,
  p = .75, list = FALSE, times = 1
)
train_data <- preprocessed_data[trainIndex, ]
test_data <- preprocessed_data[-trainIndex, ]

# Prepare the data for XGBoost
x_train <- model.matrix(rating ~ . - 1, train_data)
y_train <- train_data$rating
x_test <- model.matrix(rating ~ . - 1, test_data)
y_test <- test_data$rating

# construct a xgb.DMatrix object that can be passed to XGBoost
xgb_train <- xgb.DMatrix(data = x_train, label = y_train)
xgb_test <- xgb.DMatrix(data = x_test, label = y_test)

# set up watchlist to monitor intermediate results
watchlist <- list(train = xgb_train, test = xgb_test)

# set up parameters for XGBoost.  Goal to reduce RMSE
# eta and max_depth chosen to avoid overfitting
params <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6
)

# train the XGBoost model
set.seed(123)
xgb_fit <- xgb.train(
  params = params,
  data = xgb_train,
  nrounds = 19000, # best number  of iterations on training sessions = 18898
  watchlist = list(train = xgb_train, test = xgb_test),
  early_stopping_rounds = 50, # stop if no improvement in RMSE after 50 rounds
  print_every_n = 10
)

# use the model to predict rating on test set from edx preprocessed_data
predicted_ratings_xgb <- predict(xgb_fit, xgb_test)

# compute RMSE
RMSE_xgb <- sqrt(mean((predicted_ratings_xgb - y_test)^2))
print(RMSE_xgb)

# model results
# > print(RMSE_xgb)
# [1] 0.866901
# Stopping. Best iteration:
#[15749]	train-rmse:0.834898	test-rmse:0.866901


# Test model's ability to predict ratings in final_holdout_test sample

# Prepare final holdout data for XGBoost on preprocessed final_holdout_data
x_final_holdout <-
  model.matrix(rating ~ . - 1, preprocessed_final_holdout_test)
y_final_holdout <- preprocessed_final_holdout_test$rating

# Construct a xgb.DMatrix object for the final holdout data
xgb_final_holdout <- xgb.DMatrix(data = x_final_holdout, label = y_final_holdout)

# Use the model to predict ratings on the final holdout set
predicted_ratings_holdout <- predict(xgb_fit, xgb_final_holdout)

# Compute RMSE for the final holdout set
RMSE_final_holdout <-
  sqrt(mean((predicted_ratings_holdout - y_final_holdout)^2))
print(RMSE_final_holdout)
#> print(RMSE_final_holdout)
# [1] 0.8623448
