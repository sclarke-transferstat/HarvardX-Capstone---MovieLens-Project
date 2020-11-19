#title: "HarvardX Capstone - MovieLens Project"
author: "Stephen Clarke"


knitr::opts_chunk$set(echo = TRUE)

#Load all potential libraries that may be required
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(dslabs)
data(movielens)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
ds_theme_set()
library(Lahman)
library(lattice)
library(e1071)
library(caret)
library(knitr)
library(tidyr)
library(stringr)
memory.limit(56000)
# memory limit increased to improve processing ability

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)


kable(head(edx))

kable(summary(edx))


n_distinct(edx$userId)

n_distinct(edx$movieId)


any(is.na(edx))


Avg_Rating <- mean(edx$rating)
edx %>% ggplot(aes(rating)) +
  geom_histogram(col='purple4',bins=10,fill='turquoise4') + 
  scale_x_continuous(breaks = seq(0.5,5,0.5)) +
  geom_vline(xintercept=Avg_Rating, col='red4',linetype='solid') +
  xlab("Rating") +
  ylab("No. of Ratings") +
  ggtitle("Frequency of Each Rating")


Genre_by_num_ratings <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(ratings_per_genre = n()) %>%
  arrange(desc(ratings_per_genre))

head(Genre_by_num_ratings, n=10)

edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(ratings_per_genre = n()) %>%
  arrange(desc(ratings_per_genre)) %>%
  head(n=10) %>%
  ggplot(aes(genres, ratings_per_genre)) + geom_col(aes(fill = genres)) +
  xlab("Genre") +
  ylab("No. of Ratings")+
  ggtitle("No. Ratings per Genre")


Genre_by_avg_rating <-edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(rating_of_genre = mean(rating)) %>%
  arrange(desc(rating_of_genre))

head(Genre_by_avg_rating, n=10)

edx %>% 
  separate_rows(genres, sep = "\\|") %>% 
  select(genres, rating) %>% 
  group_by(genres) %>% 
  filter(genres %in% c("Drama","Comedy","Action","Thriller","Adventure")) %>% 
  ggplot(aes(x = genres, y = rating, col = genres)) +
  geom_boxplot()+
  xlab("Genre") +
  ylab("Spectrum of Ratings Given")+
  ggtitle("Ratings of Top 5 Genres")


edx <- edx %>% mutate(Date_of_Rating = as.Date(as.POSIXct(timestamp, origin="1970-01-01")))

edx <- edx %>% mutate(title = str_trim(title)) %>%  
  extract(title, c('title', 'Year_Released'), regex = '(.*)\\s\\((\\d+)\\)', convert = TRUE)


edx %>% 
  separate_rows(genres, sep = "\\|") %>% 
  select(genres, Year_Released, rating) %>%      
  group_by(genres, Year_Released) %>% 
  filter(genres %in% c("Drama","Comedy","Action","Thriller","Adventure")) %>% 
  summarise(rating_of_genre_by_Year = mean(rating)) %>%
  ggplot(aes(x = Year_Released, y = rating_of_genre_by_Year, col = genres)) +
  geom_line() +
  xlab("Year of Film Release") +
  ylab("Average Film Rating") +
  ggtitle("Film Ratings by Genre and Release Date")


edx <- edx %>% mutate(Year_of_Rating = as.numeric(format(Date_of_Rating,'%Y')))

edx %>% 
  separate_rows(genres, sep = "\\|") %>% 
  select(genres, Year_of_Rating, rating) %>% 
  group_by(genres, Year_of_Rating) %>% 
  filter(genres %in% c("Drama","Comedy","Action","Thriller","Adventure")) %>% 
  summarise(rating_of_genre_by_Year_Rated = mean(rating)) %>%
  ggplot(aes(x = Year_of_Rating, y = rating_of_genre_by_Year_Rated, col = genres)) +
  geom_line() +
  xlab("Date of Film Rating") +
  ylab("Average Film Rating") +
  ggtitle("Film Ratings by Genre Over Time")


top10_movies <- edx %>% select(title, rating) %>% group_by(title) %>% summarise(ratings_per_film = n(), rating_of_film = mean(rating)) %>% filter(ratings_per_film > 20000) %>% arrange(desc(rating_of_film)) %>% head(10)

kable(top10_movies)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm = TRUE))
}


mu <- mean(edx$rating)
mu


naive_rmse <- RMSE(validation$rating, mu)

naive_rmse

model_results <- data.frame(model = "Naive Baseline Model", RMSE = naive_rmse)

model_results


movie_avg <- edx %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu))


movie_avg


predicted_ratings <- mu + validation %>% 
  left_join(movie_avg, by = 'movieId') %>% 
  .$b_i

#.$ means pull() function

movie_rating_bias <- RMSE(validation$rating, predicted_ratings)


model_results <- bind_rows(model_results, data_frame(model = "Movie Rating Bias Model", RMSE = movie_rating_bias))

model_results


user_avg <- edx %>% 
  left_join(movie_avg, by = 'movieId') %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))


user_avg


predicted_ratings <- validation %>% 
  left_join(movie_avg, by = 'movieId') %>% 
  left_join(user_avg, by = 'userId') %>% 
  mutate(b_i_b_u = mu + b_i + b_u) %>%
  .$b_i_b_u

user_bias <- RMSE(validation$rating, predicted_ratings)


model_results <- bind_rows(model_results, data_frame(model = "Movie and User Bias Model", RMSE = user_bias))

model_results


edx_movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()


validation %>% count(movieId) %>%
  left_join(movie_avg) %>%
  left_join(edx_movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  head(n=10)


lambdas <- seq(0, 10, 0.25)

mu <- mean(edx$rating)
just_the_sum <- edx %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())


rmses <- sapply(lambdas, function(l){
  predicted_ratings <- validation %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, validation$rating))
})
  
  qplot(lambdas, rmses)  
  lambdas[which.min(rmses)]
  
  min(rmses)
  
  
  
  lambdas <- seq(0, 10, 0.25)
  
  rmses <- sapply(lambdas, function(l){
    
    mu <- mean(edx$rating)
    
    b_i <- edx %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    
    b_u <- edx %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
    predicted_ratings <- 
      validation %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    
    return(RMSE(predicted_ratings, validation$rating))
  })
  
  
  qplot(lambdas, rmses)
  lambda <- lambdas[which.min(rmses)]
  lambda
  
  
  model_results <- bind_rows(model_results, data_frame(model = "Regularised Movie and User Bias Model", RMSE = min(rmses)))
  
  model_results
  