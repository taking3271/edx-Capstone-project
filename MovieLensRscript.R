## Trevor King
## MovieLens Project 
## HarvardX PH125.9x - Data Science: Capstone

#########################################################
# Movie Rating Predictions using the MovieLens 10M datset
#########################################################

# Introduction

## Dataset

##########################################################
# Create edx and final_holdout_test sets (edx supplied code)
# You will use the following code to generate your datasets.
# Develop your algorithm using the edx set.
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(knitr)
library(scales)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# setting the timeout
options(timeout = 120)

# Download file name (check to see if it exists already and download if not)
dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Checking for files and unzipping if not found
ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

# Read the file line by line and split the line at "::" into separate fields 
ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
# Set the column names
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

# Read the file line by line and split the line at "::" into separate fields 
movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
# Set the column names
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

# Join the Ratings dataframe to the Movies Dataframe by the column "MovieId"
movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
# Need to check the R version since the syntax is different for R version>3.6
# Set the random seed value
if(base::getRversion()>'3.6'){ set.seed(1, sample.kind="Rounding") }else{set.seed(1) }

# Partition the data by creating an index where 10% is for the hold out data and
# 90% for the remain
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,] # Final holdout test data

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

# Save the Unique Genres Groups
UniqGenGroups<-edx%>%dplyr::select(genres)%>%unique()

# Unique Genres
Genres<-UniqGenGroups%>%tidyr::separate_rows(genres,sep ="\\|")%>%unique()

# Cleanup removing temparary dataframes
rm(dl, ratings, movies, test_index, temp, movielens, removed)

#### Methods and Analysis ####

### Data Analysis ###

### Dimenions of the dataset
dims<-data.frame(edx=dim(edx),final_holdout_test=dim(final_holdout_test),
                 row.names=c('Number of Rows', 'Number of Columns'))
                 

# Get the Total numbers of unique ratings, users. and genres
Table1<-data.frame(
  Ratings=sum(edx$rating!=0),
  Movies = n_distinct(edx$movieId),
  Users = n_distinct(edx$userId),
  `Genre Combonations Count` = n_distinct(UniqGenGroups)
)

# Display the Number of unique movies and users etc in the
# edx dataset from the table above 
knitr::kable(Table1,
             col.names = c('Ratings','Movies','Users','Count of Unique Genre Combonations'),
             caption = "Number of Unique Users, Movies, and Genres")
## Most Rated Movies
# List the ten most rated movies and display in a table
edx %>% group_by(movieId, title) %>% 
  summarize(count = n()) %>% arrange(desc(count))%>%head(10)%>%
  knitr::kable(col.names = c('MovieId','Title','Count'))

### Highest rated movies with 50+ ratings
# Group the data by movie title
edx %>% group_by(title) %>% 
  
  # Summarize the data for each title group
  summarize(
    numberOfRatings = n(),  # Count the number of ratings for each title
    averageRating = mean(rating)  # Calculate the average rating for each title
  ) %>%
  
  # Filter movies with more than 50 ratings
  dplyr::filter(numberOfRatings > 50) %>%
  
  # Sort the data by average rating in descending order (highest first)
  arrange(desc(averageRating)) %>%
  
  # Select the top 10 movies with the highest average ratings
  top_n(10) %>%
  
  # Create a Kable table with custom column names
  knitr::kable(col.names = c('Title','Number of Ratings','Average Rating'))

# Ratings distribution
# Number of movies by rating

# Alternating Colours by Half star/Full Star
# Save as Plot1
# unicode character for 1/2 \u00bd

lbls<-c('1','1\u00bd','2','2\u00bd','3','3\u00bd','4','4\u00bd','5')

Plot1 <-edx%>%ggplot(aes(rating)) + geom_histogram(binwidth = 0.5,
                                                   fill = rep(c("#e69f00","#009ee9"),5), color = "red") + ggtitle("Distribution of Movie Ratings") +
  theme(plot.title = element_text(hjust = 0.5))

# Change Y access to show count in Millions (M)
Plot1  + scale_y_continuous(labels = scales::label_number_si())+
  scale_x_continuous(breaks=seq(1,5,0.5),labels=lbls)

### Check for missing values in the data set

sapply(edx, {function(x) any(is.na(x))})%>%ifelse('Missing Values','None')%>%knitr::kable(col.names = '# of Missing Values')
## Rating distribution (number of ratings per user)

# Create a histogram of the count of movies by rating
# Save as Plot2

Plot2<-edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "black", bins = 50) +
  scale_x_log10() +
  xlab("# Ratings") +
  ylab("# Users") +
  ggtitle("Users Distribution - Number of Ratings") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot 2 & 3 are plotted together side by side in the RMD file

# Create a histogram of the count of movies by rating
# Save as Plot2

Plot3<-edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "blue", bins = 50) +
  scale_x_log10() +
  xlab("# Ratings") +
  ylab("# Movies") +
  ggtitle("Movie Distribution - Number of Ratings") +
  theme(plot.title = element_text(hjust = 0.5))

Plot2

Plot3


# Number of ratings by UserID with Average Rating
# rounded to the nearest 1/2 star
u_ratings <- edx %>% group_by(userId) %>% 
  summarize(n= n(), avg.rating = round(mean(rating)*2,digits = 0)/2)

# Plot the userID against the number of ratings


u_ratings %>% ggplot(aes(avg.rating)) + geom_histogram(colour = "#996666", fill = "#0099F8",bins = 10) +
  labs(title = "Average Movie Ratings Nearest 1/2 Star",x='Avgerage Rating',y='Number of Ratings')+theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1,5,0.5),labels=lbls)+
  scale_y_continuous(labels = scales::label_number_si())

## Rating distribution (number of ratings per movie)
mv_ratings <- edx %>% group_by(movieId) %>%
  summarize(nMovie_Rtings= n(), avg.rating = mean(rating))%>%
  arrange(desc(avg.rating))

gc # clear memory
mv_ratings %>% ggplot(aes(x = nMovie_Rtings, y = avg.rating)) + geom_point(colour = "#806699") + geom_smooth(method = "loess", span=0.1,formula = 'y ~ x') +
  labs(title = "Average movie ratings vs. frequency of ratings", y='Average Rating',x='Number of Movie Ratings') +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks=seq(1,5,0.5),labels=lbls)

## Top 10 Most Popular Genres

strsplit(edx$genres,"\\|")%>%unlist(use.names = FALSE)%>%table()%>%
  sort(decreasing =TRUE)%>%head(10)%>%as.matrix()%>%t()%>%knitr::kable()

## Movie Age
library(stringr)
library(lubridate)
# Code to extract the release date and calculate Age of the movie from when it was rated
Yr_patt<-"(\\(\\d{4}\\))$" # Pattern to match release year (in title)

# Code to calculate Age of the movie from when the database was created
DBYr <- edx$timestamp%>%max()%>%as_datetime()%>%year()

# Extract the release date from the movie title field encapsulated in brackets at the end of the field, Calculate the age of the movie and extract the year from the timestamp (Year Rated)
edx_with_dates<-edx%>%
  mutate(releaseYR=as.numeric(substr(str_extract(title, Yr_patt),2,5))
         ,title = sub(Yr_patt,'',title),
         year_rated = year(as_datetime(timestamp)),
         MovieAge=DBYr - releaseYR
  )

edx_with_dates %>%dplyr::select(title,genres,releaseYR,year_rated,MovieAge)%>% head() %>% knitr::kable(caption = 'edx dataset with movie age',col.names = c('Title','Genres','Release Year', 'Year Rated','Movie Age'))

# Average movie rating vs. Frequency of rating
# Get the average by movie age
AvgByAge<-edx_with_dates %>%
  group_by(year_rated,MovieAge)%>%
  summarize(AvgRatingByAge=mean(rating))

# And Plot it against the ratings
AvgByAge%>%ggplot(aes(MovieAge,AvgRatingByAge))+geom_point()+
  ggtitle("Age of a Movie vs Average Movie Rating")+
  xlab("Age in Years")+ylab("Average Rating")+
  scale_y_continuous(breaks=seq(1,5,0.5),labels=lbls)
#Splitting the edx data into 20% training and 80% test
# Set seed (I am running this code on R 3.5.2 so I will need the older code)
# Final Holdout data created in the first code block

if(base::getRversion()>'3.6'){ set.seed(1, sample.kind="Rounding") }else{set.seed(1) }

# Split the edx dataset into Traing amd Test sets 
# Create 1 partition 20% of the edx data
# Using edx_with_dates (Same data with additional fields MovieAge)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx_with_dates[-test_index,]
test_set <- edx_with_dates[test_index,]

# Removing rows that are in both test and train sets

test_set <- test_set %>%
  semi_join(train_set, by = "movieId")
train_set <- train_set %>%
  semi_join(test_set, by = "movieId")

# RMSE function to be used in other code chunks

RMSE <- function(actual_ratings, predicted_ratings){
  sqrt(mean((actual_ratings - predicted_ratings)^2))
}

## Modeling

## Simple Model

# Determine the average rating in the training data
muHat <- mean(train_set$rating)

# Lets build a tibble and add the RMSE for each method
# Initializing the tibble and adding the first result

RMSE_results<- tibble(Method= "Average without Bias",
                      RMSE = RMSE(test_set$rating, muHat))
# Adding in the Movie Bias bi
## Movie effect

# For each movie, we determine how much its
# average rating deviates from the total mean.
# Subtract the mean from the ratings for each movie
# Plot number of movies with the computed b_i
# Calculating b_i
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - muHat))

predicted_ratings <- muHat + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
m_1_rmse <- RMSE(predicted_ratings, test_set$rating)
# adding RMSE for the Movie Effect to the tibble
RMSE_results <- RMSE_results%>%add_row(Method="Movie Effect Model",
                                       RMSE = m_1_rmse )
# Use Movie_avgs in the next chunk
# Graphing Averages for users who have rated at least 50 movies

LimitN <-50
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  dplyr::filter(n()>LimitN)%>%
  summarize(b_u = mean(rating - muHat - b_i))

Plot7<- user_avgs%>% qplot(b_u, geom ="density", data = .,
                           colour = I("#2EA7CE"), fill = I("#8E87CE"),
                           main = "Number of Users vs. Bias")
Plot7
# Calculating User Averages b_u using muHat and b_i from the previous chunk

user_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - muHat - b_i))

# Predicting Ratings using the test data

predicted <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = muHat + b_i + b_u) %>%
  .$pred
m_2_rmse <- RMSE(predicted, test_set$rating)

# adding RMSE for the Movie and User Effect to the tibble
RMSE_results <- RMSE_results%>%add_row(Method="Movie and User Effects Model",
                                       RMSE = m_2_rmse )

# Lets add the Genres effect
muHat <- mean(train_set$rating)
# Calculating Genres Averages b_g using muHat and b_i, b_u from the previous chunk

genre_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - muHat - b_i - b_u))

# Predicting Ratings using the test data
predicted <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(pred = muHat + b_i + b_u + b_g) %>%
  .$pred
m_3_rmse <- RMSE(predicted, test_set$rating)
RMSE_results <- RMSE_results%>%
  add_row(Method="Movie, User, Genres Effects Model", RMSE = m_3_rmse )

# Lets add the release year
# Calculating Release Year Averages b_y using muHat and
# b_i, b_u, b_g from the previous chunk
relyear_avgs <- train_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  group_by(releaseYR) %>%
  summarize(b_y = mean(rating - muHat - b_i - b_u - b_g))

# Predicting Ratings using the test data
predicted <- test_set %>%left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(relyear_avgs, by = "releaseYR") %>%
  mutate(pred = muHat + b_i + b_u + b_g + b_y) %>%
  .$pred
m_4_rmse <- RMSE(predicted, test_set$rating)

RMSE_results <- RMSE_results%>%
  add_row(Method="Movie, User, Genres, Release Year Effects Model",
          RMSE = m_4_rmse )
# Regularize by Movie, User, Genres, Release Year
# lambda is used as a tuning parameter

# Function used to calculate RMSE for a given lambda

get_rmses<-function(l) {
  muHat <- mean(train_set$rating)
  # message(paste("Trying Lambda", as.character(l)))
  # setWinProgressBar(pb, l, sprintf("Lamda (%s)", l), l)
  bi <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - muHat)/(n() + l))
  
  bu <- train_set %>% left_join(bi, by = "movieId") %>%
    group_by(userId) %>% summarize(b_u = sum(rating -
                                               b_i - muHat)/(n() + l))
  
  bg <- train_set %>% left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - muHat)/(n() + l))
  
  by <- train_set %>% left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    left_join(bg, by = "genres") %>%
    group_by(releaseYR) %>%
    summarize(b_y = sum(rating - b_i - b_u - b_g - muHat)/(n() + l))
  
  # Predicting Ratings using the test data
  predicted <- test_set %>%left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    left_join(bg, by = "genres") %>%
    left_join(by, by = "releaseYR") %>%
    mutate(p = muHat + b_i + b_u + b_g + b_y) %>% .$p
  return(RMSE(predicted, test_set$rating))
  
}

# Get approximately the best lambda
# To save on processing steps we start with 5 values

lambdas <- seq(2, 6, 1)

m_avg <- mean(train_set$rating)

# Get predictions from the lambdas using the function get_rmses
p_rmses <- sapply(lambdas, get_rmses)

# Get the lowest value
good<-lambdas[which.min(p_rmses)]

# Save these for the graph
P_temp<-p_rmses
L_temp<-lambdas

# Create a new set of lambda values with smaller increments
# centered around the best value we've identified in the
# previous step

lambdas<-seq(good - 0.8, good +0.8, 0.2)

# Get predictions from the new lambdas using the function get_rmses
p_rmses <- sapply(lambdas, get_rmses)
# Get the lowest value
better<-lambdas[which.min(p_rmses)]

# for the graph
P_temp<-c(P_temp,p_rmses)
L_temp<-c(L_temp,lambdas)

# Plot with title and axis labels
qplot(L_temp,P_temp,xlab='Lambda',ylab='RSME', 
      main = 'Plot of RMSE versus lambda')
# Create a new set of lambda values with even smaller increments
# centered around the best value we've identified in the
# previous step

lambdas<-seq(better - 0.1, better +0.1, 0.05)
p_rmses <- sapply(lambdas, get_rmses)

best<-lambdas[which.min(p_rmses)]

best # Print the best RMSE
# adding RMSE for the Regularize by Movie, User, Genres, Release Year
# to the tibble
RMSE_results <- RMSE_results%>%
  add_row(
    Method="Regularize by Movie, User, Genres, Release Year Effects Model",
    RMSE = min(p_rmses) )

# Printing all the RMSE results
RMSE_results %>% knitr::kable()# The optimal lambda                                                             
best

# testing the optimized lambda on the Final Hold out data
# Fist get the average rating from the final_holdout_test data
m_avg <- mean(final_holdout_test$rating)

# get dataframe with the movieId and Release year
Movie_Release<-edx_with_dates%>%group_by(movieId)%>%distinct(releaseYR)

# Need to add the release year to the holdout data with a left join
# since it wasn't created with the edx supplied code

final_holdout_test1<-final_holdout_test %>%
  left_join(Movie_Release, by = "movieId")

# Function to get the RMSE on the final_holdout_test data
get_holdout<-function(l){
  
  message(paste("Using Lambda", as.character(l),"on the holdout data"))
  
  # b_i
  bi <- final_holdout_test1%>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - muHat)/(n() + l))
  # b_u
  bu <- final_holdout_test1 %>%
    left_join(bi, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - muHat)/(n() + l))
  # b_g
  bg <- final_holdout_test1%>%
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - muHat)/(n() + l))
  # b_y
  by <- final_holdout_test1%>%
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    left_join(bg, by = "genres") %>%
    group_by(releaseYR) %>%
    summarize(b_y = sum(rating - b_i - b_u - b_g - muHat)/(n() + l))
  
  # using Holdout test data for the predictions
  predicted <- final_holdout_test1%>%
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    left_join(bg, by = "genres") %>%
    left_join(by, by = "releaseYR") %>%
    mutate(p = muHat + b_i + b_u + b_g +b_y) %>% .$p
  return(RMSE(predicted, final_holdout_test1$rating))
  
  
  return(RMSE(predicted, final_holdout_test$rating))
}

RMSE_Holdout<-get_holdout(best)
RMSE_results <- RMSE_results%>%add_row(Method="Regularize by Movie, User, Genres, Release Year Effects Model using holdout data",
RMSE = RMSE_Holdout )

RMSE_results %>% knitr::kable()

# End
