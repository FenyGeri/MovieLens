###################################################################################
# Create edx set, validation set + keep whole movielens set for data exploration
###################################################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

#we remove unnecessary data
rm(dl, ratings, movies, test_index, temp, removed)

##########################
#exploring the dataset
##########################
library(lubridate)

#we create some additonal dataframe for data exploration
#distinct list of movieid vs title
titles <- movielens %>% select(movieId,title) %>% distinct()

#distinct list of users
users <- movielens %>% select(userId) %>% distinct()

################################
#movielens : the whole dataset
################################
#explore the structure
str(movielens)
#basic statistics
summary(movielens)
#number of movies rated
movielens %>% select(movieId) %>% distinct() %>% summarise(n=n())
#or the same can be obtained by
n_distinct(movielens$movieId)
#number of users who rated movies
movielens %>% select(userId) %>% distinct() %>% summarise(n=n())
#or again:
n_distinct(movielens$userId)
#possible ratings are between 0.5 and 5
movielens %>% select(rating) %>% distinct() %>% arrange(rating)
#in title we have the production year of the movie
movielens %>% select(title) %>% head()
#we can extract it and add a column prodyear
movielens <- movielens %>% mutate(prodyear=str_sub(title, str_length(title)-4,str_length(title)-1))
#films were produced between the following years
range(movielens$prodyear)
#the most rated films are not the most recent ones, but from the mid-90s (recent enough to be popular + enough time to rate)
movielens %>% group_by(prodyear) %>% summarise(n=n()) %>% arrange(desc(n)) %>% slice(1:10) %>% knitr::kable()
#or visually
movielens %>% ggplot(aes(as.numeric(prodyear))) + geom_bar() + scale_x_continuous("Production year") + 
  scale_y_continuous("Number of ratings") + ggtitle("Number of ratings vs production year of films") +
  theme(plot.title = element_text(hjust = 0.5))
#we can zoom on the 1990's
movielens %>% filter(prodyear>=1990 & prodyear<=2000 ) %>% ggplot(aes(as.numeric(prodyear))) + geom_bar() + 
  scale_x_continuous("Production year") + 
  scale_y_continuous("Number of ratings") + 
  ggtitle("Zoom on the 90's") +
  theme(plot.title = element_text(hjust = 0.5))
#we can transform the rate timestamp to date
movielens <- movielens %>% mutate(rate_date=as_datetime(timestamp))
#ratings were given between the following dates
range(movielens$rate_date)
#the number of ratings per year is not constant
movielens %>% mutate(rate_year=year(as.Date(rate_date))) %>% 
  ggplot(aes(rate_year)) + geom_histogram() + scale_x_continuous("Rating year") + 
  scale_y_continuous("Number of ratings") + ggtitle("Number of ratings per year") +
  theme(plot.title = element_text(hjust = 0.5))
#1995 and 2009 are two outliers and should be excluded
movielens %>% mutate(rate_year=year(as.Date(rate_date))) %>% group_by(rate_year) %>% 
  summarise(n=n()) %>% arrange(desc(n))
#but we still see, that in 2000 we had 6 times more ratings than in 1998
movielens %>% mutate(rate_year=year(as.Date(rate_date))) %>% group_by(rate_year) %>% 
  summarise(n=n()) %>% filter(rate_year>1995 & rate_year<2009) %>%
  summarise(rapport=max(n)/(min(n))) 

#however an upward trend is observable
movielens %>% mutate(rate_year=year(as.Date(rate_date))) %>% group_by(rate_year) %>% 
  summarise(n=n()) %>% filter(rate_year>1995 & rate_year<2009) %>%
  ggplot(aes(rate_year,n)) + geom_point() + geom_smooth(method = "lm", alpha=0.5, se=FALSE) + 
  scale_x_continuous("Year of rating") + 
  scale_y_continuous("Number of ratings received") + ggtitle("Number of ratings received per year (trend)") +
  theme(plot.title = element_text(hjust = 0.5))

#function to get the mode of a distribution
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#average, median and mode of movies' rating
movielens %>% summarise(average=mean(rating),median=median(rating), mode=getmode(rating))
#the median is higher than the average which means a negative skew that we prove later
#stdev
movielens %>% summarise(stdev=sd(rating))
#empirical ditribution is multimodal as half ratings are rare:
movielens %>% ggplot(aes(rating)) + geom_histogram(bins = 10) + 
  scale_x_continuous("Rating") + 
  scale_y_continuous("Number of ratings received") + ggtitle("Distribution of ratings") +
  theme(plot.title = element_text(hjust = 0.5))
#People tend to rate more good films
#half ratings are rare -> it is better to round ratings -> we can see that the distribution is skewed and unimodal, with a mode of 4
movielens %>% mutate(rating=round(rating)) %>% ggplot(aes(rating)) + geom_histogram(bins = 5) + 
  scale_x_continuous("Rating") + 
  scale_y_continuous("Number of ratings received") + ggtitle("Distribution of ratings (round to integer)") +
  theme(plot.title = element_text(hjust = 0.5))
#3rd and 4th momentum (skewness + kurtosis) : negative skew + almost normal kurtosis (normal distrib: skew=0, kurtosis = 3)
#distribution is not normal, we cannot use the central limit theorem and confidence intervals
#meaning of the negative skew: people tend to rate slightly better than the average, but there is a longer negative tail than normal
library(moments)
movielens %>% summarise(skewness=skewness(rating), kurtosis=kurtosis(rating))
###################
#ratings per film
###################
#number of ratings received per film
movielens %>% group_by(movieId) %>% summarise(n=n()) %>% ggplot(aes(n)) + geom_histogram(bins=50) + 
  scale_x_continuous("Number of ratings received") + 
  scale_y_continuous("Number of films") + ggtitle("Distribution of number of ratings per film") +
  theme(plot.title = element_text(hjust = 0.5))
#on a log scale we can better visualize the distribution
movielens %>% group_by(movieId) %>% summarise(n=n()) %>% 
  ggplot(aes(n)) + geom_histogram(bins=200) + 
  scale_y_continuous("Number of films") + ggtitle("Distribution of number of ratings per film") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_log10("Number of ratings received")
#a couple of blockbusters received a really large number of ratings
movielens %>% group_by(movieId) %>% summarise(n=n()) %>% arrange(desc(n)) %>% slice(1:10) %>% left_join(titles, by='movieId') %>%
  select(title,n)
#while a lot of films received only a couple of ratings
movielens %>% group_by(movieId) %>% summarise(number_of_ratings=n()) %>% group_by(number_of_ratings) %>% 
  summarise(number_of_films=n()) %>% arrange(number_of_ratings) %>% slice(1:10)
#most films received a number of ratings of only one digits:
movielens %>% group_by(movieId) %>% summarise(number_of_ratings=n()) %>% 
  group_by(number_of_ratings) %>% summarise(number_of_films=n()) %>% arrange(desc(number_of_films)) %>% slice(1:10)
#some films received better rating on average then others, but the number of ratings is important as well
movielens %>% group_by(movieId) %>% summarise(mean_rating=mean(rating),  number_rating=n()) %>% arrange(desc(mean_rating)) %>%
  slice(1:10) %>% left_join(titles,by='movieId') %>% select(title, mean_rating, number_rating)
#these films are art movies rated by just a couple of users, the rating is strongly biaised, we should select a minimum threshold
#We can doubt that the Hungarian Satantango is the best rated film
#Let's try with n=20, the best films are:
movielens %>% group_by(movieId) %>% summarise(mean_rating=mean(rating),  number_rating=n()) %>% 
  filter(number_rating>=20) %>% arrange(desc(mean_rating)) %>%
  slice(1:10) %>% left_join(titles,by='movieId') %>% select(title, mean_rating, number_rating)
#with the same threshold of n=20 the worst rated films are the following:
movielens %>% group_by(movieId) %>% summarise(mean_rating=mean(rating),  number_rating=n()) %>% 
  filter(number_rating>=20) %>% arrange(mean_rating) %>%
  slice(1:10) %>% left_join(titles,by='movieId') %>% select(title, mean_rating, number_rating)
#This operation well isolated outliers
#We can observe the same effect for worst film ratings, the worst averages are given to not well known films by just one or two users
movielens %>% group_by(movieId) %>% summarise(mean_rating=mean(rating),  number_rating=n()) %>% arrange(mean_rating) %>%
  slice(1:10) %>% left_join(titles,by='movieId') %>% select(title, mean_rating, number_rating)

############################
#ratings per user
############################
#some users rated a large number of films
movielens %>% group_by(userId) %>% summarise(n=n()) %>% arrange(desc(n))
#while others gave only a couple of ratings, most users giving about 20 ratings
movielens %>% group_by(userId) %>% summarise(number_of_ratings=n()) %>% group_by(number_of_ratings) %>% 
  summarise(number_of_users=n()) %>% arrange(desc(number_of_users)) %>% 
  slice(1:10)
#visually:
movielens %>% group_by(userId) %>% summarise(number_of_ratings=n()) %>% group_by(number_of_ratings) %>% 
  summarise(number_of_users=n()) %>% ggplot(aes(number_of_ratings, number_of_users)) + geom_point() +
  scale_x_continuous("Number of ratings") + 
  scale_y_continuous("Number of users") + ggtitle("Number of rating per user") +
  theme(plot.title = element_text(hjust = 0.5))
#some users tend to give better ratings then others, while some users are kinky
rating_by_user <- movielens %>% group_by(userId) %>% summarise(avg_rating=mean(rating)) 
rating_by_user %>% ggplot(aes(avg_rating)) + geom_histogram(colour = "black", 
                                                            fill = "white") 
#this distribution seems quite normal
rating_by_user %>% ggplot(aes(x=avg_rating)) + geom_histogram(aes(y=..density..),color="black", fill="lightblue")+
  stat_function(fun = dnorm, args = list(mean = mean(rating_by_user$avg_rating), sd = sd(rating_by_user$avg_rating)),color="red")
#people tend to give on average more average or slightly better than average ratings, kinky users are also more common, 
#giving average ratings about 2 - 2.5
#this can be verified by negative skew and curtosis > 3 (leptokurtic distribution)
rating_by_user %>% summarise(skewness=skewness(avg_rating), kurtosis=kurtosis(avg_rating))

#we can examine whether users tend to become kinkier or softer with their ratings over time
#irst we create the first rating date of each user
first_ratings <- movielens %>% group_by(userId) %>% summarise(first_rating=min(rate_date)) 

#then we create a rate_date_rel column (transform seconds to sqrt of days)
movielens <- movielens %>% left_join(first_ratings, by = "userId") %>% mutate(rate_date_rel=(as.numeric(rate_date-first_rating))/(60*60*24)) %>% 
  mutate(rate_date_rel=sqrt(rate_date_rel))

#let's examine the two most active users:
#userId=59269
df <- movielens %>% filter(userId=="59269")
fit <- loess(rating ~ rate_date_rel, degree=1, span = 0.5, data=df)
df <- df %>% mutate(smooth = fit$fitted) 

df2 <- movielens %>% filter(userId=="67385")
fit2 <- loess(rating ~ rate_date_rel, degree=1, span = 0.5, data=df2)
df2 <- df2 %>% mutate(smooth = fit2$fitted) 

rbind(df,df2) %>% ggplot(aes(rate_date_rel,smooth)) + geom_line(color="blue") +
  facet_grid(. ~ userId) + 
  scale_x_continuous("Time to first rating") + 
  scale_y_continuous("Average rating given") + ggtitle("Average rating in time for the two most active users") +
  theme(plot.title = element_text(hjust = 0.5))
#We can doubt that user effect in ratings is time dependent. Users tend to change their given average ratings, so the user bias can be 
#modeled as a function of time: u(t)

################################################
#some film genres are more popular then others
################################################
#we create a list of distinct film genres
l_genres <- movielens %>% select(genres) %>% distinct() %>% as.list() 
l_genres <- sapply(transpose(l_genres),function(x){
  transpose(str_split(x,"\\|"))
})
l_genres <- unlist(l_genres)
l_genres <- as.data.frame(l_genres) %>% distinct()
l_genres <- data.frame(lapply(l_genres,as.character),stringsAsFactors = FALSE)

#then we can get the average rating of each genre + number of ratings
genres_rat <- sapply(unlist(l_genres),function(x){
  movielens %>% filter(str_detect(genres,x)) %>% summarise(avg=mean(rating))
})
genres_nr <- sapply(unlist(l_genres),function(x){
  movielens %>% filter(str_detect(genres,x)) %>% summarise(n=n())
})
df <- data.frame(cbind(l_genres,rating=unlist(genres_rat),n=unlist(genres_nr))) %>%
  mutate(genres=l_genres) %>% select(genres,rating,n)

#it is to treat with caution as one film can have more than one genre
#We can see, genres with lower and higher number of ratings tend to have a better average rating
df <- df %>% filter(genres!="(no genres listed)") 
fit <- loess(rating ~ n, degree=1, span = 0.8, data=df)
df %>% filter(genres!="(no genres listed)") %>% mutate(smooth = fit$fitted) %>% ggplot(aes(n,rating,color=genres)) + geom_point() +
  geom_line(aes(n, smooth), color="red") +
  scale_x_continuous("number of ratings") + scale_y_continuous("average rating")  + 
  ggtitle("Average rating and number of ratings per genre" ) +
  theme(plot.title = element_text(hjust = 0.5))
  
#we can also visualize the rating of each genre on a boxplot to see which genre has the most consistent rating 
#and averages can also be compoared
#create a dataframe df2 with the associated genres for each movie (tidy format)
#empty df2
df2 <- movielens %>% filter(genres=="toto") %>% mutate(gnr="toto")
#append each genre to df
for(i in 1:length(unlist(l_genres))){
  df1 <- movielens %>% filter(str_detect(genres,unlist(l_genres)[i])) %>% mutate(gnr=unlist(l_genres)[i])
  df2 <- rbind(df1,df2)
}

#create boxplot with mean showed
df2 %>%
  mutate(genre = reorder(gnr, rating, FUN = mean)) %>% ggplot(aes(genre,rating)) + geom_boxplot(fill="lightblue") + 
  stat_summary(fun.y = mean, geom = "point", shape=20, size=8,color="red", fill="red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_hline(yintercept = mean(movielens$rating), color="blue", size=2) + 
  ggtitle("Average rating per genre" ) +
  theme(plot.title = element_text(hjust = 0.5))
#a possible genre effect can be used but with caution as films have more than one genres
#The boxplot shows for example that horror is rated on average lower than the average 3.5 of all films, 
#but ratings are not consistent, some films are rated 0.5 while others were rated to 5
#Science-Fiction received more consistent ratings, the box is smaller 
#the same can be obtained using the separate_rows command:
split_movielens <- movielens   %>% separate_rows(genres, sep = "\\|")

#########################
#training + test set
#########################
#edx : training set (less observations, but same number of movies and users)
str(edx)
#number of movies rated
edx %>% select(movieId) %>% distinct() %>% summarise(n=n())
#by number of users
edx %>% select(userId) %>% distinct() %>% summarise(n=n())


#validation: validation set with less number of users and films
str(validation)
#number of movies rated
validation %>% select(movieId) %>% distinct() %>% summarise(n=n())
#by number of users
validation %>% select(userId) %>% distinct() %>% summarise(n=n())

#for final validation we sill use the RMSE (Residual Means Squares Errors)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#We will use a model with regularized movie + user effect (lambda is a degressive regularization factor)
#We can use 10_fold cross validation without the validation set to determine the optimal lambda that minimizes RMSE
#We set the seed to receive a stable result
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 10, p = 0.1, list = FALSE)
results <- sapply(seq(1,10),function(i){
  
  train_set <- edx[-test_index[,i],]
  temp <- edx[test_index[,i],]
  
  # Make sure userId and movieId in validation set are also in edx set
  test_set <- temp %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(temp, test_set)
  train_set <- rbind(train_set, removed)
  
  lambdas <- seq(0, 10, 0.25)
  
  rmses <- sapply(lambdas, function(l){
    
    mu <- mean(train_set$rating)
    
    b_i <- train_set %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    
    b_u <- train_set %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
    predicted_ratings <- 
      test_set %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    
    return(RMSE(predicted_ratings, test_set$rating))
  })
  
  return(c(lambdas[which.min(rmses)],min(rmses)))
})

#results: the lambda to use will be the average lambda of the 10 sets 
#and the average RMSE needs to be compared with the RMSE of the validation set to see whether we overtrained
results
mean(results[1,]) #this is the lambda to use: 4.775
mean(results[2,]) #this is the average RMSE on the training set: 0.8649748

#let's check results on the validation set with lambda=4.775
lambda <- 4.775

#global average
mu <- mean(edx$rating)

#penalized movie effect  
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

#penalized user effect  
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

#predicted ratings  
predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

#evaluation
RMSE(predicted_ratings, validation$rating)

#the RMSE of 0.8648198 obtained on the validation set is globally the same as the one obtained during training
#there is no overtraining, we can accept results