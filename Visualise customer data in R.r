##############################################################################
# Assignment 3 Week 4: Visualise data to gather insights  

# Scenario: Turtle Games sources and sells Lego products across various
# countries. After predicting the optimal prices and optimal numbers of
# product segments, they want your help with exploring the data to derive 
# information on certain customer groups, which in turn will provide them 
# with information to potentially improve their sales performance.

# Objectives:
# - Determine the customer group that will most likely leave a review on the
#   the products they have purchased: Which age group submits the most
#   reviews?
# - Determine the most expensive product purchased by a particular group of
#   customers: What is the most expensive Lego set purchased by customers who 
#   are at least 25 years old (>25 years)?

###############################################################################

# Prepare workstation
getwd()
setwd("C:/Users/mhbil/OneDrive/Desktop/LSE - Course 3/Final assignment")
getwd()

# Import libraries
library('tidyverse')
# Tidyverse is a collection of packages used to perform a variety of functions
# such as reading in data, exploring data, transforming data, and visualising
# data

# Import data file
lego <- read.csv(file.choose(), header=TRUE, stringsAsFactors = FALSE)

# Explore Data

## Preview data - first few rows
head(lego)
## Observations: The data appears to have been imported correctly. There are 7
## columns for: age, list_price, num_reviews, piece_count, play_star_rating, 
## review_difficulty, and country. The data corresponds with what is expected
## from the metadata.

## Preview data - tibble
as_tibble(lego)
## Observations: There are 12261 observations. All of the data is numeric.

## Preview data - summary
summary(lego)
## Observations:
##   -Age (ages): ranges from 0-30 (mean: 17, median: 19)
##   -Price of the product (list_price): ranges from 2.27 USD-1104.87 USD 
##   (mean: 65.14 USD, median:36.59 USD)
##   -Number of reviews (num_reviews): ranges from 0-367 (mean: 15, median: 4)
##   -Number of Lego pieces in the product (piece_count): ranges from 1-7541 
##   (mean: 493, median: 216)
##   -Star rating by players/customers (play_star_rating): ranges from 0-5 
##   (mean: 4, median: 4)
##   -Difficulty level of the product (review_difficulty): ranges from 0-5 
##   (mean: 2, median: 2)
##   -Number of countries product is sold to (country): ranges from 0-20 
#   (mean: 10, median: 10)

## Check for missing values
colSums(is.na(lego))
## Observations: There are no missing values in the data set

# Which age group submits the most reviews?

## Examine outliers
qplot(x=as.factor(ages), y=num_reviews, data=lego, geom="boxplot", 
      main="Outlier analysis", xlab="Ages", ylab="Number of reviews")

## Remove observations where num_reviews > 300
lego_filtered <- filter(lego, num_reviews < 300)
## Check that num-_reviews > 300 have been removed
summary(lego_filtered)
## Observations: The new data frame has successfully removed num_reviews values
## > 300 since the max is now 228

## Subset by age
## [1] Group by ages
lego_groups <- lego_filtered %>% dplyr::select(ages,num_reviews) %>% 
  arrange(ages) %>% group_by(ages)
head(lego_groups)
## [2] Create an age vector
age.vec <- (lego %>% dplyr::select(ages) %>% arrange(ages))$ages %>% unique(.)
## [3] Assign the age vector to a new data frame (lego_age.df)
lego_age.df <- data.frame(ages=age.vec)
## [4] Map num_reviews by age
lego_age.df$num_reviews <- purrr::map(lego_age.df$ages,function(i){
  (lego %>% filter(.,ages==i))$num_reviews %>% sum(.)
  }) %>% as.integer(.)
## [5] Map list_price by age
lego_age.df$list_price <- purrr::map(lego_age.df$ages,function(i){
  (lego %>% filter(.,ages==i))$list_price %>% sum(.)
}) %>% as.integer(.)
## [6] Check new data frame
lego_age.df

## Plot number of reviews by age
review_age <- ggplot(data=lego_age.df, aes(x=ages, y=num_reviews))+
  geom_col()+
  geom_text(aes(label=num_reviews), color="red", vjust = -0.5)+
  labs(title="Number of reviews per age",
       x="Ages",
       y="Number of reviews")
review_age
## Observations: The most reviews are left for 8 year-olds, with a total of
## 53379 reviews

# What is the most expensive Lego set purchased by customers who are at least 25
# years old?

## Filter data to customers 25 and older
data_age25.df <- filter(lego_filtered, ages >= 25)

## Check that data has been filtered properly
summary(data_age25.df)
## Observations: Data has been filtered properly since the min is 25

## Arrange new data frame by list_price
data_age25.df <- arrange(data_age25.df,list_price)
## Check arrangement
head(data_age25.df)
tail(data_age25.df)

## Plot list prices for subset of those who are >= 25
price <- ggplot(data=data_age25.df, aes(x=list_price))+
  geom_histogram(bins=50)+
  geom_vline(xintercept= 259.87, color="red", linetype="longdash")+
  scale_x_continuous(breaks=seq(0, 260, by=25))+
  labs(title="List price",
       subtitle="Customers age 25 and older",
       x="List Price")
price

## Observation: The most expensive product bought by customers age 25 and older
## cost 259.87 USD

# Summary of insights and possible next steps:
# - The most reviews are left for 8 year-olds, with a total of 53379 reviews.
# - The most expensive product bought by customers age 25 and older cost 259.87 
#   USD.
# - If Turtle Games has another data set containing product names that can be
#   merged with the lego data set, this would be beneficial in identifying what
#   products customers are leaving the most reviews for. In addition, it would 
#   help to merge this analysis with a sentiment analysis to determine whether
#   the reviews are positive or negative. It would also be beneficial to know
#   what the name and/or type of product is that is the most expensive product
#   bought by those aged 25 and older. These insights could help inform
#   Turtle Games' product line and marketing campaigns. 

