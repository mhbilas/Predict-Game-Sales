###############################################################################
# Scenario: The Turtle Games sales team has provided you with the data on the
# number of video games units (in millions) for the next financial year for
# each of the video games. However, before making predictions, you need to help
# them prepare the data for analysis.

# Objective: Prepare the data and ensure that it is ready for analysis by:
# - ensuring there are no missing values
# - modifying the data using string manipulation
# - visualising the data to identify and understand possible trends in the data
#   set

###############################################################################

# Import data file
games <- read.csv(file.choose(), header=T)

# Import libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(moments)
# Tidyverse and dplyr needed for data wrangling. Ggplot2 needed for plotting.
# Moments needed for evaluating measures of shape (skewness and kurtosis).

# Sense check the data set
View(games)
summary(games)
str(games)
# Observations: The data appears to have loaded correctly and matches the csv
# file provide (as seen through the 'View' function). There are 16598 
# observations in the data set and 9 columns.'Name', 'Platform', 'Year','Genre',
# and 'Publisher' are stored as character variables. 'Rank' is stored as an 
# integer. 'NA_Sales', 'EU_Sales', and 'Global_sales' are stored as numeric. 
# 'Rank' ranges from 1-16600. 'NA_Sales' range from 0-41.49, with a mean of 
# 0.26 and median of 0.08. 'EU_Sales' range from 0-29.02, with a mean of 0.15 and
# median of 0.02. 'Global_Sales# range from 0.01-82.74, with a mean of 0.54 and
# a median of 0.17. 

# Check for missing values
sum(is.na(games))
# Observation: No missing values present

# Convert all values under Genre to lowercase for consistency
games$Genre = tolower(games$Genre)
# check that values have been converted
head(games)

# Merge values for Genre and Platform
games$Genre_Platform <- paste(games$Platform, games$Genre, sep=" ")
# Check merge
head(games)

# Evaluate skewness of data
skewness(games$Global_Sales)
# Observation: Skew is 17.40. This suggests that the data is highly 
# positively-skewed

# Plot to view whether data is skewed
ggplot(games, aes(x=Global_Sales)) +
  geom_histogram(bins=50)+
  labs(title="Distribution of global sales",
       x="Global sales (in millions of units)")
# Observation: The data is heavily positively skewed

# Evaluate kurtosis of data
kurtosis(games$Global_Sales)
# Observation: Kurtosis is 606.75. This indicates that the data is 
# leptokurtic (heavy-tailed) distribution, with more extreme outliers than a 
# normal distribution. 

# Check for normality using a qqplot
qqnorm((games$Global_Sales))
# Observation: The data is not normally distributed

# Check skewness of NA_Sales
skewness(games$NA_Sales)
# Observation: The skewness is also high for NA_Sales (18.8), with very high 
# positive skewness

# Check normality of NA_Sales using a qqplot
qqnorm((games$NA_Sales))
# Observation: The data is not normally distributed

# Check skewness of EU_Sales
skewness(games$EU_Sales)
# Observation: The skewness is also high for EU_Sales (18.9), with very high 
# positive skewness

# Check normality of EU_Sales using a qqplot
qqnorm((games$EU_Sales))
# Observation: The data is not normally distributed

# Observation: The global, North American, and EU sales data is not normally 
# distributed. Therefore, we have to be careful about the conclusions we draw 
# from this data. 

# Examine correlation between global sales and North America sales
ggplot(games, aes(x=NA_Sales,
                  y=Global_Sales)) +
  geom_point()+
  labs(title="Relationship between global sales and North America sales",
       x="North America sales", 
       y="Global sales")
# Observation: The data seems to show a linear relationship but is heavily 
# concentrated with lower values and some very high positive outliers.

# Examine correlation between global sales and EU sales
ggplot(games, aes(x=EU_Sales,
                  y=Global_Sales)) +
  geom_point()+
  labs(title="Relationship between global sales and European sales",
       x="EU sales", 
       y="Global sales")
# Observation: The data seems to show a linear relationship but is heavily 
# concentrated with lower values and some very high positive outliers.

# Insights: The global, North America, and EU data is heavily positively skewed.
# While there seems to be a linear relationship between global sales and NA sales
# and EU sales, the analysis would be improved by normalising the data. 
