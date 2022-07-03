###############################################################################

# Scenario: Continuing from the previous assignment activity, Turtle Games wants
# your help with predicting the sales of the video games for the next financial
# year based on the available sales data from Europe and North America.

# Objective: Determine the optimal global sales (in millions) for the next
# financial year for each of the video games.

###############################################################################

# Import data set
games_sales <- read.csv(file.choose(), header=TRUE)

# Import libraries
library(tidyverse)

# Sense-check the data set
summary(games_sales)
str(games_sales)
# Observations: There are 16598 observations in the data set and 9 columns.
# 'Name', 'Platform', 'Year','Genre', and 'Publisher' are stored as character 
# variables. 'Rank' is stored as an integer. 'NA_Sales', 'EU_Sales', and 
# 'Global_sales' are stored as numeric. 
# 'Rank' ranges from 1-16600. 'NA_Sales' range from 0-41.49, with a mean of 
# 0.26 and median of 0.08. 'EU_Sales' range from 0-29.02, with a mean of 0.15 
# and median of 0.02. 'Global_Sales' range from 0.01-82.74, with a mean of 0.54 
# and a median of 0.17. 

# Apply multi-linear regression to determine the optimal global sales for all 
# the video games based on: sales of video games in North America and Europe

## Create a MLR model for global sales using NA sales and EU sales
model_NAandEU <- lm(Global_Sales ~ NA_Sales + EU_Sales, data=games_sales)
summary(model)
## Observations: NA_Sales and EU_Sales have high significance, which indicate
## that the model is strong. The adjusted R-squared value is high (0.96), which 
## also indicates that the model is strong. 

## Create a MLR model for global sales using NA sales
model_NA <- lm(Global_Sales ~ NA_Sales, data=games_sales)
summary(model)
## Observations: NA_Sales has high significance, which indicates
## that the model is strong. The adjusted R-squared value is high (0.89), but 
## not as high as it was using NA sales and EU sales.

## Create a MLR model for global sales using EU sales
model_EU <- lm(Global_Sales ~ EU_Sales, data=games_sales)
summary(model)
## Observations: EU_Sales has a high significance, which indicates
## that the model is strong. The adjusted R-squared value is high (0.82), but 
## not as high as it was using NA sales and EU sales.

## Observation: The MLR model using both NA and EU sales is the best model since
## it has the highest adjusted R-squared value. 

## Use the predict function to predict values based on previous data behaviours
## and fit data to a model.
predictTest= predict(model_NAandEU, data=games_sales, interval='confidence')

## Store predicted values as new column in games_sales data set
games_sales$predicted_global_sales <- as.numeric(predictTest[,1])

## View updated data set containing predicted values
View(games_sales)
## Export as csv
write.table(x=games_sales,file="predicted_game_sales.csv",
    sep=",",quote=F,row.names=F,col.names=T)

## View plot of predicted values
ggplot(data=games_sales,aes(x=Global_Sales,y=predicted_global_sales)) + 
  geom_point() +
  labs(title="Predicted global sales",
       x= "Current global sales",
       y="Predicted global sales")

# Insights: The MLR model using both NA sales and EU sales to predict global
# sales is the best model because it has the highest adjusted R-squared value. 
# The predicted values seem to have a linear relationship with the current 
# values.