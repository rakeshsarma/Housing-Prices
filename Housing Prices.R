# setting workspace
setwd("/kaggle/Real Estate prices")
# remove env variables
rm(list = ls())

# libraries 
library(dplyr)
library(caret)
library(lubridate)
library(MASS)
library(car)
# setting seed


# Import dataset
house_data <- read.csv("/kaggle/Real Estate prices/kc_house_data.csv")
View (head(house_data, 10))
# understanding data
str(house_data)
# 1. Date is imported as factors; Change date format
# 2. leave bedrooms as numeric
# 3. Waterfront, view  should be categorical
# 4. lat long and zip code will give almost same info.

# Data cleaning
View(cor(house_data[-c(1,2)]))
plot(cor(house_data[-c(1,2)]))
plot((house_data[-c(1,2)]))
View(head(house_data))     
house_data$waterfront <- as.numeric(as.factor(house_data$waterfront))
house_data  <- select(house_data, -starts_with("id"))
head(View(house_data))
house_data$date <- as.Date(house_data$date)
house_data$date <- unlist(strsplit(as.character(house_data$date), split = "T"))[[1]]  # Split by T and take the forst character as datr
house_data$date <- as.Date(house_data$date, format = "%Y%M%d")   # convert to date
View(head(house_data))


# understanding data
# The data is taken on the date 13-Oct-2014
hist(house_data$price, probability = T)
hist((house_data$bedrooms), xlim = c(0,10))
barplot(table(as.numeric(as.character(house_data$bedrooms))))
View(select(house_data, house_data$bedrooms> 5))
View(filter(house_data, house_data$bedrooms>=9))
max(house_data$bedrooms)
table(house_data$bedrooms)
table(house_data$date)
year(house_data$date)
house_data$year_diff_built <- year(house_data$date)-house_data$yr_built # year diffrernce between built yr and 2014
house_data$year_diff_renov <- year(house_data$date)-house_data$yr_renovated # year diffrernce between renovated yr and 2014
house_data$year_diff_renov <- ifelse(house_data$year_diff_renov == 2014, 0, house_data$year_diff_renov) 
str(house_data)
# remove renovated year and year built
house_data_noYears <- select(house_data, -c(date, yr_built, yr_renovated))
head(house_data_noYears)
house_data_noYears$bedBathRatio <- as.numeric(house_data_noYears$bedrooms/house_data_noYears$bathrooms)
cor(house_data_noYears$price, house_data_noYears$bedBathRatio)
View(house_data[house_data$bedrooms == 0,])

str(house_data_noYears)
# Correcation between floors and price
cor(house_data$price, house_data$floors)
plot( house_data$bedrooms, house_data$price)
cor(house_data$year_diff_renov, house_data$price)

# Split into traning and test data
set.seed(123)
train_index <- createDataPartition(house_data$price, p=0.60, list = F)
house_data_train <- house_data[train_index,]
house_data_test1 <- house_data[-train_index,]

test_index <- createDataPartition(house_data_test1$price, p=0.5, list = F)
house_data_validation <- house_data_test1[test_index,]
house_data_test <- house_data_test1[-test_index,]

nrow(house_data) == nrow(house_data_train)+nrow(house_data_test)+nrow(house_data_validation) # just checking

# using step wise linear regression in r
model <- lm(house_data_train$price ~ ., data = house_data_train)
summary(model)
stepAIC(model,direction = "both")
vif(model)
