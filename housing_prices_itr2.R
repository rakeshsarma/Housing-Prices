#Iteration 2

# setting workspace
setwd("/kaggle/Real Estate prices")
# remove env variables
rm(list = ls(all.names = T))

# libraries 
library(caret)
library(lubridate)
library(MASS)
library(car)
library(dplyr)

# setting seed
set.seed(123)

house_data <- read.csv("/kaggle/Real Estate prices/kc_house_data.csv", header=T)

head(house_data, 10)
str(house_data)
View(house_data$date)
nrow(house_data)
# data cleaning
#Remove id as they do not have any information
# lets observe date
head(house_data$date, 10)
#split date
house_data$date <- unlist(strsplit(as.character(house_data$date),  "T"))[1]
house_data$date <- ymd(house_data$date)
# convert grade as factor 
house_data$grade <- as.factor(as.character(house_data$grade))

# understanding if floors impacts price : I think it shouls increase and then decrease lets see.
unique(house_data$floors)
floor_price <- house_data %>% group_by(floors) %>% summarise(mean_price = mean(price))
floor_price
plot(floor_price)

# What I am trying to achive is understand how grade affects the prices of houses. 

house_grade_price <- house_data %>% 
  group_by(grade) %>% 
  summarise(mean_price = mean(price))

plot(house_grade_price)
# Price and grade are directly proportional as grade increases price increases.

# price and bedrooms
house_bedroom_price <- house_data %>% 
  group_by(bedrooms) %>% 
  summarise(mean_price = mean(price))

plot(house_bedroom_price)
table(house_data$bedrooms)
# looks like there's a house with 11 and and 33 beds houses 
# removing rows with 0 bedrooms,9,10 11, 33 
house_data[house_data$bedrooms = 0]
length(which(house_data$bedrooms == 0 | house_data$bedrooms>8))
# removing 24 rows.
# rest of thr rows agter removing bedroom outliers
house_data <- house_data[which(house_data$bedrooms != 0 & house_data$bedrooms<9),]
# sanity Check
length( which(house_data$bedrooms != 0 & house_data$bedrooms<9)) + length(which(house_data$bedrooms == 0 | house_data$bedrooms>8)) == nrow(house_data)
# removing id
house_data <- select(house_data, -c(id))
# this is not executing, so lets detach packages and reload them in an order
detach(name =  "package:dplyr", character.only = T)
detach(name =  "package:caret", character.only = T)
detach(name =  "package:car", character.only = T)

# lets also understand the relation between condition and price
house_condition_price <- house_data %>% group_by(condition) %>% summarise(mean_price = mean(price))
plot(house_condition_price)
# looks like the's a zig zag relationship between house condition and price
house_condition_table <- table(house_data$condition)
plot(house_condition_table) # There's a steady increase and decrease of the count of housees wrt the house condition

plot( as.numeric(house_data$grade), house_data$sqft_lot)

#

mod1<- lm(house_grade_price$mean_price~house_grade_price$grade, data = house_grade_price )
summary(mod1)
abline(mod1)
plot(mod1)




