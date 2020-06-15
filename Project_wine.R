#**********************************************************
# Author: D. Kassa
# Date: June 12, 2020 
#**********************************************************
#  Version  1.00 
# This script will utilize different machine learning models, and determine if the effect 
# of country where the wine was produced, and the sell price of wine affects rating 
# prediction. 
#**********************************************************

#*********************************************************************************
#Install and Load packages 
#*********************************************************************************

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggfortify)) install.packages("ggfortify", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(dplyr)
library(caret)
library("data.table")
library(dslabs)
library(stringr)
library(knitr)
library(tinytex)
library(ggthemes)
library(ggrepel)
library(gridExtra) 
library(ggfortify)
library(ggplot2)	
library(lubridate)
library(tidyr)
library(e1071)
library(recosystem)


#*********************************************************************************
# Load downloaded data into wine_kaggle dataframe, and examine dataset. 
#*********************************************************************************

wine_kaggle <- read_csv(unzip("wine_review_data.csv.zip"))

names(wine_kaggle)

str(wine_kaggle)


#*********************************************************************************
# Create dataframe wine_data by selecting columns wineId, country, points, 
# and price columns; and determine row count of the dataframe.
#*********************************************************************************

wine_data <- subset(wine_kaggle, select = c(wineId, country, points, price))
nrow(wine_data)

#*********************************************************************************
# Exclude rows that do not contain values, and thar are duplicate from the dataset
#*********************************************************************************
wine_review<- wine_data%>%drop_na()%>%distinct()

#*********************************************************************************
# Determine observations and variables of the cleaned dataframe (wine_review)
#*********************************************************************************
dim(wine_review)

#*********************************************************************************
# Determine number of row counts per points
#*********************************************************************************
wine_review%>% 
group_by(points) %>% 
summarize(n = n())%>%
print(n=21)

#*********************************************************************************
# Plot a bar chart of the number of bottles vs review points
#*********************************************************************************

p<- ggplot(wine_review, aes(factor(points)))
p + geom_bar(aes(fill = factor(points))) +
theme(legend.position = "none") +
xlab("Poinits") + 
ylab("Number of Bottle") +
ggtitle("Number of Wines per Points")  

 

#*********************************************************************************
# Determine the price of the top ten common bottle counts
#*********************************************************************************
wine_review%>%
group_by(price) %>% 
summarize(n = n())%>%
arrange(desc(n), price)%>%
print(n=10)



#*********************************************************************************
# Plot a boxplot of points vs price distribution of wines
#*********************************************************************************

wine_review%>%
ggplot(aes(points, price, group = 1)) + 
geom_boxplot()+
geom_jitter(width = 0.1, alpha = 0.2) +
scale_y_continuous(trans = "log2") +
xlab("Review Points") + 
ylab("Price per Bottle") +
ggtitle("Review Point vs Price")  



# Define rating value of each rows that corresponds to the point value. 

c5<- wine_review%>%filter(points >=95 & points <=100)%>%
mutate(rating = 5)
c4<- wine_review%>%filter(points >=90 & points <=94) %>%
mutate(rating = 4)
c3<- wine_review%>%filter(points >=85 & points <=89) %>%
mutate(rating = 3)
c2<- wine_review%>%filter(points >=80 & points <=84) %>%
mutate(rating = 2)
c1<- wine_review%>%filter(points >=75 & points <=79) %>%
mutate(rating = 1)
c0<- wine_review%>%filter(points >=50 & points <=74) %>%
mutate(rating = 0) 



#*********************************************************************************
# Combine all rated rows into wine_review dataframe.
#*********************************************************************************

t1<-rbind(c5,c4)
t2<-rbind(t1,c3)
t3<-rbind(t2,c2)

wine_review<-t3




#*********************************************************************************
# Define country column numerically - 
# Create column countryId, and assign corresponding numerical value of 
# each country.
#*********************************************************************************

a1001<- wine_review%>%filter(country == 'Argentina')%>%mutate(countryId = 1001)
a1002<- wine_review%>%filter(country == 'Armenia')%>%mutate(countryId = 1002)
a1003<- wine_review%>%filter(country == 'Australia')%>%mutate(countryId = 1003)
a1004<- wine_review%>%filter(country == 'Austria')%>%mutate(countryId = 1004)
a1005<- wine_review%>%filter(country == 'Bosnia and Herzegovina')%>%mutate(countryId = 1005)
a1006<- wine_review%>%filter(country == 'Brazil')%>%mutate(countryId = 1006)
a1007<- wine_review%>%filter(country == 'Bulgaria')%>%mutate(countryId = 1007)
a1008<- wine_review%>%filter(country == 'Canada')%>%mutate(countryId = 1008)
a1009<- wine_review%>%filter(country == 'Chile')%>%mutate(countryId = 1009)
a1010<- wine_review%>%filter(country == 'China')%>%mutate(countryId = 1010)
a1011<- wine_review%>%filter(country == 'Croatia')%>%mutate(countryId = 1011)
a1012<- wine_review%>%filter(country == 'Cyprus')%>%mutate(countryId = 1012)
a1013<- wine_review%>%filter(country == 'Czech Republic')%>%mutate(countryId = 1013)
a1014<- wine_review%>%filter(country == 'England')%>%mutate(countryId = 1014)
a1015<- wine_review%>%filter(country == 'France')%>%mutate(countryId = 1015)
a1016<- wine_review%>%filter(country == 'Georgia')%>%mutate(countryId = 1016)
a1017<- wine_review%>%filter(country == 'Germany')%>%mutate(countryId = 1017)
a1018<- wine_review%>%filter(country == 'Greece')%>%mutate(countryId = 1018)
a1019<- wine_review%>%filter(country == 'Hungary')%>%mutate(countryId = 1019)
a1020<- wine_review%>%filter(country == 'India')%>%mutate(countryId = 1020)
a1021<- wine_review%>%filter(country == 'Israel')%>%mutate(countryId = 1021)
a1022<- wine_review%>%filter(country == 'Italy')%>%mutate(countryId = 1022)
a1023<- wine_review%>%filter(country == 'Lebanon')%>%mutate(countryId = 1023)
a1024<- wine_review%>%filter(country == 'Luxembourg')%>%mutate(countryId = 1024)
a1025<- wine_review%>%filter(country == 'Macedonia')%>%mutate(countryId = 1025)
a1026<- wine_review%>%filter(country == 'Mexico')%>%mutate(countryId = 1026)
a1027<- wine_review%>%filter(country == 'Moldova')%>%mutate(countryId = 1027)
a1028<- wine_review%>%filter(country == 'Morocco')%>%mutate(countryId = 1028)
a1029<- wine_review%>%filter(country == 'New Zealand')%>%mutate(countryId = 1029)
a1030<- wine_review%>%filter(country == 'Peru')%>%mutate(countryId = 1030)
a1031<- wine_review%>%filter(country == 'Portugal')%>%mutate(countryId = 1031)
a1032<- wine_review%>%filter(country == 'Romania')%>%mutate(countryId = 1032)
a1033<- wine_review%>%filter(country == 'Serbia')%>%mutate(countryId = 1033)
a1034<- wine_review%>%filter(country == 'Slovakia')%>%mutate(countryId = 1034)
a1035<- wine_review%>%filter(country == 'Slovenia')%>%mutate(countryId = 1035)
a1036<- wine_review%>%filter(country == 'South Africa')%>%mutate(countryId = 1036)
a1037<- wine_review%>%filter(country == 'Spain')%>%mutate(countryId = 1037)
a1038<- wine_review%>%filter(country == 'Switzerland')%>%mutate(countryId = 1038)
a1039<- wine_review%>%filter(country == 'Turkey')%>%mutate(countryId = 1039)
a1040<- wine_review%>%filter(country == 'Ukraine')%>%mutate(countryId = 1040)
a1041<- wine_review%>%filter(country == 'Uruguay')%>%mutate(countryId = 1041)
a1042<- wine_review%>%filter(country == 'US')%>%mutate(countryId = 1042)



#*********************************************************************************
# Combine all temporary tables created to assign country id into a single dataframe, 
# and update wine_review dataframe.
#*********************************************************************************
country_id <- rbind	(a1001,a1002,a1003,a1004,a1005,a1006,a1007,a1008,a1009,a1010,
                      a1011,a1012,a1013,a1014,a1015,a1016,a1017,a1018,a1019,a1020,
                      a1021,a1022,a1023,a1024,a1025,a1026,a1027,a1028,a1029,a1030,
                      a1031,a1032,a1033,a1034,a1035,a1036,a1037,a1038,a1039,a1040,
                      a1041,a1042)

wine_review<-country_id



#*********************************************************************************
# Examine/confirm updated dataframe observations and variables; and list the
# columns name.
#*********************************************************************************
dim(wine_review)
names(wine_review)



#*********************************************************************************
#Determine the count of different wines, maximum price, minimum price, and 
# average price in each rating group
#*********************************************************************************

wine_review %>% 
group_by("Rating" = rating) %>% 
summarize("Count" = n(),
                 "Max. Price" = max(price), 
                 "Min, Price" = min(price), 
                 "Avg. Price" = round(mean(price)))%>%
knitr::kable()



#*********************************************************************************
# Plot – Distribution of Points vs Price
#*********************************************************************************

ggplot(wine_review, aes(x = points, y = price)) + 
geom_point(aes(colour = factor(rating)), size = 2) +
theme(legend.position = "none") +
xlab("Points") + 
ylab("Price") +
ggtitle("Distribution of Price vs Points")



#*********************************************************************************
# Plot – a line graph related to distribution of Price vs Points line graph
#*********************************************************************************
wine_review%>%
filter(rating%in%c("5", "4", "3", "2")) %>%
ggplot(aes(price, points, col = rating)) +
geom_line() +
scale_y_continuous(name = "Points", labels = scales::comma) +
xlab("Price") +
ggtitle("Price per Points")



#*********************************************************************************
# Since linear regression is sensitive to outliers; we will identify price anomaly 
# for group 3 and 4 and remove the extreme data.
#*********************************************************************************
wine_review%>%
filter(rating == 3) %>%
arrange(desc(price)) %>%
print(n=1)



wine_review%>%
filter(rating == 4) %>%
arrange(desc(price)) %>%
print(n=1)



#*********************************************************************************
# Drop anomaly data from rating 3 and 4
#*********************************************************************************
wine_review<-subset(wine_review, wineId!=80290 & wineId!=120391) 



#*********************************************************************************
# Plot graph to observe irregular data are removed
#*********************************************************************************
p <- ggplot(wine_review, aes(rating, price))
p + geom_point(aes(colour = factor(rating)), size = 2)+
theme(legend.position = "none") +
xlab("Rating") + 
ylab("Price") +
ggtitle("Distribution of Price per Rating") 



#*********************************************************************************
# Set seed to 1. 
# Extract 10% of dataset for validation purpose, and assign the remaining 90% 
# to wine_df dataframe.
#*********************************************************************************

set.seed(1, sample.kind = "Rounding")

y = wine_review$rating
test_index<- createDataPartition(y, times = 1, p = 0.1, list = FALSE)
wine_df<- wine_review[-test_index,]
temp<- wine_review[test_index,]



#*********************************************************************************
# Create dataframe validation, and make sure country and price 
# are also in wine_df dataset
#*********************************************************************************
validation<- temp %>% 
  semi_join(wine_df, by = "country") %>%
  semi_join(wine_df, by = "price")



# Determine row count of validation dataset 
nrow(validation)



#*********************************************************************************
# Keep data from temp table for which there are no data in 
# validation dataset; and determine row count of removed rows 
#*********************************************************************************
removed <- anti_join(temp, validation)
nrow(removed)

#*********************************************************************************
# add rows removed from validation table back into wine_df table
#*********************************************************************************
wine_df <- rbind(wine_df, removed) 

nrow(wine_df)

#*********************************************************************************
# Set seed value to 1, create training and testing datasets from wine_df dataset.
# 90% for training, and 10% for testing.
#*********************************************************************************

set.seed(1, sample.kind="Rounding")

train_index <- createDataPartition(wine_df$rating, times = 1, p = 0.1, list = FALSE) 
train_set <- wine_df[-train_index,] 
temp_set <- wine_df[train_index,] 

#*********************************************************************************
# Make sure country and price in test_set are in train_set
#*********************************************************************************
test_set <- temp_set %>% 
  semi_join(train_set, by = "country") %>% 
  semi_join(train_set, by = "price") 

#*********************************************************************************
# Create dataframe removed to populate with data not available in test_set. 
# Determine the count of rows extracted in removed table
#*********************************************************************************
removed <- anti_join(temp_set, test_set) 
nrow(removed)

#*********************************************************************************
# Combine rows from removed dataframe, back into train_set; and determine the number 
# of rows in train_set dataset
#*********************************************************************************
train_set <- rbind(train_set, removed) 
nrow(train_set)

#*********************************************************************************
# Determine total number of rows in train and test set:
#*********************************************************************************
nrow(train_set) + nrow(test_set)

#*********************************************************************************
#                          Develop Models for Prediction
#*********************************************************************************
#*********************************************************************************
# 1st model: Simple Average Model - Baseline Model
#*********************************************************************************

# mu - average rating of all bottle of wines. 

mu <- mean(train_set$rating) 

model_1_rmse <- RMSE(test_set$rating, mu) 
model_1_rmse

#*********************************************************************************
# create dataframe(table), rmse_results to store the results of RMSE values. 
#*********************************************************************************

rmse_results <- data_frame(Model = "Simple Average", RMSE = model_1_rmse) 

rmse_results%>%knitr::kable()

#*********************************************************************************
# 2nd model: Country Effect Model
# Improve model by adding country effect parameter, b_c, that represents the 
# average rating for country: 
#*********************************************************************************

coun_avgs <- train_set %>% 
  group_by(countryId) %>% 
  summarize(b_c = mean(rating - mu)) 

coun_avgs %>% 
  qplot(b_c, geom ="histogram", bins = 10, data = ., color = I("blue"), 
        ylab = "Number of Countries", 
        main = "Number of wines with the computed b_c") 

predicted_ratings <- mu + test_set %>% 
  left_join(coun_avgs, by='countryId') %>% 
  .$b_c 

model_2_rmse <- RMSE(predicted_ratings, test_set$rating) 

rmse_results <- bind_rows(rmse_results, 
                          data_frame(Model="Country Effect Model", RMSE = model_2_rmse )) 

# Update the results table 
rmse_results %>%knitr::kable()

#*********************************************************************************
# 3rd model: Country and Price Effect Model
# Improve model by adding price effect (b_p): 
#
#*********************************************************************************

# Determine for each country average wine price, average rating, and count wines 
# produced in the aggregated dataset.   

wine_corr<- wine_review%>%
group_by(countryId) %>% 
summarize(Avg_Price = round(mean(price)), 
                 Mean_rating = round(mean(rating)),
                 Count = n())%>%
arrange(Avg_Price)%>%
print(n=43)

#************************************************************
# Plot average wine price vs country   
#************************************************************

wine_review%>%
group_by(country) %>%
summarize(price = round(mean(price))) %>%
ggplot(aes(country, price)) +
geom_point() +
ggtitle("Average Price vs Country") +
xlab("Country") +
ylab("Average Price") +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Different wines are rated differently even though the prices are the same. 
# For further insight to the dataset, we will compute the average price for 
# each country that have produced wines.

# Plot a histogram graph to see trend.

train_set %>% 
  group_by(price) %>% 
  summarize(b_p = mean(rating)) %>% 
  ggplot(aes(b_p)) + 
  geom_histogram(bins = 30, color = "blue") 

# The generated histogram above shows that the rating data are not normally 
# distributed. Let us see if country + price model will provide lower RMSE value. 

price_avgs <- train_set %>%
  left_join(coun_avgs, by='countryId') %>%
  group_by(price) %>% 
  summarize(b_p = mean(rating - mu - b_c)) 

predicted_ratings <- test_set %>% 
  left_join(coun_avgs, by='countryId') %>%
  left_join(price_avgs, by='price') %>% 
  mutate(pred = mu + b_c + b_p) %>% 
  .$pred 

model_3_rmse <- RMSE(predicted_ratings, test_set$rating) 

rmse_results <- bind_rows(rmse_results, 
      data_frame(Model="Country + Price Effect Model", RMSE = model_3_rmse )) 

# Update the results table 
rmse_results %>%
knitr::kable()

# we have achieved to lower the RMSE value. 

#*********************************************************************************
# 4th Model: Country + Price Effect Model 
# By tuning the model, let us determine lambda value that will minimize 
# RMSE value for country + price effect. 
#*********************************************************************************

lambdas <- seq(0, 5, 0.25)
model_4_rmses <- sapply(lambdas, function(lambdas){
  mu <- mean(train_set$rating)
  b_c <- train_set %>%
    group_by(countryId) %>%
    summarize(b_c = sum(rating - mu)/(n()+lambdas)) 
  b_p <- train_set %>% 
    left_join(b_c, by="countryId") %>%
    group_by(price) %>%
    summarize(b_p = sum(rating - b_c - mu)/(n()+lambdas))
  predicted_ratings <- test_set %>% 
    left_join(b_c, by = "countryId") %>%
    left_join(b_p, by = "price") %>%
    mutate(pred = mu + b_c + b_p) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, model_4_rmses)  


# The generated plot above shows the optimal lambda value using regularized 
# country + price effects.

# Using the minimum lambda value obtained determine RMSE value.

lambda <- lambdas[which.min(model_4_rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Country + Price Effect Model",  
                                     RMSE = min(model_4_rmses)))

# Update the results table 
rmse_results %>%  knitr::kable()

#*********************************************************************************
#
# Evaluating predictive model using wine_df (training dataset) 
# and validation (validation dataset)
#
#*********************************************************************************
#
# A model is best validated with independent large training datasets. Therefore,
# we will use wine_df dataset (train_set + test_set) to train the model, and use 
# the validation dataset to evaluate the prediction, and obtain RSME value. 
#
#*********************************************************************************
# 
# 5th Model: Evaluating country + price Effect Model 
#
# Since the country + price Effect Model produced a lower RSME value than the country 
# Effect Model, we will use derived minimum lambda value to compute the 
# regularized effect and achieve lower RSME result. 
#
#*********************************************************************************

mu_vali <- mean(wine_df$rating)

country_avgs_vali <- wine_df %>% 
  group_by(countryId) %>% 
  summarize(b_c = sum(rating - mu_vali)/(n()+lambda))

price_avgs_vali <- wine_df %>% 
  left_join(country_avgs_vali, by='countryId') %>%
  group_by(price) %>%
  summarize(b_p = sum(rating - b_c - mu_vali)/(n()+lambda))

predicted_vali <- validation %>% 
  left_join(country_avgs_vali, by='countryId') %>%
  left_join(price_avgs_vali, by='price') %>%
  mutate(pred = mu_vali + b_c + b_p) %>% 
  .$pred

validate_model <- RMSE(predicted_vali, validation$rating)

rmse_results <- bind_rows(rmse_results,
            data_frame(Model="Validating Regularized Country + Price Effect Model", 
                                     RMSE = validate_model))

# Update the results table 
rmse_results %>%
knitr::kable()

#*********************************************************************************
#
#                              Matrix Factorization
#
#*********************************************************************************

# Recosystem is an R wrapper of the LIBMF library developed by Yu-Chin Juan, Yong Zhuang, Wei-Sheng Chin and Chih-Jen Lin (http://www.csie.ntu.edu.tw/~cjlin/libmf/), an open source library for recommender system using matrix factorization. 

# LIBMF is a parallelized library, that users can take advantage of multicore CPUs to speed up the computation. It also utilizes some advanced CPU features to further improve the performance.


# Usage of recosystem

# The usage of recosystem is quite simple, mainly consisting of the following steps:
# 1.	Create a model object (a Reference Class object in R) by calling Reco().
# 2.	(Optionally) call the $tune() method to select best tuning parameters along a set of candidate values.
# 3.	Train the model by calling the $train() method. A number of parameters can be set inside the function, possibly coming from the result of $tune().
# 4.	(Optionally) output the model, i.e. write the factorized P and Q matrices info files.
# 5.	Use the $predict() method to compute predictions and write results into a file.


#*********************************************************************************
#
# Set seed to 1, 
# and assign training dataset as train_mf, and testing dataset as test_mf
#
#*********************************************************************************

set.seed(1, sample.kind = "Rounding") 

# Convert the train and test sets into recosystem input format
train_mf <-with(train_set, data_memory(user_index = wineId, 
                item_index = price, rating = rating))
test_mf <-with(test_set, data_memory(user_index = wineId, 
                item_index = price, rating = rating))


# Create the model object
r <-  recosystem::Reco()

# Tune model, Select the best tuning parameters
opts <- r$tune(train_mf, opts = list(dim = c(10, 20, 30), 
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = 4, niter = 10))

 # Train the algorithm  
r$train(train_mf, opts = c(opts$min, nthread = 4, niter = 20))


# Calculate the predicted values  
pr_reco <-  r$predict(test_mf, out_memory())

rmse_results <- bind_rows(rmse_results, 
                  data_frame(Model = "Matrix Factorization", 
                          RMSE = RMSE(test_set$rating, pr_reco)))


# Update the results table 
rmse_results %>%
knitr::kable()


#*********************************************************************************
#
#                              Matrix Factorization - Validation
#
#*********************************************************************************

set.seed(1, sample.kind = "Rounding")

# Convert 'wine_df' and 'validation' sets to recosystem input format
wine_reco <-  with(wine_df, 
              data_memory(user_index = wineId, item_index = price, 
                          rating = rating))
validation_reco <-  with(validation, data_memory(user_index = wineId, 
                        item_index = price, rating = rating))


# Create the model object
r_vali <-  recosystem::Reco()

# Tune the parameters
opts_vali <-r_vali$tune(wine_reco, opts = list(dim = c(10, 20, 30), 
                                     lrate = c(0.1, 0.2),
                                     costp_l2 = c(0.01, 0.1), 
                                     costq_l2 = c(0.01, 0.1),
                                     nthread  = 4, niter = 10))

# Train the model
r_vali$train(wine_reco, opts = c(opts_vali$min, nthread = 4, niter = 20))

# Calculate the prediction
pr_reco_vali<-r_vali$predict(validation_reco, out_memory())

# Update the result table
rmse_results <- bind_rows(rmse_results, 
                data_frame(Model = "Validating Matrix Factorization", 
                      RMSE = RMSE(validation$rating, pr_reco_vali)))

#*********************************************************************************
# Results
#*********************************************************************************

rmse_results %>%knitr::kable()


