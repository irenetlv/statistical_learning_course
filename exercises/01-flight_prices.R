# Exercise 01 - flight price data ----
# The data to this exercise is proprietary, hence available only to specific individuals.
# To the regular exercise, please refer to a differnt exercise script.

# Read flight data ----

library(tidyverse)
library(riskir)
library(lubridate)

flights <- read_csv_from_s3(
  bucket = "riskified-research-files",
  filepath = "statistical-learning-course/flight_prices.csv",
  stringsAsFactors = FALSE)

# glimpse
glimpse(flights)

# Train/test split ----
set.seed(0)
flights <- flights %>% 
  as_tibble() %>% 
  mutate_at(vars(is_roundtrip, domestic_trip), as.logical) %>% 
  mutate(is_train = runif(NROW(order_id)) < 0.8)

# This is an advance exercise, in the sense that you are free to "roam in the wild" as long as you are 
# handling the problems with the methods we have shown in class (or otherwise closely related methods).

# The exercise:
# We would like to predict the **order_total_spent** as best we can, using any of the tools we have discussed so far.
# That is, you can use feature selection (e.g., stepwise), feature importance, transformation of variables anyway you like,
# for example: factoring, log, sqrt, or any other which comes to mind.
# preprocessing anyway you like, outlier removal, quantile regression, etc.

pop_score <- flights %>% 
  mutate(beginning_week = floor_date(ymd_hms(beginning_of_trip), "week")) %>% 
  count(beginning_week, name = "popularity_count") %>% 
  mutate(popularity_rank = min_rank(popularity_count)) %>% 
  select(beginning_week, popularity_rank, popularity_count)
  
flights_simple <- flights %>% 
  filter(!is.na(min_days_until_trip)) %>%
  mutate_at(vars(created_at, beginning_of_trip, end_of_trip), ymd_hms) %>% 
  mutate(beginning_week = floor_date(beginning_of_trip, "week")) %>% 
  left_join(pop_score, by = "beginning_week") %>% 
  mutate(min_days_until_trip = floor(min_days_until_trip/10)) %>% 
  mutate(sleep_time = between(local_created_hour, 3, 7),
         is_weekday = between(local_created_dow, 1, 5),
         beginning_month = month(beginning_of_trip),
         end_month = month(end_of_trip),
         days_from_flight = difftime(beginning_of_trip, created_at, units = "days"),
         visit_length_days = visit_length_hours / 24) 

flights_train <- flights_simple %>% 
  filter(is_train) 

# make local_created_dow and local_created_hour categorical

our_model <- lm(order_total_spent ~ num_of_passengers +
                  is_roundtrip + domestic_trip +
                  number_of_flights + sleep_time +
                  number_of_flights * num_of_passengers * max_connection_cnt +
                  popularity_count + min_days_until_trip,
                data = flights_train)

summary(our_model)

flights_simple %>% 
  mutate(predicted_price = predict(our_model, newdata = flights_simple)) %>% 
  mutate(predicted_price = ifelse(is.na(predicted_price), 
                                  0, predicted_price)) %>% 
  model_accuracy(order_total_spent, predicted_price)


# log:
# min_days_until_trip
# visit_length_hours

# If you know pca (which we will talk about later) or regularization methods (lasso, ridge) you can also use them.
# If you have additional relevant tools from the current domain (linear regression and generalization), you are welcome to use them.

# Comparing results ----

# function to compute test error rates, 
# in RMSE (root mean squared error) and MAPE (mean absolute percentage error)

# example 
# model_accuracy(flights, 
#                original_price = order_total_spent, 
#                predicted_price = predicted_price)

model_accuracy <- function(tbl, original_price, predicted_price){
  if (!(has_name(tbl, "is_train"))){
    warning("No is_train variable detected, assuming that the data set is entirely the test set.")
    tbl <- tbl %>% 
      mutate(is_train = F)
  }
  
  tbl %>% 
    mutate(rmse = ({{original_price}} - {{predicted_price}})^2,
           mape = abs({{original_price}} - {{predicted_price}})/{{original_price}},
           mae = abs({{original_price}} - {{predicted_price}})) %>% 
    group_by(is_train) %>% 
    summarize(rmse = sqrt(mean(rmse)),
              mae = mean(mae),
              mape = mean(mape)) %>% 
    select(is_train, rmse, mape, mae)
}

# An example with a fairly simplistic linear model
flights_lm <- lm(data = flights %>% filter(is_train),
                 formula = order_total_spent ~ 
                   local_created_hour + local_created_dow + num_of_passengers + 
                   min_days_until_trip + avg_price_per_passenger + visit_length_hours + max_connection_cnt +
                   is_roundtrip + domestic_trip + number_of_flights)

summary(flights_lm)

flights %>% 
  mutate(predicted_price = predict(flights_lm, newdata = flights)) %>% 
  mutate(predicted_price = ifelse(is.na(predicted_price), 0, predicted_price)) %>% 
  model_accuracy(order_total_spent, predicted_price)




lm(formula = order_total_spent ~ num_of_passengers + max_connection_cnt + 
    is_roundtrip + domestic_trip + number_of_flights + sleep_time, 
  data = flights_train)