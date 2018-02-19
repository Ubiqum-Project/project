data = read.csv(gzfile("finalRate.csv.gz"))



# Split into training, validation and test sets
train_tbl <- data[1:1500]
valid_tbl <-data[1501:2000]
test_tbl  <- data[2001:nrow(data)]

h2o.init()        # Fire up h2o

h2o.no_progress() # Turn off progress bars

train_h2o <- as.h2o(train_tbl)
valid_h2o <- as.h2o(valid_tbl)
test_h2o  <- as.h2o(test_tbl)

# Set names for h2o
y <- "AveragedExchange"
x <- setdiff(names(train_h2o), y)

# linear regression model used, but can use any model
automl_models_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o, 
  leaderboard_frame = test_h2o, 
  max_runtime_secs = 60, 
  stopping_metric = "deviance")

# Extract leader model
automl_leader <- automl_models_h2o@leader

pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

h2o.performance(automl_leader, newdata = test_h2o)
# 
# # Investigate test error
# error_tbl <- data %>% 
#   filter(lubridate::year(date) == 2017) %>%
#   add_column(pred = pred_h2o %>% as.tibble() %>% pull(predict)) %>%
#   rename(actual = price) %>%
#   mutate(
#     error     = actual - pred,
#     error_pct = error / actual
#   ) 
# error_tbl
# 
# error_tbl %>%
#   summarise(
#     me   = mean(error),
#     rmse = mean(error^2)^0.5,
#     mae  = mean(abs(error)),
#     mape = mean(abs(error_pct)),
#     mpe  = mean(error_pct)
#   ) %>%
#   glimpse()
# 
