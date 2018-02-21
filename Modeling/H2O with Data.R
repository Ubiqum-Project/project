library(h2o)
data = read.csv(gzfile("finalRate.csv.gz"))
data = read.csv(gzfile("finalRate.csv.gz"))
data = data[ , apply(data, 2, function(x) !any(is.na(x)))]
data=data[,3:ncol(data)]
data$AveragedExchange = as.numeric(as.character(data$AveragedExchange))

test = data
test$bin = as.numeric(cut2(data$AveragedExchange, g=20))

data$AveragedExchange = as.numeric(cut2(data$AveragedExchange, g=20))


# Split into training, validation and test sets
train_tbl <- data[1:1501,]
valid_tbl <-data[1701:nrow(data),]
test_tbl  <- data[1:nrow(data),]

h2o.init()        # Fire up h2o

#h2o.no_progress() # Turn off progress bars

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
  max_runtime_secs = 180, 
  stopping_metric = "deviance")

# Extract leader model
automl_leader <- automl_models_h2o@leader

pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

h2o.performance(automl_leader, newdata = test_h2o)

variableImportance = as.data.frame(h2o.varimp(automl_leader))

h2o.r2(automl_leader, train = FALSE, valid = T, xval = F)


write.csv(variableImportance, "variableImportance.csv")
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
