# This project is based on: https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection

library(dplyr)
library(data.table)

library(xgboost)

#Loading datasets
train <- read.csv('train_sample.csv')
test <- read.csv('test.csv')

# Checking datasets
head(train)
head(test)

# Creating the dataset we are going to use for predictions
sub <- data.table(click_id = test$click_id, is_attributed = NA) 

# Removing click_id, because it won't help our model
test['click_id'] <- NULL

# Checking how many clicks converted into downloads
train %>% group_by(is_attributed) %>% summarise(n = n())

# Counting how many times each IP has clicked, because it can indicate a fraud
clicks_by_ip <- train %>% group_by(ip) %>% count()
clicks_by_ip_test <- test %>% group_by(ip) %>% count()

# Naming the collumns and merging this information with our originals datasets
colnames(clicks_by_ip) <- c("ip", "clicks_by_ip")
colnames(clicks_by_ip_test) <- c("ip", "clicks_by_ip")
train <- merge(train, clicks_by_ip, by.x = "ip", by.y = "ip")
test <- merge(test, clicks_by_ip_test, by.x = "ip", by.y = "ip")

# Removing these data from memory
clicks_by_ip_test <- NULL
clicks_by_ip <- NULL

# IP addresss won't be relevant anymore
train["ip"] <- NULL
test["ip"] <- NULL

# Converting dates
train.date <- as.POSIXlt(strptime(train[["click_time"]], format='%Y-%m-%d  %H:%M:%S'))
test.date <- as.POSIXlt(strptime(test[["click_time"]], format='%Y-%m-%d  %H:%M:%S'))

# Extracting info from dates and removing the original column
train["wday"] <- train.date$wday
train["hour"] <- train.date$hour
train["mday"] <- train.date$mday
train["click_time"] <- NULL
train["attributed_time"] <- NULL

test["wday"] <- test.date$wday
test["hour"] <- test.date$hour
test["mday"] <- test.date$mday
test["click_time"] <- NULL

# Removing these variables from memory
test.date <- NULL
train.date <- NULL

# Checking how many clicks we have by week day
train %>% group_by(wday) %>% count()

# Checking how many clicks we have by hour
train %>% group_by(hour) %>% count()

# Checking how many clicks we have by month day
train %>% group_by(mday) %>% count()

# Checking variables
str(train)

# Organizing variables
features <- c("app", "device", "os", "channel", "clicks_by_ip", "wday", "hour", "mday", "is_attributed")
features_test <- c("app", "device", "os", "channel", "clicks_by_ip", "wday", "hour", "mday")

train <- train[features]
test <- test[features_test]

# Our target
y <- train['is_attributed']

# Creating a validation dataset
n_train <- nrow(train)

train_index <- sample(c(1:n_train),0.9*n_train, replace = FALSE) 

train2 <- train[train_index,]
y1 <- y[train_index,]

train_val <- train[-train_index,]
y2 <- y[-train_index,]

# Preparing the datasets for the model xgboost

model_train <- xgb.DMatrix(data = data.matrix(train2[,1:8]), label = y1)

model_val <- xgb.DMatrix(data = data.matrix(train_val[,1:8]), label = y2)

xgb_test <- xgb.DMatrix(data = data.matrix(test[,1:8]))

# Setting the parameters https://xgboost.readthedocs.io/en/latest/parameter.html
# eta: control the learning rate. Smaller the eta, higher the accuracy, but it affects the performance
# subsample and colsample_bytree: proportion of observations and proportion of columns to be randomly sampled
# scale_pos_weight: Control the balance of positive and negative weights, useful for unbalanced classes. A typical value to consider: sum(negative instances) / sum(positive instances)
params <- list(objective = "binary:logistic", 
               booster = "gbtree",
               eval_metric = "auc",
               max_depth = 10,
               eta = 0.05,
               subsample = 0.7,
               colsample_bytree = 0.7,
               gamma = 0.9,
               nrounds = 100,
               scale_pos_weight= 439)

# Creating the model
myxgb_model <- xgb.train(params, model_train, params$nrounds, list(val = model_val), early_stopping_rounds = 50)

# Checking the importance of each variable for this model
imp <- xgb.importance(colnames(model_train), model=myxgb_model)
xgb.plot.importance(imp)

# Making predictions
predicted_xgb <- predict(myxgb_model, xgb_test)

# Cleaning the memory
test <- NULL
train <- NULL

# Adding the predictions to the dataset we created at the start
sub[, is_attributed := predicted_xgb]

# Creating the file with the predictions
fwrite(sub, "predictions_xgb.csv")
