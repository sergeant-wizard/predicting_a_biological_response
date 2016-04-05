library(magrittr)
library(xgboost)
source("/Users/ryomiyajima/predicting_a_biological_response/log_loss.R")

train <- read.csv("/Users/ryomiyajima/predicting_a_biological_response/train.csv", header = T)
test <- read.csv("/Users/ryomiyajima/predicting_a_biological_response/test.csv", header = T)

# for hold-out validation
buff <- train
train <- train[1:2500,]
test <- buff[2501:3751,]

fit <- xgboost(data=train.input, label=train$Activity, nrounds=15, params=list(objective="binary:logistic"))
first_col <- predict(fit, test.input)
result <- cbind(1-first_col, first_col)
log_loss(result, test$Activity)
