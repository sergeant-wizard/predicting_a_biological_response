library(caret)

fit_generator <- function(data) {
  label <- data[, 1] %>% as.integer - 1 # factor to integer
  input <- data[, -1] %>% data.matrix # data.frame to matrix
  return(xgboost(data=input, label=label, nrounds=15, params=list(objective="binary:logistic")))
}

predictor <- function(fit, data) {
  return(predict(fit, data %>% data.matrix))
}

raw <- read.csv("/Users/ryomiyajima/predicting_a_biological_response/train.csv", header = T)
raw[, 1] <- factor(raw[, 1])
num_folds <- 4
folds <- createFolds(raw[, 1], k=num_folds)
results <- sapply(folds, function(sample) {
  train <- raw[-sample,]
  test <- raw[sample,]
  fit <- fit_generator(train)
  predicted <- predictor(fit, test[, 1])
  log_loss(cbind(1-predicted, predicted), test[, 1] %>% as.integer - 1)
})
