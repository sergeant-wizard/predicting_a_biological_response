library(magrittr)
library(xgboost)
library(caret)
source("/Users/ryomiyajima/repos/predicting_a_biological_response/cross_validation.R")

train <- read.csv("/Users/ryomiyajima/repos/predicting_a_biological_response/train.csv", header = T)
train[, 1] <- factor(train[, 1])
test <- read.csv("/Users/ryomiyajima/repos/predicting_a_biological_response/test.csv", header = T)

num_bootstrap_samples <- 2
num_folds <- 4
folds <- create_folds(num_folds)
bootstrap_samples <- createResample(train[, 1], times=num_bootstrap_samples)

cross_validation_results <- data.frame(predicted=double(), fold=character(), sample=character())
for (sample_name in names(bootstrap_samples)) {
  sample <- bootstrap_samples[[sample_name]]
  input <- train[sample, -1] %>% data.matrix
  label <- train[sample, 1] %>% as.integer - 1
  fit <- xgboost(data=input, label=label, nrounds=15, params=list(objective="binary:logistic"))
  predictor <- function(fit, data) {
    return(predict(fit, data %>% data.matrix))
  }
  cross_validation_results <- bind_rows(
    cross_validation_results,
    cross_validate(fit, predictor, train, folds) %>% mutate(sample=sample_name))
}

weight <- c(0.1, 0.9)
names(weight) <- c("Resample1", "Resample2")
cross_validation_results %>% mutate(weight=weight[sample]) %>% group_by(fold, sample) %>% summarise(predicted*weight)
