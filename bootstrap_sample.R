library(magrittr)
library(xgboost)
library(caret)
source("/Users/ryomiyajima/repos/predicting_a_biological_response/cross_validation.R")

train <- read.csv("/Users/ryomiyajima/repos/predicting_a_biological_response/train.csv", header = T)
train[, 1] <- factor(train[, 1])
test <- read.csv("/Users/ryomiyajima/repos/predicting_a_biological_response/test.csv", header = T)

num_bootstrap_samples <- 50
num_folds <- 4
folds <- create_folds(num_folds)
bootstrap_samples <- createResample(train[, 1], times=num_bootstrap_samples)

cross_validation_results <- data.frame()
test_prediction <- data.frame()
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
  test_prediction <- bind_rows(
    test_prediction,
    data.frame(
      predicted=predictor(fit, test),
      sample=sample_name,
      data_id=as.integer(rownames(test)),
      stringsAsFactors=F))
}

# TODO: we want to optimize this weight
weight <- rep(1 / num_bootstrap_samples, num_bootstrap_samples)
names(weight) <- cross_validation_results$sample %>% unique
eps <- 1.0E-4
cross_validation_results %>%
  mutate(weight=weight[sample]) %>%
  group_by(data_id, fold) %>%
  summarise(prob=sum(predicted*weight)) %>%
  mutate(activity=train$Activity[data_id]) %>%
  mutate(log_arg = if (activity == 1) max(eps, prob) else max(eps, 1-prob)) %>%
  mutate(log=log(log_arg)) %>%
  group_by(fold) %>%
  summarise(log_loss=-mean(log)) %>%
  summarise(mean(log_loss))
  
# aggregate test prediction based on optimized weight
submitted_data <- test_prediction %>%
  mutate(weight=weight[sample]) %>%
  group_by(data_id) %>% 
  summarise(prob=sum(predicted*weight))

write.table(
  submitted_data,
  file="out.csv", row.names=F, sep=",")
