library(magrittr)
library(randomForest)
source("/Users/ryomiyajima/predicting_a_biological_response/log_loss.R")

remove_singularity <- function(input, pca, threshold=1.0E-2) {
  valid_features <- pca$sdev > threshold
  rotated <- input %*% pca$rotation
  return(rotated[, valid_features])
}

train <- read.csv("/Users/ryomiyajima/predicting_a_biological_response/train.csv", header = T)

# for hold-out validation
buff <- train
train <- train[1:2500,]
test <- buff[2501:3751,]
train.input <- data.matrix(train[, 2:1777])
test.input <- data.matrix(test[, 2:1777])
  
train.pca <- prcomp(train.input)

fit <- randomForest(remove_singularity(train.input, train.pca), factor(train$Activity))
log_loss(
  predict(
    fit,
    remove_singularity(test.input, train.pca, pca_sdev_threshold),
    type="prob"),
  test$Activity)
# 0.62

# for submitting
real_test <- read.csv("/Users/ryomiyajima/predicting_a_biological_response/test.csv", header = T)
real_test <- data.matrix(real_test)
real_test.predicted <- predict(
  fit,
  remove_singularity(real_test, train.pca, pca_sdev_threshold),
  type="prob")

write.table(
  cbind(rownames(real_test.predicted) %>% as.integer, real_test.predicted[,1]), 
  file="random_forest_predict.csv", row.names=F, sep=",")
