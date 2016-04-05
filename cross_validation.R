library(caret)
library(magrittr)
library(dplyr)

create_folds <- function(num_folds) {
  return(createFolds(data[, 1], k=num_folds))
}

cross_validate <- function(fit, predictor, data, folds) {
  ret <- data.frame(fold=character(), predicted=double())
  for (fold in names(folds)) {
    sample <- folds[[fold]]
    train <- data[-sample,]
    test <- data[sample,]
    predicted <- predictor(fit, test[, -1])
    ret <- bind_rows(ret, data.frame(predicted=predicted, fold=fold, stringsAsFactors=F))
  }
  return(ret)
}
