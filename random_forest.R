library(magrittr)
library(randomForest)
source("/Users/ryomiyajima/predicting_a_biological_response/log_loss.R")

train <- read.csv("/Users/ryomiyajima/predicting_a_biological_response/train.csv", header = T)
test <- read.csv("/Users/ryomiyajima/predicting_a_biological_response/test.csv", header = T)

# for hold-out validation
buff <- train
train <- train[1:2500,]
test <- buff[2501:3751,]

fit <- randomForest(factor(Activity) ~ ., data=train, ntree=500)
result <- predict(fit, test, type="prob")
log_loss(result, test$Activity)
# 0.463

# for submission
predicted <- predict(fit, test, type="prob")
write("MoleculeId,PredictedProbability", "out.csv")

write.table(
  cbind(rownames(predicted) %>% as.integer, predicted[,2]), 
  file="out.csv", row.names=F, sep=",", append=T)
