library(magrittr)
library(rpart)
source("/Users/ryomiyajima/predicting_a_biological_response/log_loss.R")

train <- read.csv("/Users/ryomiyajima/predicting_a_biological_response/train.csv", header = T)

# for hold-out validation
buff <- train
train <- train[1:2500,]
test <- buff[2501:3751,]

fit <- rpart(Activity ~ ., data=train, method="class", control=rpart.control(cp=0.0025))
fit <- prune(fit, cp=0.0025)

log_loss(predict(fit, test), test$Activity)
# 0.614

library(dplyr)
library(reshape2)
library(tidyr)
predicted <- predict(fit, test) %>% data.frame %>% tbl_df %>%
  mutate(id=as.integer(rownames(.))) %>%
  gather("key", "value", -id) %>%
  group_by(id) %>%
  top_n(1, value) %>%
  arrange(id) %>%
  mutate(Activity=if(key=="X0") 0 else 1) %>% select(id, Activity)

# precision
((predicted$Activity == test$Activity) %>% sum) / length(predicted$Activity)
