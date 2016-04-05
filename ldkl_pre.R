library(magrittr)

remove_singularity <- function(input, pca, threshold=2.0E-1) {
  valid_features <- pca$sdev > threshold
  rotated <- sweep(input, 2, pca$center) %*% pca$rotation
  # without scaling
  return(rotated[, valid_features])
  # with scaling
  # return(rotated[, valid_features] %*% solve(diag(pca$sdev[valid_features])))
}

train <- read.csv("/Users/ryomiyajima/predicting_a_biological_response/train.csv", header = T)

# for hold-out validation
buff <- train
train <- train[1:2500,]
test <- buff[2501:3751,]

train.input <- data.matrix(train[, 2:1777])
test.input <- data.matrix(test[, 2:1777])
  
train.pca <- prcomp(train.input)

train.white <- remove_singularity(train.input, train.pca)
test.white <- remove_singularity(test.input, train.pca)

# raw
write.table(
  cbind(train$Activity * 2 -1, train.input),
    file="train.csv", row.names = F, col.names = F)
  
write.table(
  cbind(test$Activity * 2 -1, test.input),
  file="test.csv", row.names = F, col.names = F)

# white
write.table(
  cbind(train$Activity * 2 -1, train.white),
    file="train.csv", row.names = F, col.names = F)
  
write.table(
  cbind(test$Activity * 2 -1, test.white),
  file="test.csv", row.names = F, col.names = F)
  
train.scaled <- train.input * 2 - 1
test.scaled <- test.input * 2 - 1
write.table(
  cbind(train$Activity * 2 -1, train.scaled),
  file="bio_scaled.train", row.names = F, col.names = F)
  
write.table(
  cbind(test$Activity * 2 -1, test.scaled),
  file="bio_scaled.test", row.names = F, col.names = F)
  
## pca -> kmeans clustering
num_clusters <- 2
num_pcs <- 3
train.white.kmeans <- kmeans(train.white[, 1:num_pcs], num_clusters)
# train
for (cluster_index in seq(num_clusters)) {
  valid_rows <- train.white.kmeans$cluster == cluster_index
  activity <- train$Activity[valid_rows]
  write.table(
    cbind(activity * 2 -1, train.white[valid_rows, ]),
    file=paste(as.character(cluster_index), ".train", sep=""), row.names = F, col.names = F)
}

#  test
library(dplyr)
library(tidyr)

test.clusters <- test.white[, 1:num_pcs] %>% data.frame %>% tbl_df
test.clusters <- test.clusters %>% mutate(data_index=as.integer(rownames(test.clusters)))
distance_to_clusters <- data.frame(distance=double(), cluster_index=integer(), data_index=integer(), stringsAsFactors=F) %>% tbl_df
for (cluster_index in seq(num_clusters)) {
  distances <- sweep(test.white[, 1:3], 2, train.white.kmeans$centers[cluster_index,]) %>% data.frame %>% tbl_df %>%
    mutate_each(funs(.^2)) %>%
    transmute(distance=rowSums(.))
  distances <- distances %>% mutate(cluster_index=cluster_index, data_index=test.clusters$data_index)
  distance_to_clusters <- union(distance_to_clusters, distances)
}
clusters <- distance_to_clusters %>%
  group_by(data_index) %>%
  arrange(cluster_index, data_index) %>%
  mutate(closest_cluster=which.min(distance)) %>%
  filter(cluster_index == closest_cluster) %>%
  select(data_index, closest_cluster)

for (cluster_index in seq(num_clusters)) {
  valid_rows <- clusters$closest_cluster == cluster_index
  activity <- test$Activity[valid_rows]
  write.table(
    cbind(activity * 2 -1, test.white[valid_rows, ]),
    file=paste(as.character(cluster_index), ".test", sep=""), row.names = F, col.names = F)
}
