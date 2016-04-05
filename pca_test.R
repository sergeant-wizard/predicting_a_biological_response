rotate_matrix <- function(angle) {
  return(matrix(
    c(cos(angle), sin(angle), -sin(angle), cos(angle)),
    nrow = 2, ncol = 2))
}

n = 1024
r <- runif(n, min = 0, max = 1)
theta <- runif(n, min = -pi, max = pi)

data <- cbind(4 * r * sin(theta), r * cos(theta))
data <- data %*% rotate_matrix(pi / 6) + cbind(rep(10, n), rep(20, n))

###

data.pca <- prcomp(data)
data.normalized <- sweep(data, 2, data.pca$center) %*% data.pca$rotation %*% solve(diag(data.pca$sdev))
plot(data.normalized)
