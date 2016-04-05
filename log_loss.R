library(magrittr)

log_loss <- function(result, answer) {
  eps <- 1E-4
  result <- cbind(result, answer)
  return(apply(result, 1, function(x) {
    if (x[3] == FALSE) {
      ret = x[1]
    } else {
      ret = x[2]
    }
    return(max(ret, eps))
  }) %>% log %>% mean * -1)
}
