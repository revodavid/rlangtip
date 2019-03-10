z_score <- function(x) {
  (x - mean(x)) / stats::sd(x)
}
