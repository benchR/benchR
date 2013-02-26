# ------------------------------------------------------------------
# Contributed by Michel Lang, TU Dortmund
# ------------------------------------------------------------------
# L1-penalized linear regression using the penalized package with default parameters
# USEAGE: Rscript [scriptfile] [problem-number] [number of replications]
# Output: unadjusted R^2
library(penalized)
type <- "regression"

args <- commandArgs(TRUE)
if (length(args)) {
  num <- as.integer(args[1])
  repls <- as.integer(args[2])
}

load(file.path("problems", sprintf("%s_%02i.RData", type, num)))

R2 <- numeric(repls)
for (repl in seq_len(repls)) {
  set.seed(repl)
  train <- sample(nrow(problem)) < floor(2/3 * nrow(problem))
  mod <- optL1(problem$y[train], penalized = as.matrix(subset(problem, train, select=-y)), model="linear")
  y <- problem[!train, "y"]
  y.hat <- predict(mod$fullfit, as.matrix(subset(problem, !train, select=-y)))[, 1]
  R2[repl] <- 1 - sum((y - y.hat)^2) / sum((y - mean(y))^2)
}
message(round(mean(R2), 4))
