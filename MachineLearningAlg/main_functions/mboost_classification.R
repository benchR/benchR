# ------------------------------------------------------------------
# Contributed by Michel Lang, TU Dortmund
# ------------------------------------------------------------------
# Gradient based boosted classification using the mboost package with default parameters
# USEAGE: Rscript [scriptfile] [problem-number] [number of replications]
# Output: Misclassification rate
library(mboost)
type <- "classification"

args <- commandArgs(TRUE)
if (length(args)) {
  num <- as.integer(args[1])
  repls <- as.integer(args[2])
}

load(file.path("problems", sprintf("%s_%02i.RData", type, num)))

mcrs <- numeric(repls)
for (repl in seq_len(repls)) {
  set.seed(repl)
  train <- sample(nrow(problem)) < floor(2/3 * nrow(problem))
  mod <- mboost(y ~ ., data = problem[train, ], family=Binomial())
  predicted = predict(mod, problem[!train, ], type = "class")
  mcrs[repl] <- mean(problem$y[!train] == predicted)
}
message(round(mean(mcrs), 4))
