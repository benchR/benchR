# ------------------------------------------------------------------
# Contributed by Michel Lang, TU Dortmund
# ------------------------------------------------------------------
# Logistic regression (as generalized linear model) using the stats package
# USEAGE: Rscript [scriptfile] [problem-number] [number of replications]
# Output: Misclassification rate
library(stats)
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
  mod <- glm(y ~ ., data = problem[train, ], family=binomial())
  predicted = c("a", "b")[(predict(mod, problem[!train, ], type = "response") > 0.5) + 1]
  mcrs[repl] <- mean(problem$y[!train] == predicted)
}
message(round(mean(mcrs), 4))
