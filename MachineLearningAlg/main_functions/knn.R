# ------------------------------------------------------------------
# Contributed by Michel Lang, TU Dortmund
# ------------------------------------------------------------------
# K-nearest-neighbor classification using the class package with default parameters
# USEAGE: Rscript [scriptfile] [problem-number] [number of replications]
# Output: Misclassification rate
# FIXME -DOES NOT WORK WITH FACTORS-
library(class)
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
  predicted <- knn(train = subset(problem, train, select=-y),
                   test = subset(problem, !train, select=-y),
                   problem$y[train])
  mcrs[repl] <- mean(problem$y[!train] == predicted)
}
message(round(mean(mcrs), 4))
