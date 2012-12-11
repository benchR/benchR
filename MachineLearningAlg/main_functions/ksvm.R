library(kernlab)
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
  mod <- ksvm(y ~ ., data = problem[train, ])
  predicted <- predict(mod, problem[!train, ], type="response")
  mcrs[repl] <- mean(problem$y[!train] == predicted)
}
message(round(mean(mcrs), 4))
