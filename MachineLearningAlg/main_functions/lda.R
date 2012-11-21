library(MASS)
type <- "classification"

args <- commandArgs(TRUE)
if (length(args)) {
  num <- as.integer(args[1])
  repls <- as.integer(args[2])
}

load(file.path("problems", sprintf("%s_%02i.RData", type, num)))

for (repl in seq_len(repls)) {
  mod <- lda(y ~ ., data = problem)
}