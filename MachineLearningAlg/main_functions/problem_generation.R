# ------------------------------------------------------------------
# Contributed by Michel Lang, TU Dortmund
# ------------------------------------------------------------------
generateProblem <- function(task, n, q, r, n.levels, NAs) {
  task <- match.arg(as.character(task), c("regression", "classification"))
  n <- as.integer(n) # number of samples
  q <- as.integer(q) # number of numeric variables
  r <- as.integer(r) # number of factor variables
  n.levels <- as.integer(n.levels) # number of levels for each factor variable
  NAs <- as.integer(NAs) # number of incomplete cases
  p <- q+r

  stopifnot(n > 0)
  stopifnot(p > 0)
  stopifnot(NAs < n)


  # construct response
  if (task == "regression") {
    y <- rnorm(n)
  } else  {
    repeat {
      y <- factor(sample(c("a", "b"), n, replace = TRUE), levels = c("a", "b"))
      if (all(table(y) > 0))
        break
    }
  }

  # construct matrix of variables
  X <- as.data.frame(matrix(rnorm(q * n), ncol = q))
  if (r > 0) {
    stopifnot(n.levels > 0)
    stopifnot(n.levels < n)
    # ensure that all factor levels are present
    repeat {
      tmp <- as.data.frame(replicate(r, sample(letters[seq_len(n.levels)], n, replace=TRUE)))
      if (all(sapply(tmp, function(x) length(levels(x))) == n.levels))
        break
    }

    X <- cbind(X, tmp)
  }

  # add NAs
  if (NAs > 0) {
    for (i in sample(n, size = NAs))
      X[i, sample(p, size = sample(p, 1, prob = 1/(1:p)))] <- NA
  }

  # check again that all factor levels are still present
  if (r > 0 ) {
    i = sapply(X, is.factor)
    stopifnot(all(sapply(X[, i], function(x) all(table(x) > 0))))
  }

  # combine all into one data frame and set col names
  X <- cbind(y, X)
  colnames(X) <- c("y", sprintf("q%03i", seq_len(q)), sprintf("r%03i", seq_len(r)))

  X
}


# create design for usual tasks
design = rbind(expand.grid(n = c(100, 1000), q = c(5, 20), r = 0,        n.levels = 0,       NAs = c(0, 20)),
               expand.grid(n = c(100, 1000), q = c(5, 20), r = c(5, 20), n.levels = c(2, 5), NAs = c(0, 20)),
               list(n = 5000,  q = 120, r = 0,  n.levels = 0, NAs = 0),
               list(n = 5000,  q = 100, r = 20, n.levels = 5, NAs = 0),
               list(n = 5000,  q = 120, r = 0,  n.levels = 0, NAs = 500),
               list(n = 5000,  q = 100, r = 20, n.levels = 5, NAs = 500),
               list(n = 10000, q = 120, r = 0,  n.levels = 0, NAs = 0),
               list(n = 10000, q = 100, r = 20, n.levels = 5, NAs = 0),
               list(n = 10000, q = 120, r = 0,  n.levels = 0, NAs = 1000),
               list(n = 10000, q = 100, r = 20, n.levels = 5, NAs = 1000)
)

design = as.data.frame(sapply(design, as.integer))
save(design, file = "design.RData")

# generate problems, save to disk
for (i in seq_len(nrow(design))) {
  message("Generating problems for row ", i)
  set.seed(i)
  for (task in c("classification", "regression")) {
    problem = do.call("generateProblem", args = c(list(task = task), as.list(design[i, ])))
    fn = file.path("problems", sprintf("%s_%02i.RData", task, i))
    save(problem, file = fn)
  }
}
