generateProblem <- function(task, n, q, r, n.levels, NAs) {
  task <- match.arg(as.character(task), c("regression", "classification"))
  n <- as.integer(n) # number of samples
  q <- as.integer(q) # number of numeric variables
  r <- as.integer(r) # number of factor variables
  n.levels <- as.integer(n.levels) # number of levels for each factor variable
  NAs <- as.double(NAs) # fraction of NAs in the variables
  p <- q+r

  stopifnot(n > 0)
  stopifnot(p > 0)


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
    repeat {
      tmp <- as.data.frame(replicate(r, sample(letters[seq_len(n.levels)], n, replace=TRUE)))
      if (all(sapply(tmp, function(x) length(levels(x))) == n.levels))
        break
    }

    X <- cbind(X, tmp)
  }

  # add NAs
  if (!isTRUE(all.equal(NAs, 0))) {
    NAs <- floor(n * p * NAs)
    count <- 0
    while (count < NAs) {
      i <- sample(1:n, 1)
      j <- sample(1:p, 1)
      if (!is.na(X[i, j])) {
        X[i, j] <- NA
        count <- count + 1
      }
    }
  }

  # combine all into one data frame and set col names
  X <- cbind(y, X)
  colnames(X) <- c("y", sprintf("q%03i", seq_len(q)), sprintf("r%03i", seq_len(r)))

  X
}


# create design
design <- expand.grid(n = c(100, 1000),
                      q = c(5, 20),
                      r = c(0, 5, 20),
                      n.levels = c(2, 5),
                      NAs = c(0, 0.3))

# remove redundant/duplicant/ineffictive rows
desgin = subset(design, !(r == 0 & n.levels != 2))

# generate problems, save to disk
for (i in seq_len(nrow(design))) {
  set.seed(i)
  for (task in c("classification", "regression")) {
    problem = do.call("generateProblem", args = c(list(task = task), as.list(design[i, ])))
    fn = file.path("problems", sprintf("%s_%02i.RData", task, i))
    save(problem, file = fn)
  }
}

