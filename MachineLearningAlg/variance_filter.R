### Exemplary variance filter on a big matrix
# Sometimes used in gene expression analysis to pre-filter the
# number of covariates. Only top N covariates are kept for further
# analysis. This has to be done on the training set only, otherwise
# biased results are probable.

# change these numbers to get suitable runtime
n <- 300    # number of observations
p <- 20000  # number of covariates
N <- 100    # number of replications

X <- matrix(replicate(p, rnorm(n, sd = runif(1, 0.1, 10))), n, p)
colnames(X) <- sprintf("var_%05i", 1:p)

####################################################################################
### benchmark code below
####################################################################################


for (i in 1:N) {
  train <- sample(nrow(X), 2 / 3 * nrow(X))
  colVars <- apply(X[train, ], 2, var)
  keep <- names(head(sort(colVars, decreasing = TRUE), 100))
  # myAlgorithm(X[, keep])
}
